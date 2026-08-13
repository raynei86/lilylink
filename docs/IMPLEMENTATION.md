# Lilylink implementation status

This document is the authoritative reference for what the Lilylink converter
currently supports and what it does not. It is a living document: update it
whenever parser or emitter behavior changes.

The upstream specification is `docs/notation.pdf` (the LilyPond Notation
Reference, 2.25.81).

## Overview

Lilylink converts a subset of LilyPond input (`.ly`) into MusicXML
(`score-partwise`). It is a standalone Common Lisp library with no runtime
dependency on LilyPond; parsing is done with a hand-written tokenizer and
recursive-descent parser.

Pipeline:

```
convert-string / convert-file
  -> tokenize      (src/lexer.lisp)    source string -> tokens
  -> parse-music   (src/parser.lisp)   tokens -> events (relative octaves resolved)
  -> build-score   (src/measure.lisp)  events -> score (measures, attributes, divisions)
  -> emit-score    (src/musicxml.lisp) score  -> MusicXML string
```

## Architecture and data model

| File | Responsibility |
| --- | --- |
| `src/main.lisp` | Package definition, public API, `lilylink-parse-error` condition |
| `src/model.lisp` | CLOS classes for the intermediate representation |
| `src/duration.lisp` | Duration arithmetic in division units (decomposition, measure chunks) |
| `src/lexer.lisp` | Hand-written tokenizer; emits `token` structs |
| `src/parser.lisp` | Recursive-descent parser; resolves `\relative` octaves and ties |
| `src/measure.lisp` | Lays events into measures, auto-splitting at barlines |
| `src/musicxml.lisp` | Emits `score-partwise` XML with escaping |

### Model classes (`src/model.lisp`)

- `pitch` — `step` (0–6, c=0 … b=6), `alter` (semitones: +1 sharp, −1 flat),
  `octave` (scientific; middle C = 4).
- `duration` — `log` (log2 of the reciprocal; 0 = whole, 2 = quarter),
  `dots`.
- `note` — a `pitch` + `duration`, plus `tie-start`/`tie-stop` flags (a note
  can both end and begin a tie).
- `rest-event` — a `duration`.
- `chord` — a list of `note`s sharing one `duration`.
- `measure` — `number`, `events`, and an attribute snapshot (`attributes`,
  `attr-data`) captured when the measure is opened or a mid-measure command
  appears in an empty measure.
- `staff` — current clef / key / time state, `divisions`, and the list of
  `measure`s.
- `score` — a list of `staff`s (currently always one).

`\time`, `\key`, `\clef`, and bar checks are first-class events too:
`time-change` (beats/beat-type), `key-change` (pitch/mode), `clef-change`
(clef/octave-shift), and `barline`. `build-score` dispatches over them with a
single `etypecase`.

Dependency: `alexandria` (used for `when-let`/`if-let` in the parser); there is
no runtime dependency on LilyPond.

### Public API (`src/main.lisp`)

- `lilylink:convert-string` `(source)` → MusicXML string.
- `lilylink:convert-file` `(path)` → MusicXML string (reads UTF-8 via uiop).
- `lilylink:tokenize` / `lilylink:parse-music` / `lilylink:build-score` /
  `lilylink:emit-score` — pipeline stages, exposed for testing.
- Conditions: `lilylink-error` (root), `lilylink-parse-error` (has
  `parse-error-message`/`line`/`col`/`token`), `lilylink-emit-error`
  (internal invariants), `lilylink-warning` (has `warning-message`/`line`/`col`).
- Recovery: see the Error handling section below.

## What is implemented

### File structure and wrappers

| LilyPond | Behavior |
| --- | --- |
| `%` line comments | Skipped by the lexer. |
| `%{ ... %}` block comments | Skipped by the lexer. |
| `\version "…"` | Consumed and ignored. |
| `\header { … }` | Block is skipped entirely (metadata is not emitted). |
| `\layout { … }`, `\midi { … }`, `\paper { … }` | Blocks are skipped. |
| `\score { … }` | Inner music is parsed; `\layout`/`\midi` inside are skipped. |
| bare `{ … }` | Parsed as absolute-mode music. |

### Notes and pitches

- Note names `c` … `b` (lowercase only).
- Accidentals (Dutch convention), attached to the note name:
  `is` (sharp), `es` (flat), `isis` (double sharp), `eses` (double flat).
  Repeated suffixes chain, so `cisis`, `aeses`, etc. work. Alterations are
  stored in semitones.
- Octave marks after the name: `'` raises one octave, `,` lowers one; they
  combine (`c''`, `c,,`). Unmarked pitches default to the octave below middle
  C (scientific octave 3).
- Reminder / cautionary accidental markers `!` and `?` are consumed and
  ignored (they carry no pitch information).
- Absolute mode: `{ c'4 d }` uses the octave marks directly.

### Relative mode

- `\relative startpitch { … }` and `\relative { … }` (no start pitch; first
  note is then absolute).
- Octave placement rule (validated against LilyPond 2.26):
  `octave = round((ref-num - target-step) / 7)`, where
  `ref-num = 7 * ref-octave + ref-step`. This is equivalent to choosing the
  octave that minimizes the diatonic interval to the reference, ignoring
  accidentals. (For example `\relative c' { c g }` yields `c4 g3`.)
- Octave marks are applied *on top of* the natural relative placement, so
  `\relative { c' <c e g> <c' e g'> <c, e, g''> }` resolves to
  `c4 [c4 e4 g4] [c5 e5 g6] [c4 e3 g5]`.
- After a chord, the reference resets to the chord's **first** note.
- Nested `\relative { … }` blocks are independent: they do not change the
  enclosing reference. Rests never change the reference.
- Accidentals do not affect octave placement.

### Durations

- Duration numbers that are powers of two: `1 2 4 8 16 32 64 128` (also
  `256`…`1024`; anything longer is rejected with a parse error).
- Augmentation dots: `c4.`, `c8..`.
- Isolated duration events reuse the preceding note's pitch
  (`c4 8` = quarter then eighth, both `c`); durations carry forward to the
  next note that omits one (`c8 d` ⇒ both eighth).
- Notes without an explicit duration default to a quarter note.

### Ties

- `~` after a note ties it to the next note of the same written pitch
  (step + alteration + octave, so `cis` does not tie to `des`).
- Tie chains work (`c4~ c4~ c4` ⇒ start / stop+start / stop), as do isolated
  durations (`a'2~ 4` uses the "last explicit pitch").
- Chord ties match by pitch: `<c e>4~ <c e>4` ties all matching notes, and
  chord-internal ties (`<c~ e>4`) tie only the marked note. Non-matching ties
  are dropped rather than carried forward (matching LilyPond), and a rest (or
  spacer `s`) breaks any pending tie.
- Emitted as `<tie>` (sound) between `<duration>` and `<type>`, and `<tied>`
  (notation) inside `<notations>`.

### Rests

- `r` (with optional duration) produces a normal MusicXML rest.
- `s` (spacer) is accepted but treated identically to `r` — it is emitted as
  a normal rest, not an invisible one.

### Chords

- `<a c e>4` — a chord of pitches sharing one duration.
- Chord notes are emitted with `<chord/>` in MusicXML.

### Bar checks and measures

- `|` closes the current measure (empty measures are not emitted).
- Measures are also split automatically when the accumulated duration reaches
  the time signature's length.
- A note, rest, or chord that would overflow a measure is split at the
  barline into dyadic-dotted pieces: notes and chords become tied pieces
  (which compose with explicit source `~` ties), rests become separate
  untied rests. See `src/duration.lisp`.
- Measure numbers are sequential from 1; a barline check that does not land on
  a measure boundary is not validated.

### Commands

- `\time n/m` — sets the meter and the measure length used for auto-splitting.
- `\key pitch \major | \minor` — key signature. Only `\major` and `\minor`
  modes; fifths computed on the circle of fifths (relative minor supported).
- `\clef treble | alto | tenor | bass` — base clefs, plus octave-shift
  suffixes `_8`/`^8`/`_15`/`^15` (quoted or bare, e.g. `\clef "treble_8"`).
  Clef changes mid-piece emit `<attributes>` in the correct measure.

### MusicXML emission

- `score-partwise` version `3.1`.
- Single `<part id="P1">` with `<part-name>Music</part-name>`.
- `<attributes>` emitted per measure: `<divisions>`, `<key>` (`<fifths>`,
  `<mode>`), `<time>` (`<beats>`, `<beat-type>`), `<clef>` (`<sign>`,
  `<line>`, optional `<octave-change>`).
- Notes: `<pitch>` (`<step>`, optional `<alter>`, `<octave>`), `<duration>`,
  `<type>`, optional `<dot/>`. Rests: `<note><rest/>…`. Chords: `<chord/>`.
- Polyphonic scores: each note carries `<voice>N</voice>` (after `<tie>`,
  before `<type>`) and voices are emitted block-per-voice with `<backup>`
  rewinds between them.
- `divisions` is computed globally as `2 ^ max(log + dots)` over all
  durations (raised as needed so every measure length is also integral in
  division units), so every `<duration>` and every measure boundary is an
  integer.
- All emitted element and attribute names are fixed literals; no user-supplied
  text reaches the XML, so no escaping is required.

### Expressive marks (ch. 3)

Attached marks follow a note/rest/chord, via `\name` commands, `-X` shorthand
articulations (`->` `-.` `-^` `-!` `-+` `--` `-_`), or `^`/`_`/`-` prefixes
(direction is parsed and ignored):

- Articulations → `<articulations>`: `\accent`, `\staccato`, `\tenuto`,
  `\marcato` (strong-accent), `\staccatissimo`, `\portato` (detached-legato),
  `\espressivo` (other-articulation), and `\breathe` (breath-mark, attached to
  the preceding note).
- Ornaments → `<ornaments>`: `\trill`, `\mordent` (inverted-mordent),
  `\prall` (mordent), `\turn`, `\reverseturn` (inverted-turn), `\slashturn`,
  and the `\upmordent`/`\prallup`/`\haydnturn` … family (other-ornament).
- Technical → `<technical>`: `\upbow`, `\downbow`, `\open`, `\stopped`,
  `\flageolet` (harmonic), `\snappizzicato`, and `\thumb`/`\heel`/`\toe` …
  (other-technical).
- Dynamics → `<dynamics>`: `\ppppp`…`\p`, `\mp`, `\mf`, `\f`…`\fffff`,
  `\fp`, `\sf`, `\sfz`, `\rfz`, `\n`; `\sff`/`\sp`/`\spp` as other-dynamics.
- `\fermata` (and short/long/henze variants) → `<fermata/>`; the variants all
  map to a plain fermata (MusicXML has no short/long types).

Spanners:

- Slurs `( … )` and phrasing slurs `\( … \)` → `<slur type="start|stop"
  number=N>` in `<notations>`, matched by a nesting stack. Phrasing slurs use
  the same element with an offset number (100 + N) since MusicXML has no
  phrase-slur element.
- Hairpins `\<`/`\>`/`\!` (and `\cr`/`\decr`/`\endcr`/`\enddecr`) →
  `<wedge>` inside `<direction>` elements (siblings of `<note>`). An open
  hairpin closes on `\!`, on an absolute dynamic, or when another hairpin
  starts.
- `\glissando` → `<glissando type="start|stop" number=N>` connecting to the
  immediately following note.
- `\startTrillSpan`/`\stopTrillSpan` → `<trill-mark/>` plus `<wavy-line>`.
- `\arpeggio` on a chord → `<arpeggiate/>`, with `\arpeggioArrowUp`/`Down`
  selecting the direction.
- `\bendAfter ±N` → `<doit/>` (up) or `<falloff/>` (down).

## What is not implemented

Anything not listed above is unsupported and will raise
`lilylink-parse-error` (an unsupported `\command`, an unexpected character,
or an unexpected token), or may misparse. Grouped by the Notation Reference
chapters:

### Pitches (ch. 1)

- Quarter-tone accidentals `ih` / `eh` / `sih` / `eseh`.
- Note names in other languages (`\language "italiano"`, `do re mi`, …).
- `\transpose`, `\fixed`, `\resetRelativeOctave`, `\octaveCheck`.
- Instrument transposition, ambitus, note-head styles, shape notes.

### Rhythms (ch. 2)

- Tuplets `\tuplet`.
- Grace notes `\grace`, `\acciaccatura`, `\appoggiatura`.
- `\partial` (pickup measures), `\longa`, `\breve`, `\maxima`.
- Tremolo (`:16` …), feathered beams, manual beams, slash/stem styles.
- Compound or Scheme-based time signatures (`\time #'(5/2 . 4)`).

### Expressive marks (ch. 3)

- Text scripts (`^"…"`), textual dynamics (`\cresc`, `\decresc`, `\dim`),
  and `\after` (delayed marks).
- Slur/hairpin styling commands (`\slurUp`, `\slurDashed`, …), `\=N` slur
  labeling, and glissando mapping (`\glissandoMap`).
- `< >` empty-chord carriers for dynamics-only placement.

### Repeats (ch. 4)

- `\repeat`, `\alternative`, `\volta`, segno/coda structures.

### Simultaneous notes (ch. 5)

- Multiple voices: `<< { v1 } \\ { v2 } [\\ { v3 } …] >>` on a single staff.
  Each expression becomes an implicit voice `1, 2, …`; events are tagged with
  a voice number and laid out into the same measure numbers, then emitted
  block-per-voice with `<voice>N</voice>` and `<backup>` rewinds. `\voiceOne`
  … `\oneVoice` and style commands are consumed and ignored (the voice number
  distinguishes voices); spanners do not cross voices.
- Deferred: the chord-forming `<< {a} {b} >>` construct (no `\\`, which would
  merge identical rhythms into chords), `\new Voice`, `\new Staff` /
  multi-staff, `\partcombine`, `\voices`, and collision merging. `|` bar
  checks advance only their own voice and are expected to align.

### Staff notation (ch. 6)

- Multiple staves / staff groups: `\new Staff`, `\new PianoStaff`,
  `\new ChoirStaff`, `\with`, instrument names.
- `\key` church modes (`\ionian` `\dorian` `\phrygian` `\lydian`
  `\mixolydian` `\aeolian` `\locrian`) and custom modes.

### Text and vocal music (ch. 8–9)

- `\markup`, text spanners, rehearsal marks.
- Lyrics: `\lyricmode`, `\addlyrics`, `\lyricsto`, melismata, hyphens,
  extenders, stanza numbers.

### Chords, percussion, specialist (ch. 10–17)

- `\chordmode` and chord-name leadsheets.
- `\drummode`, `\figuremode`, tablature, fret diagrams, ancient notation,
  fingering instructions.

### Structural / emitter gaps

- Only one part and one staff; `id="P1"` and `part-name "Music"` are
  hardcoded.
- No `<voice>` element (single-voice assumption).
- Header metadata (`title`, `composer`, …) is dropped rather than emitted as
  `<work>` / `<creator>`.
- Clef names other than `treble`/`alto`/`tenor`/`bass` error at emission
  (`ecase` in `clef-sign-line`).

## Known limitations and approximations

These are places where behavior is deliberately lenient or lossy:

- **Incomplete measures** are not padded and no warning is issued.
- **Chord duration attachment**: a `<c e g> 4` (space before the `4`) is
  treated as the chord's duration, matching `<c e g>4`, rather than as an
  isolated duration event.
- **`s` spacer rests** become ordinary visible rests.
- **Single global `divisions`** per score.
- **MusicXML is not schema-validated** by the library.

## Error handling and recovery

The pipeline signals conditions from a single hierarchy rooted at
`lilylink-error`:

- `lilylink-parse-error` — bad input, carrying the source `line`/`col` and the
  offending `token` (or NIL).
- `lilylink-emit-error` — internal invariants (should not fire on valid input).
- `lilylink-warning` — recoverable problems, reported but not fatal.

**Default lenient behavior.** By default (`*strict-mode*` is NIL), recoverable
problems — unsupported `\commands`, unknown articulations, isolated durations,
malformed `\key`/`\clef` arguments, and simultaneous-music mistakes — signal a
`lilylink-warning` and are skipped, so a conversion keeps going and produces
partial output. Bind `lilylink:*strict-mode*` to `t` to turn those into
`lilylink-parse-error` signals instead.

**Recovery restarts.** The parser establishes restarts that a caller can drive
from a `handler-bind` (all available regardless of strict mode):

- `skip-event` — drop the offending event and continue.
- `skip-command` — drop an unsupported command (and its arguments) and continue.
- `abort-parse` — stop parsing and return the events collected so far.

Structural errors — unterminated strings/blocks/`<<…>>`, and EOF where a
closing delimiter was required — are always hard `lilylink-parse-error`s.

## Testing

- Rove suite under `tests/` (run with `(asdf:test-system :lilylink)`),
  covering: tokenization (notes, accidentals, octave marks, rests, chords,
  comments, commands, ties), relative-octave resolution (scale wrap, interval
  minimization, chords, nested blocks, no-start-pitch), explicit ties (chains,
  isolated durations, chord and chord-internal ties, unmatched-tie dropping),
  duration decomposition, barline auto-splitting (notes, chords, rests),
  expressive marks (articulations, ornaments, dynamics, technical, slurs,
  phrasing slurs, hairpins, glissando, trill spans, arpeggio, breath, bends),
  voices (`<< \\ >>` parsing, voice tagging, spanner isolation, shared-measure
  layout, `<voice>`/`<backup>` emission), error handling (condition hierarchy,
  warning messages, strict vs. lenient mode, `skip-event`/`skip-command`/
  `abort-parse` restarts), and MusicXML emission (structure, attributes,
  dotted notes, rests, chords, tie and mark markers, `\score` wrappers, file
  round-trip).
- Relative-octave behavior was cross-validated against the real `lilypond`
  binary (via `\displayMusic` and MIDI output), including the counterintuitive
  chord-octave-mark results and the "nested `\relative` leaves the enclosing
  reference untouched" behavior.

## Roadmap (suggested order)

1. Multiple staves and staff groups (`\new Staff`, `\new PianoStaff`).
2. `\new Voice`/`\partcombine`, and the chord-forming `<< {a} {b} >>`.
3. `\chordmode` and lyrics.
4. `\transpose` and quarter-tone accidentals.
5. Header metadata into MusicXML `<work>` / `<creator>`.
