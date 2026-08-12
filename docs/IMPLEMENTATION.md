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
| `src/lexer.lisp` | Hand-written tokenizer; emits `token` structs |
| `src/parser.lisp` | Recursive-descent parser; resolves `\relative` octaves |
| `src/measure.lisp` | Groups events into measures, computes `divisions` and key fifths |
| `src/musicxml.lisp` | Emits `score-partwise` XML with escaping |

### Model classes (`src/model.lisp`)

- `pitch` — `step` (0–6, c=0 … b=6), `alter` (semitones: +1 sharp, −1 flat),
  `octave` (scientific; middle C = 4).
- `duration` — `log` (log2 of the reciprocal; 0 = whole, 2 = quarter),
  `dots`.
- `note` — a `pitch` + `duration`.
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
- `lilylink:lilylink-parse-error` — condition with `parse-error-message`,
  `parse-error-line`, `parse-error-col`.

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
  `256`…`1024`).
- Augmentation dots: `c4.`, `c8..`.
- Isolated duration events reuse the preceding note's pitch
  (`c4 8` = quarter then eighth, both `c`); durations carry forward to the
  next note that omits one (`c8 d` ⇒ both eighth).
- Notes without an explicit duration default to a quarter note.

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
- `divisions` is computed globally as `2 ^ max(log + dots)` over all
  durations, so every `<duration>` is an integer.
- All emitted element and attribute names are fixed literals; no user-supplied
  text reaches the XML, so no escaping is required.

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

- Ties `~` (the `~` character errors in the lexer).
- Tuplets `\tuplet`.
- Grace notes `\grace`, `\acciaccatura`, `\appoggiatura`.
- `\partial` (pickup measures), `\longa`, `\breve`, `\maxima`.
- Tremolo (`:16` …), feathered beams, manual beams, slash/stem styles.
- Compound or Scheme-based time signatures (`\time #'(5/2 . 4)`).

### Expressive marks (ch. 3)

- Slurs `( … )`, phrasing slurs `\( … \)`.
- Articulations and ornaments (`-.` `-^` `-+` `-!` `->` `\fermata`
  `\prall` `\trill` …).
- Dynamics (`\f`, `\p`, `\mf`, `\pp`, …) and hairpins (`\<` `\>` `\!`).
- Text scripts (`^"…"`), glissando, arpeggio, breath marks.

### Repeats (ch. 4)

- `\repeat`, `\alternative`, `\volta`, segno/coda structures.

### Simultaneous notes (ch. 5)

- Multiple voices: `<< … >>` (parses as chord brackets and will not behave
  like parallel music) and `\\` (errors).
- Clusters, `\partcombine`.

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

- **Barline overflow**: a note whose duration crosses a measure boundary is
  kept whole in the earlier measure (no automatic tie-splitting), so that
  measure's content can exceed the time signature. This is not validated.
- **Incomplete measures** are not padded and no warning is issued.
- **Chord duration attachment**: a `<c e g> 4` (space before the `4`) is
  treated as the chord's duration, matching `<c e g>4`, rather than as an
  isolated duration event.
- **`s` spacer rests** become ordinary visible rests.
- **Single global `divisions`** per score.
- **MusicXML is not schema-validated** by the library.
- Error recovery is minimal: the first `lilylink-parse-error` aborts the
  whole conversion.

## Testing

- Rove suite under `tests/` (run with `(asdf:test-system :lilylink)`),
  covering: tokenization (notes, accidentals, octave marks, rests, chords,
  comments, commands), relative-octave resolution (scale wrap, interval
  minimization, chords, nested blocks, no-start-pitch), measure auto-splitting,
  error signaling, and MusicXML emission (structure, attributes, dotted notes,
  rests, chords, `\score` wrappers, file round-trip).
- Relative-octave behavior was cross-validated against the real `lilypond`
  binary (via `\displayMusic` and MIDI output), including the counterintuitive
  chord-octave-mark results and the "nested `\relative` leaves the enclosing
  reference untouched" behavior.

## Roadmap (suggested order)

1. Ties `~` and slurs `( )` — enables automatic barline tie-splitting and
   fixes the overflow limitation.
2. Articulations, ornaments, and dynamics.
3. Polyphony / voices (`<< >>`, `\\`) and `\voiceOne` … `\voiceFour`.
4. Multiple staves and staff groups.
5. `\chordmode` and lyrics.
6. `\transpose` and quarter-tone accidentals.
7. Header metadata into MusicXML `<work>` / `<creator>`.
