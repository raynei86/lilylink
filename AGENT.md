# AGENT.md

## Project overview

Lilylink is a tool that converts Lilypond files (music notation format) into
MusicXML. It is written in Common Lisp and built with ASDF.

## Build and test

Build (load) the system from a Lisp REPL:

```lisp
(asdf:load-system :lilylink)
```

Run the test suite (Rove):

```lisp
(asdf:test-system :lilylink)
```

The test suite lives in `tests/` and is wired up via the `lilylink/tests`
system defined in `lilylink.asd`.

## Code conventions

- Source files live under `src/`; the entry point is `src/main.lisp`.
- Define packages with `uiop:define-package` and enter them with
  `(in-package #:lilylink)`.
- Write tests with Rove using `deftest`, `testing`, and `ok`.
- New systems/components must be registered in `lilylink.asd`.
- Keep the GPLv3 license and authorship attribution in the ASDF files.

## Commit style

Commits must be **atomic** and follow the **Conventional Commits** format.

Atomic commits:

- Each commit captures exactly one logical change: one feature, one fix, one
  refactor, one docs update. Do not bundle unrelated changes.
- Keep the diff as small as possible while remaining complete; split large
  changes into a sequence of self-contained commits that each leave the
  project in a working state.
- Include tests with the code they cover (or explain why tests are not
  applicable in the body).

Conventional Commits format: `<type>(<scope>): <description>` with the type
from the following set (mirroring the standard set):

- `feat` — a new feature or user-facing behavior
- `fix` — a bug fix
- `refactor` — code change that neither fixes a bug nor adds a feature
- `docs` — documentation only (including `README`/`AGENT.md`)
- `test` — adding or updating tests
- `build` — build system or external dependencies (e.g. `lilylink.asd`)
- `chore` — maintenance that does not touch source or tests (e.g. hooks,
  gitignore)

Guidelines:

- Use lowercase for the type and description; no trailing period.
- Optional scope: a short noun in parentheses when it aids clarity, e.g.
  `fix(parser): handle unterminated strings`.
- Imperative mood for the description: "add", "fix", "remove", not "added".
- Keep the subject under ~50 characters. Wrap the body at 72 columns.
- Use the body to explain the *why* and *what*, not the mechanics.

Message depth should scale with the size and impact of the diff:

- Small, self-evident changes (a one-line fix, a docs tweak) need only the
  subject, or a one-line body.
- Large diffs (a new module, a multi-file refactor, a broad behavior change)
  need a body that gives the reader the essentials without opening the diff:
  - what changed and why (the motivation, not just the mechanics),
  - the key design decisions or trade-offs made,
  - what is affected (which files/modules, what behavior changed),
  - anything deliberately left out or deferred.
- A good rule of thumb: the body should let a reviewer understand the change
  well enough to review it without reading the full diff, and a future reader
  should be able to reconstruct the *intent* from the message alone.

Example:

```
feat(parser): add top-level block parsing

Parse Lilypond blocks into an intermediate structure so subsequent
stages can consume them without re-scanning the raw source.
```

## Commit policy: LLM co-author trailer

This repository records LLM participation in commits. **Every commit must be
marked with a co-author trailer**, regardless of whether the change was written
by a human, an LLM, or a combination:

```
Co-authored-by: Deepseek V4 <info@deepseek.ai>
```

The trailer must appear in the commit message body (blank line followed by the
trailer), as a standard Git trailer, e.g.:

```
feat(parser): add top-level block parsing

Parse top-level Lilypond blocks into an intermediate structure.

Co-authored-by: Deepseek V4 <info@deepseek.ai>
```

- The trailer is automatically appended by the `commit-msg` hook in
  `.githooks/` when a commit is created through `git` (see `.githooks/commit-msg`).
- Do not remove, reorder, or edit an existing `Co-authored-by: Deepseek V4`
  trailer; only edit it if the actual LLM identity changes.
- If you commit in a way that bypasses the hook (e.g. `git commit --no-verify`),
  add the trailer manually.
