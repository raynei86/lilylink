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
by a human, an LLM, or a combination. The trailer credits whichever LLM model
produced the change; it is not tied to a specific vendor.

The `commit-msg` hook in `.githooks/` appends the trailer automatically when a
commit is created through `git`. It resolves the identity in this order:

1. the `LLM_COAUTHOR` environment variable, or
2. the `llm.coauthor` git config value.

Set the current model before committing, e.g.:

```
git config llm.coauthor "Claude <noreply@anthropic.com>"
```

The value is `Name <email>`; the hook prepends `Co-authored-by:` and appends it
as a standard Git trailer (blank line followed by the trailer), e.g.:

```
feat(parser): add top-level block parsing

Parse top-level Lilypond blocks into an intermediate structure.

Co-authored-by: Claude <noreply@anthropic.com>
```

### Model initialization

On initialization, **every model must check `$LLM_COAUTHOR`**. If the variable
is unset, empty, or already names a different model, the model must export it
with its own identity before committing anything:

```sh
export LLM_COAUTHOR="<self> <<self-email>>"
```

- Do not overwrite `LLM_COAUTHOR` when it already identifies the model that is
  currently working; leave it as-is.
- Exporting the variable is session-scoped; if the identity is not persisted
  for the session, also record it with `git config llm.coauthor` so the hook
  keeps working in later sessions.

- If neither `LLM_COAUTHOR` nor `llm.coauthor` is set, the hook adds nothing.
- The hook is idempotent: it leaves a message untouched if the trailer is
  already present.
- Do not remove, reorder, or edit an existing `Co-authored-by:` trailer; only
  change it when the actual LLM identity changes.
- If you commit in a way that bypasses the hook (e.g. `git commit --no-verify`),
  add the trailer manually.
