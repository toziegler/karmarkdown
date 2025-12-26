# Karmarkdown

Karmarkdown is a fast, opinionated, custom Markdown language server (LSP) focused on notes and knowledge work. It provides document/workspace symbols, link resolution, completions, and productivity code actions while staying simple and hackable.

## Features

LSP capabilities:
- Document symbols: headings, lists, tasks, tables, code fences, links, reference defs, frontmatter tags, project tags.
- Workspace symbols: all indexed symbols across the workspace.
- Completion: wiki links, link paths, heading anchors, Markdown snippets.
- Diagnostics: missing links, missing anchors, missing reference definitions.

Symbols:
- Headings: `H1: Title`
- Lists/tasks: `List: item`, `Task: [ ] todo`
- Code fences: `Code: zig`
- Tables: `Table: rows x cols`
- Links: `Link: [label](path)`
- Reference defs: `Ref: [label]`
- Tags: `Tag: #frontmatter-tag`
- Project tags: `ProjectTag: @blog` from `[@blog]`

Tags:
- Frontmatter tags only (`tags:` key in `---` frontmatter).
- Project tags via inline markers like `[@blog]` (shown as `ProjectTag:`).

Code actions:
- Create note for missing link.
- Fix broken link.
- Rename note from H1 and update inbound links.
- Extract selection to a new note (date id).
- Insert new note link (date id).
- Insert related section (based on shared tags).
- Convert CSV selection to a Markdown table.
- Insert footnote (anchor + definition; uses selection text as id when available).

Snippets:
- Code block, frontmatter, link, image, task, table, blockquote, heading, numbered list, horizontal rule, footnote anchor/definition.

## Architecture

Parser:
- Resilient LL block parser for headings, lists, tables, code fences, frontmatter, link defs.
- Pratt-style inline parser for links, wiki links, and project tags.
- Error-tolerant: parsing keeps going after invalid spans.

Design goals:
- Fast and predictable in large note sets.
- Minimal dependencies, easy to extend.
- Clear symbol model for LSP tooling.

## Usage

Build:
```
./zig/zig build
```

Run (binary):
```
./zig-out/bin/karmarkdown
```

Tests:
```
./zig/zig build test
```

## Notes

- Frontmatter tags are parsed from:
  ```
  ---
  tags: [one, two]
  ---
  ```
- Project tags are parsed from `[@tag]`.
- Footnote action uses the selection text as the id when possible; otherwise a date id.

## Development

The code lives in `src/`. The main entry points are:
- `src/parser.zig`: parsing and symbol extraction.
- `src/server.zig`: LSP handlers, code actions, completions, diagnostics.
