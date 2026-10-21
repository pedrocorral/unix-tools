---
id: FR-FILTER-003
title: mdrr(1) Terminal Markdown Formatter and Reader in Pure Ridgun
status: implemented
priority: must
created: 2026-10-21
updated: 2026-10-21
depends_on: [ADR-0001]
verified_by: [TS-FILTER-003]
---
# FR-FILTER-003 — `mdrr(1)` Terminal Markdown Formatter and Reader in Pure Ridgun

## Statement
The project SHALL provide a pure Ridgun implementation of `mdrr` (**MD Ridgun Reader**, `src/main/ridgun/mdrr/main.rg` and `src/main/ridgun/lib/md_lib.rg`), a terminal Markdown (`.md`) parser, formatter, and reader for Linux, WSL, and macOS terminals.

## Functional Specification

### 1. Synopsis & Input Handling
- **Synopsis**: `mdrr [-p|--plain] [-a|--ascii] [-n|--line-numbers] [-t|--toc] [-s|--section heading] [-w|--width cols] [file ...]`
- **Input**: Reads one or more `.md` files in order, or reads from standard input (`/dev/stdin`) when no file arguments (or `-`) are supplied.

### 2. Block-Level Markdown & GFM Formatting
- **YAML Front-Matter (`--- ... ---`)**: Recognizes leading YAML metadata blocks and renders key-value metadata inside a compact header box.
- **ATX Headings (`#` through `######`)**: Renders levels 1–6 with hierarchical styling, ANSI color accents, and horizontal rules for top-level headings.
- **Fenced Code Blocks (` ```lang ... ``` ` and `~~~`)**: Renders code inside a bordered frame (`╭─ lang ─...` in Unicode mode, `+-- lang --...` in `-a` ASCII mode) with syntax highlighting for keywords, strings, numbers, and comments, plus optional line numbers (`-n`).
- **GFM Tables (`| A | B |` + alignment row `| :--- | ---: |`)**: Automatically computes column widths across all rows, aligns cells (left, center, right), and renders bordered tables (`┌─┬─┐`, `│ │ │`, `├─┼─┤`, `└─┴─┘`).
- **Blockquotes & GitHub Alerts (`> ...`, `> [!NOTE]`, `> [!TIP]`, `> [!IMPORTANT]`, `> [!WARNING]`, `> [!CAUTION]`)**: Renders vertical quote bars (`┃`) and distinct colored callout headers for GitHub alert blocks.
- **Lists (`-`, `*`, `+`, ordered `1.`, and task lists `- [ ]` / `- [x]`)**: Formats unordered bullets (`•`), numbered items, and task checkboxes (`☐` / `☑`).
- **Horizontal Rules (`---`, `***`, `___`)**: Renders a full-width horizontal divider across the configured terminal width.
- **Paragraph Word-Wrapping (`-w cols`)**: Wraps prose lines cleanly to `cols` (default `80`) using visible character width (ignoring zero-width ANSI SGR escape sequences).

### 3. Inline Markdown Formatting
- Renders bold (`**text**`), italic (`*text*`), bold-italic (`***text***`), strikethrough (`~~text~~`), inline code spans (`` `code` ``), links (`[label](https://...)`), and images (`![alt](https://...)`).

### 4. Navigation & Output Modes
- `-t` / `--toc`: Extracts and prints an indented Table of Contents of all Markdown headings (`#`–`######`).
- `-s <heading>` / `--section <heading>`: Filters the document to render only the matching heading section and its nested subsections.
- `-p` / `--plain`: Disables ANSI color escapes while preserving structural layout (boxes, tables, bullets, wrapping).
- `-a` / `--ascii`: Uses pure 7-bit ASCII characters (`+`, `-`, `|`, `*`, `[ ]`, `[x]`) instead of Unicode box-drawing glyphs.

## Acceptance Criteria
- AC1: `mdrr` formats headings, inline spans (`**bold**`, `*italic*`, `` `code` ``, `[link](https://...)`), lists (including `- [ ]` / `- [x]` task lists), blockquotes, and GitHub alerts (`> [!NOTE]`, `> [!WARNING]`) with ANSI styling in normal mode and clean ANSI-free text in `-p` (`--plain`) mode.
- AC2: `mdrr` parses GFM tables with `:---`, `:---:`, and `---:` alignments and fenced code blocks (` ```ridgun `) with `-n` line numbers into properly aligned Unicode or `-a` ASCII boxes.
- AC3: `mdrr -t` outputs the hierarchical Table of Contents, and `mdrr -s <heading>` extracts and renders only the requested section.
