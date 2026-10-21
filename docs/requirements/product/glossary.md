---
id: PRODUCT-GLOSSARY
title: Glossary and domain codes
status: approved
---
# Glossary

| Term | Definition | Domain code |
|---|---|---|
| BSD Text & Table Filters | Pure stream, sequence, and matrix-reshaping utilities (`jot`, `rs`, `lam`, `vis`, `unvis`) | `FILTER` |
| BSD Filesystem & Hierarchy Tools | Directory listing, boolean tree search, temporary file/directory creation, and declarative hierarchy auditing (`ls`, `find`, `mtree`, `mktemp`) | `FS` |
| BSD Process, Build & Shell Tools | Command batching, DAG build automation, POSIX/BSD command interpreter, timeout execution, and socket I/O (`apply`, `make`, `sh`, `timeout`, `nc`) | `PROC` |
| BSD Security & Privilege Tools | Rule-based privilege escalation, configuration auditing, and cryptographic digests/HMAC/Argon2id (`doas`, `doas.conf`, `sha256`) | `SEC` |

Domain codes are UPPERCASE, ≤ 8 chars, and name folders in `docs/requirements/functional/` and `docs/tests/`.
