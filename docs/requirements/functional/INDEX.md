# functional/ — FR-<DOMAIN>-NNN, one folder per domain
Read this when: working on a domain. Skip when: the domain is not yours.

| Path | What | Read when |
|---|---|---|
| `filter/` | **Tier 1 (`FILTER`)**: `FR-FILTER-001` (`jot`, `rs`), `FR-FILTER-002` (`lam`, `vis`, `unvis`), `FR-FILTER-003` (`mdrr`) | Implementing BSD text, sequence, array-reshaping, and Markdown terminal filters |
| `fs/` | **Tier 2 (`FS`)**: `FR-FS-001` (`ls`), `FR-FS-002` (`find`), `FR-FS-003` (`mtree`) | Implementing BSD filesystem and hierarchy verification tools |
| `proc/` | **Tier 3 (`PROC`)**: `FR-PROC-001` (`apply`, `make`), `FR-PROC-002` (`sh`) | Implementing BSD command execution, DAG build, and shell tools |
| `sec/` | **Tier 4 (`SEC`)**: `FR-SEC-001` (`doas` & `doas.conf`) | Implementing OpenBSD `doas` rule parser, `-C` auditor, and privilege runner |
