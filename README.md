# csem

A C/C++ project for parsing and semantic processing based on a defined grammar, with build rules, test inputs, a reference binary, and scripts to validate output differences. The repository includes source code under `src/`, headers in `include/`, example inputs and traces, and a report PDF summarizing the work.

Languages: C++ (59%), C (37%), Makefile (2.2%), Other (1.8)

## Table of Contents
- Overview
- Repository layout
- Build
- Run
- Testing and validation
- Development notes
- License

## Overview

This project appears to implement a compiler-like semantic engine or parser named “csem,” guided by a grammar specification and sample inputs. You can build the project via the `makefile`, run the resulting binary against provided inputs, and compare outputs with a reference executable. The repository also includes a PDF report.

See the report: [csem.pdf](https://github.com/dhrumilp12/csem/blob/main/csem.pdf)

## Repository layout

Top-level:
- [.gitattributes](https://github.com/dhrumilp12/csem/blob/main/.gitattributes) — Git attributes.
- [makefile](https://github.com/dhrumilp12/csem/blob/main/makefile) — Build rules for the project.
- [notes.txt](https://github.com/dhrumilp12/csem/blob/main/notes.txt) — Development notes.
- [grammar.txt](https://github.com/dhrumilp12/csem/blob/main/grammar.txt) — Grammar specification used by the parser/semantic engine.
- [print.c](https://github.com/dhrumilp12/csem/blob/main/print.c) — Small C utility/source (likely printing helpers or a minimal example).
- [sem_with_input1.cpp](https://github.com/dhrumilp12/csem/blob/main/sem_with_input1.cpp) — C++ implementation variant demonstrating semantics with a specific input file.
- [diff.sh](https://github.com/dhrumilp12/csem/blob/main/diff.sh) — Shell script to compare outputs.
- [csem.pdf](https://github.com/dhrumilp12/csem/blob/main/csem.pdf) — Project report.
- [._csem.pdf](https://github.com/dhrumilp12/csem/blob/main/._csem.pdf) — macOS metadata (can be ignored).

Executables and artifacts:
- [csem](https://github.com/dhrumilp12/csem/blob/main/csem) — Built binary of the project (example artifact).
- [csem_exe](https://github.com/dhrumilp12/csem/blob/main/csem_exe) — Alternate or minimal executable example.
- [ref_csem](https://github.com/dhrumilp12/csem/blob/main/ref_csem) — Reference executable for output comparison.

Project structure:
- [.vscode/](https://github.com/dhrumilp12/csem/tree/main/.vscode) — Editor configuration (VS Code).
- [include/](https://github.com/dhrumilp12/csem/tree/main/include) — Headers.
- [lib/](https://github.com/dhrumilp12/csem/tree/main/lib) — Libraries (placeholder or built artifacts).
- [src/](https://github.com/dhrumilp12/csem/tree/main/src) — Source files (main implementation).
- [obj/](https://github.com/dhrumilp12/csem/tree/main/obj) — Object files/build intermediates.
- [inputs/](https://github.com/dhrumilp12/csem/tree/main/inputs) — Input files/examples for testing.
- [test/](https://github.com/dhrumilp12/csem/tree/main/test) — Test resources.

## Build

Requirements:
- GCC/G++ (or Clang)
- Make
- A POSIX shell (for `diff.sh`)

Build steps:
```sh
git clone https://github.com/dhrumilp12/csem.git
cd csem
make
```

The `makefile` should produce the `csem` binary and may populate `obj/` with intermediate object files. If you encounter issues, inspect and adjust compiler flags or paths in `makefile`.

Manual build examples (if needed):
```sh
# Sample C++ build (adjust source/layout if required)
g++ -O2 -Iinclude -o csem src/*.cpp

# Sample C build (for print.c or mixed sources)
gcc -O2 -Iinclude -c print.c -o obj/print.o
```

## Run

Typical usage will involve passing an input file to the engine:
```sh
./csem < inputs/example_input.txt
```

For the C++ demonstration program:
```sh
g++ -O2 -Iinclude -o sem_with_input1 sem_with_input1.cpp
./sem_with_input1 < inputs/input1.txt
```

Note: The actual CLI may differ. Check the source files under `src/` or `sem_with_input1.cpp` to confirm arguments and input methods. Some programs may read from stdin; others may accept file paths or flags via command line.

## Testing and validation

Compare your program’s output with the reference:
```sh
# Using the provided script
bash diff.sh

# Or manual comparison
./csem < inputs/input1.txt > my_output.txt
./ref_csem < inputs/input1.txt > ref_output.txt
diff -u ref_output.txt my_output.txt
```

If differences arise, verify:
- Input file format matches `grammar.txt`
- Compiler version/flags (e.g., undefined behavior or locale/whitespace differences)
- You are using the correct executable variant for the test

## Development notes

- The grammar in `grammar.txt` is central to parsing; changes here may require corresponding updates to lexer/parser code under `src/` and `include/`.
- Consider adding:
  - A dedicated CLI with `--input`, `--grammar`, and `--mode` flags
  - Structured error reporting and logging
  - Unit tests under `test/` for grammar rules and semantic actions
- `notes.txt` documents assumptions or todos; keep it updated as the implementation evolves.
- `csem_exe` suggests a minimal or alternate entry point; consolidate or document differences against `csem`.

## License

No explicit license file is present. If you plan to share or extend this work, consider adding a LICENSE (e.g., MIT, Apache-2.0, GPL-3.0) to clarify usage terms.

---
For questions or contributions, please open an issue or submit a PR.
