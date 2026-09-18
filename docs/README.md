fortio-namelist User Guide
==========================

`fortio-namelist` reads Fortran namelist text into a Ruby Hash, and writes a
Ruby Hash back out as namelist text.

Read the guide in this order:

1. [Getting started](getting-started.md) — install, the two methods you need, a first script
2. [Reading a namelist](reading.md) — `parse`, selecting groups, how values map to Ruby
3. [Writing a namelist](writing.md) — `dump`, writing files, round trips
4. [Format options](format-options.md) — every keyword argument of `dump`
5. [Scanning](scanning.md) — `scan`, group/variable names and line numbers
6. [Editing](editing.md) — `filter`, changing an existing namelist
7. [Namelist syntax accepted](namelist-syntax.md) — what the parser does and does not accept
8. [Troubleshooting](troubleshooting.md) — error messages and known limitations
9. [API reference](api-reference.md) — the whole public API on one page

Runnable versions of most snippets in this guide live in
[`examples/`](../examples/README.md).

Design note
-----------

The library is deliberately **permissive when reading and strict when writing**.
Real-world namelist files are written by many different Fortran compilers and by
hand, so the parser accepts far more than the standard requires. The output side
does the opposite: it emits one consistent, conservative style that any Fortran
`READ(unit, nml=...)` should accept, with the details under your control through
[format options](format-options.md).
