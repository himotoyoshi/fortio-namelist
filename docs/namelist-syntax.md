Namelist syntax accepted
========================

The namelist format is barely standardised, and every Fortran compiler accepts a
slightly different dialect of it. This library's parser is deliberately
permissive: it aims to read what real files contain, not only what the standard
allows. This page describes what it accepts.

Groups
------

```fortran
&name        ! '&' or '$' may start a group
  ...
/            ! '/', '&end' or '$end' may end it
```

* Both `&` and `$` work as the group prefix, and so do `&end` / `$end` as
  terminators. The prefix of the opening and closing lines is **not** checked for
  consistency, so `$name ... &end` is accepted.
* Group names are case-insensitive and become lowercase Symbols.
* Anything outside of a group is ignored — headers, trailing notes, job card
  text around the namelist all cost you nothing.
* A group with no variables is valid and yields an empty Hash.
* If the same group name appears twice, the later definition wins.

Variable definitions
--------------------

```fortran
  var  = 1              ! scalar
  var  = 1, 2, 3        ! array, stream notation
  var(2) = 5            ! array, single element (1-based)
  var(3:5) = 7, 8, 9    ! array, index range
  var(2,3) = 5          ! multi-dimensional subscript (see the note below)
  v1 = 1, v2 = 2        ! several definitions on one line
  var = 3*1.5           ! repeat count: three elements of 1.5
  var = 1, , 3          ! skipped element, nil in Ruby
```

A multi-dimensional subscript parses, but the result is not a nested Ruby
Array: the value is stored in a flat Array at the position of the first
subscript, so `var(2,3) = 5` gives `{var: [nil, 5]}`. `scan` does report the
full subscript (`index: "2,3"`), so multi-dimensional files can still be
inspected. If you need real multi-dimensional data, reshape it yourself.

Definitions are separated by a comma, a newline, or both. An assignment may be
spread over several lines; the `=` may even be on a different line than the name.

Values
------

```fortran
  i = 123, -4                 ! integer
  f = 1.5, .5, 1., 1.2e3      ! real, 'e' exponent
  d = 1.0d-6, 5D+3            ! real, 'd' exponent (both give a Ruby Float)
  s = 'abc', "abc"            ! character, either quote
  q = 'it''s', "say ""hi"""   ! a quote is doubled to escape it
  l = .true., .t, t, T        ! logical true
  l = .false., .f, f, F       ! logical false
  c = (1.0, 2.0)              ! complex
```

Logical literals are generous: anything of the form `.t...`, `.f...` counts, so
`.t`, `.t_1`, `.TRUE.` are all true. A bare `t` or `f` is logical too — unless
it is followed by `=`, in which case it is read as a variable named `t`.

Comments
--------

`!` starts a comment that runs to the end of the line. Comments are allowed
anywhere inside a group and are discarded by `parse` — including between a
variable name and its `=`, and between the values of a list. They are **not**
reproduced by `dump`.

A comment is transparent to the line structure, so a value list may be continued
on the next line after one:

```fortran
  v = 1, 2,   ! the first half
      3, 4
```

[`scan`](scanning.md) still reports the line of the variable's *name*, not of
its last value.

Unquoted strings
----------------

A bare word on the right-hand side is read as a String:

```fortran
  v = abc        ! => "abc"
  v = 0_b        ! => "0_b"   (starts with a digit, still a string)
  v = _c         ! => "_c"
```

This is where the free format bites. A list of unquoted words is ambiguous
against a list of further definitions, and the parser resolves that by requiring
a **trailing comma** on a list of unquoted strings:

```fortran
  v = a, b, c,   ! => ["a", "b", "c"]   (note the trailing comma)
  v = a, b, c    ! parse error
```

Quoted strings have no such restriction — `v = 'a', 'b'` is fine. Quoted and
unquoted strings also cannot be mixed in one list:

```fortran
  v = "a", b, "c"   ! parse error
```

If you control the file, quote your strings.

Line continuation
-----------------

A line ending in `&` continued by a line starting with `&` is accepted and
treated as a single logical line, which is how some compilers wrap long arrays.

Case
----

Everything except the contents of quoted strings is case-insensitive.

Not supported
-------------

* Derived-type component syntax (`var%component = ...`)
* Substring assignment (`str(2:4) = 'abc'` as a character-substring, rather than
  as an array-index range — the parser reads it as the latter)

Next: [Troubleshooting](troubleshooting.md).
