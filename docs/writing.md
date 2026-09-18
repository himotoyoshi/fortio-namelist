Writing a namelist
==================

```ruby
FortIO::Namelist.dump(root, **format_options)   # -> String
```

`root` is a Hash with the two-level namelist structure; the return value is a
namelist String. Nothing is written to disk — that is ordinary Ruby file I/O:

```ruby
require "fortio-namelist"

root = {
  model: { title: "test run", nx: 128, dt: 7.5, debug: false },
  grid:  { levels: [10, 20, 30] },
}

File.write("config.nml", FortIO::Namelist.dump(root))
```

```fortran
&model
  title = 'test run',
  nx    = 128,
  dt    = 7.5,
  debug = .false.
/
&grid
  levels = 10, 20, 30
/
```

Groups and variables are emitted in Hash insertion order, so the order of your
Hash is the order of the file.

How Ruby values are written
---------------------------

| Ruby | namelist |
| --- | --- |
| `Integer` | `128` |
| `Float` | `7.5`, `1.0d-06` — see [`float_format`](format-options.md#float_format) |
| `String` | `'test run'` (single-quoted; see below) |
| `true` / `false` | `.true.` / `.false.` — see [`logical_format`](format-options.md#logical_format) |
| `Complex` | `(1,2)` |
| `Array` | `10, 20, 30` — see [`array_style`](format-options.md#array_style) |
| `nil` inside an Array | an empty element, i.e. a skipped value |

Strings are quoted for you. A string containing a single quote is emitted with
double quotes instead, and an embedded quote of the surrounding kind is doubled,
which is the Fortran escape:

```ruby
puts FortIO::Namelist.dump({g: {a: %q{it's}, b: %q{a"b}, c: %q{it's a "b"}}})
# &g
#   a = "it's",
#   b = 'a"b',
#   c = "it's a ""b"""
# /
```

`nil` as an array element round-trips as a skipped value:

```ruby
FortIO::Namelist.dump({g: {v: [1, nil, 3]}})   # => "&g\n  v = 1, , 3\n/\n"
FortIO::Namelist.parse(_)                      # => {g: {v: [1, nil, 3]}}
```

Only the types in the table above are supported. `dump` does not validate the
Hash it is given: anything else is written out via its `to_s`, which will
generally produce a file that is not valid namelist. Convert exotic values
(Symbols, Dates, nested Hashes) to one of the supported types yourself.

Round trips
-----------

`parse` and `dump` are inverses at the level of *data*, not of *text*:

```ruby
root = FortIO::Namelist.parse(text)
FortIO::Namelist.parse(FortIO::Namelist.dump(root)) == root   # => true
```

What is not preserved by a `parse` → `dump` round trip:

* comments and blank lines
* the original indentation, alignment and line breaks
* the original case of group and variable names (everything is folded to
  lowercase, unless you pass `uppercase: true`)
* `$group` prefixes and `&end` terminators (unless you ask for them with
  [`group_end`](format-options.md#group_end))
* the distinction between a scalar and a one-element array

If preserving the layout of an existing file matters, see
[Editing](editing.md).

Next: [Format options](format-options.md).
