API reference
=============

Everything public lives in the `FortIO::Namelist` module as a module function.
There is nothing to instantiate.

```ruby
require "fortio-namelist"
```

---

### `FortIO::Namelist.parse(input, group: nil) -> Hash`

Read namelist text and return a two-level Hash
(`{group_symbol => {variable_symbol => value}}`). Group and variable names are
downcased Symbols.

| argument | |
| --- | --- |
| `input` | a String, or any object responding to `#read` (`File`, `StringIO`, …) |
| `group:` | `nil` (all groups, the default), or a String / Symbol / Array of them |

Returns `{}` if the input contains no group at all. Raises `RuntimeError` on a
syntax error, or when a group named in `group:` is not present.

See [Reading a namelist](reading.md).

### `FortIO::Namelist.read(input, group: nil) -> Hash`

An alias of `parse`.

---

### `FortIO::Namelist.dump(root, **format_options) -> String`

Render a namelist Hash as namelist text. Groups and variables are emitted in
Hash insertion order. Nothing is written to disk.

| format option | values (default first) |
| --- | --- |
| `array_style:` | `'stream'`, `'index'` |
| `logical_format:` | `'normal'`, `'short'` |
| `float_format:` | `'normal'`, `'d0'`, `'exp'` |
| `alignment:` | `'left'`, `'right'`, `'none'`, `'stream'` (`'left:7'`, `'stream:70'` to set a column or width) |
| `uppercase:` | `false`, `true` |
| `separator:` | `'comma'` / `','`, `'nl'` / `"\n"` |
| `group_end:` | `'slash'` / `'/'`, `'end'` |
| `indent:` | `'  '` (any String) |

Raises `RuntimeError` for an unrecognised option value. See
[Writing a namelist](writing.md) and [Format options](format-options.md).

---

### `FortIO::Namelist.scan(input) -> Array<Hash>`

Report the structure of the input without interpreting values. One Hash per
group:

```ruby
{ group: :name, lines: 2..7, variables: [{name: "DT", lineno: 3}, ...] }
```

`name` keeps its original case; `lineno` is 1-based; `index` is present only for
array-element assignments. Returns `[]` if the input contains no group. Raises
`RuntimeError` on a syntax error.

See [Scanning](scanning.md).

---

### `FortIO::Namelist.filter(input, **format_options) { |root| ... } -> String`

`parse` the input, yield the Hash to the block for modification, and return the
`dump` of the result. `format_options` are those of `dump`. The block's return
value is ignored.

See [Editing](editing.md).

---

Value mapping
-------------

| namelist | Ruby (reading) | Ruby (writing) |
| --- | --- | --- |
| `123` | `Integer` | `Integer` |
| `1.5`, `1.0d-6` | `Float` | `Float` |
| `'abc'`, `abc` | `String` | `String` (always quoted on output) |
| `.true.`, `t` | `true` | `true` |
| `.false.`, `f` | `false` | `false` |
| `(1.0,2.0)` | `Complex` | `Complex` |
| `1, 2, 3` | `Array` | `Array` |
| skipped element | `nil` | `nil` |

Errors
------

All errors raised by this library are `RuntimeError`.

| message | meaning |
| --- | --- |
| `namelist parse error on value ...` | syntax error; the offending lines follow |
| `namelist parse error ('...')` | unrecognised character; the rest of the line follows |
| `no definition of namelist group 'x'` | `group:` named a group not in the input |
| `invalid keyword argument 'x' (should be ...)` | bad format option value |
| `invalid logical_format` / `invalid float_format` | bad format option value, raised when such a value is written |
