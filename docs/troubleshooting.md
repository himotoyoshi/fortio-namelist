Troubleshooting
===============

Error messages
--------------

Everything this library raises is a `RuntimeError`, so a single `rescue` catches
both parse and format problems:

```ruby
begin
  root = FortIO::Namelist.parse(text)
rescue RuntimeError => err
  warn err.message
end
```

### `namelist parse error on value ... in &g ... &end`

The input is not valid namelist as far as the parser is concerned. The message
names the group it was in and prints the offending line with its neighbours:

```
namelist parse error on value nil ("/") in &g ... &end
      2:   v = a, b
>>    3: /
```

The `>>` line is where the parser gave up, which is often one line *after* the
real mistake — an unterminated string or an unquoted string list typically only
fails at the next token.

Common causes, in rough order of frequency:

* a list of unquoted strings without a trailing comma (`v = a, b`) — see
  [Namelist syntax](namelist-syntax.md#unquoted-strings)
* quoted and unquoted strings mixed in one list (`v = "a", b`)
* an unterminated quote, which swallows the rest of the file
* a missing group terminator
* a stray `=` or an otherwise malformed assignment

### `namelist parse error ('...')`

The scanner hit a character it has no rule for; the text in parentheses is the
rest of that line. Derived-type syntax (`var%component = 1`) reaches you this
way.

### `namelist parse error: array subscript ...` / `repeat count ...`

An array subscript below 1, a subscript range that ends before it starts, a
negative repeat count, or a subscript or repeat count larger than
`FortIO::Namelist.max_array_size` (one million by default).

The cap exists because a variable occupies a Ruby Array as long as its highest
subscript: without it, `v(1000000000) = 1` would make a thirty byte input
allocate eight gigabytes. If your files genuinely contain arrays that large,
raise it:

```ruby
FortIO::Namelist.max_array_size = 50_000_000
```

[`scan`](scanning.md) never builds the values, so it reports the subscripts of a
file without allocating anything — useful for inspecting a file you do not
trust yet.

### `no definition of namelist group 'xxx'`

`parse(input, group: "xxx")` was asked for a group the input does not contain.
Read the whole input and check `root.key?(:xxx)` if the group is optional.

### `invalid keyword argument 'xxx' (should be ...)`

A [format option](format-options.md) was given a value it does not understand.
The message lists the accepted values.

Known limitations
-----------------

### Layout is not preserved when writing

`dump` regenerates the text from the Hash, so comments, blank lines and the
original spacing of an input file are lost. See [Editing](editing.md).

### Scalars and one-element arrays are indistinguishable

`v = 1` and a one-element array both become the scalar `1`, and `dump` writes a
one-element Ruby Array as a scalar. Use `Array(value)` on the reading side if a
variable may legitimately have one element.

### `dump` does not validate values

Values of unsupported types are written through `to_s`, producing text that will
not read back. Stick to Integer, Float, String, true/false, Complex, `nil`, and
Arrays of those.

Getting more detail
-------------------

If a file will not parse and the message is not enough, narrow it down by
scanning first — `FortIO::Namelist.scan` fails at the same place but shows you
which groups were read successfully before that — or by feeding the parser one
group at a time:

```ruby
text.scan(/^\s*[&$]\w+.*?^\s*(?:\/|[&$]end)/mi).each do |chunk|
  begin
    FortIO::Namelist.parse(chunk)
  rescue RuntimeError => err
    warn "failed chunk:\n#{chunk}\n#{err.message}"
  end
end
```

Next: [API reference](api-reference.md).
