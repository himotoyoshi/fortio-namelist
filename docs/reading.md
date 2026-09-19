Reading a namelist
==================

```ruby
FortIO::Namelist.parse(input, group: nil)   # -> Hash
FortIO::Namelist.read(input, group: nil)    # alias of parse
```

Input
-----

`input` is a String containing namelist text, or any object that responds to
`#read` and returns one:

```ruby
FortIO::Namelist.parse(File.read("config.nml"))     # String
File.open("config.nml") { |f| FortIO::Namelist.parse(f) }   # IO
FortIO::Namelist.parse(StringIO.new(text))          # StringIO
```

Text outside of groups is ignored, so a namelist embedded in a larger file (a
job card, a log) is read without any preprocessing:

```ruby
FortIO::Namelist.parse("some header text\n&g\n a = 1\n/\ntrailing notes\n")
# => {g: {a: 1}}
```

Input with no group at all gives an empty Hash rather than an error:

```ruby
FortIO::Namelist.parse("nothing here")   # => {}
```

Selecting groups
----------------

By default every group in the input is read. Pass `group:` to read only some of
them — as a String, a Symbol, or an Array of either. Names are case-insensitive.

```ruby
FortIO::Namelist.parse(input, group: "grid")
FortIO::Namelist.parse(input, group: :grid)
FortIO::Namelist.parse(input, group: ["model", "grid"])
```

Asking for a group that is not in the input raises a `RuntimeError`
(`no definition of namelist group 'xxx'`), so use this to assert that a required
group is present. To check without raising, read everything and look:

```ruby
root = FortIO::Namelist.parse(input)
if root.key?(:grid)
  ...
end
```

Selecting groups does not skip parsing the rest of the file — the whole input is
parsed either way, and a syntax error anywhere still raises.

How values map to Ruby
----------------------

| namelist | Ruby |
| --- | --- |
| `123`, `-4` | `Integer` |
| `1.5`, `.5`, `1.2e3`, `1.0d-6` | `Float` |
| `'abc'`, `"abc"` | `String` |
| `.true.`, `.t`, `t`, `T` | `true` |
| `.false.`, `.f`, `f`, `F` | `false` |
| `(1.0, 2.0)` | `Complex` |
| `abc` (unquoted) | `String` |
| `1, 2, 3` | `Array` |
| a skipped element (`1,,3`) | `nil` inside the Array |

```ruby
FortIO::Namelist.parse("&g\n i=1\n f=1.0d-6\n s='a'\n l=.t\n c=(1.0,2.0)\n/")
# => {g: {i: 1, f: 1.0e-06, s: "a", l: true, c: (1.0+2.0i)}}
```

Both `e` and `d` exponents are accepted; `d` is the usual Fortran double
precision form and becomes an ordinary Ruby `Float`.

Arrays
------

A variable is an Array in Ruby whenever the namelist gives it more than one
value, in either notation:

```ruby
FortIO::Namelist.parse("&g\n v = 10, 20, 30\n/")             # stream notation
# => {g: {v: [10, 20, 30]}}

FortIO::Namelist.parse("&g\n v(1)=10\n v(2)=20\n v(3)=30\n/") # index notation
# => {g: {v: [10, 20, 30]}}
```

Indices are 1-based in the namelist and are translated to 0-based Ruby Array
positions. Elements that are never assigned are `nil`:

```ruby
FortIO::Namelist.parse("&g\n v(5) = 7\n/")
# => {g: {v: [nil, nil, nil, nil, 7]}}
```

Subscripts start at 1, as in Fortran. A subscript below 1, or a range that ends
before it starts, is a parse error rather than something quietly reinterpreted.

Because a variable occupies a Ruby Array as long as its highest subscript, a
subscript is also capped: `FortIO::Namelist.max_array_size` (one million by
default) is the largest array a single variable may occupy, and a subscript or
repeat count beyond it raises. Raise the cap if you really do read arrays that
large:

```ruby
FortIO::Namelist.max_array_size = 50_000_000
```

Index ranges and repeat counts work as expected:

```ruby
FortIO::Namelist.parse("&g\n v(3:5) = 7, 8, 9\n/")
# => {g: {v: [nil, nil, 7, 8, 9]}}

FortIO::Namelist.parse("&g\n v = 3*1.5\n/")     # 3*1.5 means "1.5 three times"
# => {g: {v: [1.5, 1.5, 1.5]}}
```

A single value stays a scalar — it does not become a one-element Array. If a
variable may be either, normalize it yourself:

```ruby
levels = Array(root[:grid][:levels])
```

Repeated definitions
--------------------

If the same group appears twice, the later one wins; the same holds for a
variable defined twice within one group. This matches the common Fortran runtime
behaviour of the last read value taking effect.

```ruby
FortIO::Namelist.parse("&g\n a=1\n/\n&g\n a=2\n/")
# => {g: {a: 2}}
```

Next: [Writing a namelist](writing.md).
