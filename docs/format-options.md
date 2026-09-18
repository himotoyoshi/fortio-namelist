Format options
==============

Every keyword argument of `FortIO::Namelist.dump` (and of
`FortIO::Namelist.filter`, which passes them straight through). The default is
listed first in each section. An unknown value raises a `RuntimeError`, so typos
fail loudly rather than being silently ignored — `array_style`, `separator` and
`group_end` are checked when `dump` starts, `logical_format` and `float_format`
when a value of that type is actually written. `uppercase` is the one exception:
it is treated as a plain boolean, so any truthy value means "upper case".

All the examples below use this Hash:

```ruby
root = {group: {var1: 1, variable2: [1, 2, 3], v3: true, x: 1.0e-6}}
```

Summary
-------

| option | values | effect |
| --- | --- | --- |
| [`array_style`](#array_style) | `'stream'`, `'index'` | how array elements are written |
| [`logical_format`](#logical_format) | `'normal'`, `'short'` | `.true.` or `t` |
| [`float_format`](#float_format) | `'normal'`, `'d0'`, `'exp'` | notation for floats |
| [`alignment`](#alignment) | `'left'`, `'right'`, `'none'`, `'stream'` | where the `=` signs go |
| [`uppercase`](#uppercase) | `false`, `true` | case of names and literals |
| [`separator`](#separator) | `'comma'`, `'nl'` | separator between definitions |
| [`group_end`](#group_end) | `'slash'`, `'end'` | `/` or `&end` |
| [`indent`](#indent) | any String | indentation of definitions |

<a name="array_style"></a>
`array_style`
-------------

* `'stream'` (default) — all elements on one assignment
* `'index'` — one assignment per element, with a 1-based subscript

```ruby
puts FortIO::Namelist.dump(root, array_style: 'stream')
# &group
#   var1      = 1,
#   variable2 = 1, 2, 3,
#   v3        = .true.,
#   x         = 1.0d-06
# /

puts FortIO::Namelist.dump(root, array_style: 'index')
# &group
#   var1         = 1,
#   variable2(1) = 1,
#   variable2(2) = 2,
#   variable2(3) = 3,
#   v3           = .true.,
#   x            = 1.0d-06
# /
```

`'index'` is the safer choice when a Fortran program declares the array longer
than the data you are writing, or when a reader is picky about long lines.

<a name="logical_format"></a>
`logical_format`
----------------

* `'normal'` (default) — `.true.` / `.false.`
* `'short'` — `t` / `f`

```ruby
puts FortIO::Namelist.dump(root, logical_format: 'short')
# &group
#   ...
#   v3        = t,
#   ...
```

<a name="float_format"></a>
`float_format`
--------------

* `'normal'` (default) — the shortest plain notation, switching to a `d` exponent
  when the magnitude requires it
* `'d0'` — as `'normal'`, but plain values get an explicit `d0` exponent, which
  forces double precision in Fortran
* `'exp'` — always exponential

With `root = {g: {a: 1.0, b: 12.75, c: 50.0e-8}}`:

```ruby
puts FortIO::Namelist.dump(root, float_format: 'normal')
# &g
#   a = 1.0,
#   b = 12.75,
#   c = 5.0d-07
# /

puts FortIO::Namelist.dump(root, float_format: 'd0')
# &g
#   a = 1.0d0,
#   b = 12.75d0,
#   c = 5.0d-07
# /

puts FortIO::Namelist.dump(root, float_format: 'exp')
# &g
#   a = 1d+00,
#   b = 1.275d+01,
#   c = 5d-07
# /
```

Use `'d0'` if the receiving Fortran program reads into `REAL(8)` variables and
you care about the last digits; a literal without an exponent is single precision
by the standard.

<a name="alignment"></a>
`alignment`
-----------

* `'left'` (default) — names left-justified, `=` in a common column
* `'right'` — names right-justified against a common `=` column
* `'none'` — one definition per line, no padding
* `'stream'` — definitions packed onto as few lines as possible

`'left'` and `'right'` accept an explicit column, as in `'left:7'`; names longer
than the given width simply overflow it. `'stream'` accepts a line width, as in
`'stream:40'`.

```ruby
puts FortIO::Namelist.dump(root, alignment: 'left')
# &group
#   var1      = 1,
#   variable2 = 1, 2, 3,
#   v3        = .true.,
#   x         = 1.0d-06
# /

puts FortIO::Namelist.dump(root, alignment: 'left:7')
# &group
#   var1  = 1,
#   variable2 = 1, 2, 3,
#   v3    = .true.,
#   x     = 1.0d-06
# /

puts FortIO::Namelist.dump(root, alignment: 'right')
# &group
#        var1 = 1,
#   variable2 = 1, 2, 3,
#          v3 = .true.,
#           x = 1.0d-06
# /

puts FortIO::Namelist.dump(root, alignment: 'none')
# &group
#   var1 = 1,
#   variable2 = 1, 2, 3,
#   v3 = .true.,
#   x = 1.0d-06
# /

puts FortIO::Namelist.dump(root, alignment: 'stream')
# &group
#   var1 = 1, variable2 = 1, 2, 3, v3 = .true., x = 1.0d-06,
# /

puts FortIO::Namelist.dump(root, alignment: 'stream:40')
# &group
#   var1 = 1, variable2 = 1, 2, 3,
#   v3 = .true., x = 1.0d-06,
# /
```

<a name="uppercase"></a>
`uppercase`
-----------

* `false` (default)
* `true` — group names, variable names and logical literals in upper case, and
  `d` exponents as `D`. Quoted string *contents* are never touched.

```ruby
puts FortIO::Namelist.dump(root, uppercase: true)
# &GROUP
#   VAR1      = 1,
#   VARIABLE2 = 1, 2, 3,
#   V3        = .TRUE.,
#   X         = 1.0D-06
# /
```

<a name="separator"></a>
`separator`
-----------

* `'comma'` or `','` (default) — comma at the end of each line
* `'nl'` or `"\n"` — newline only

```ruby
puts FortIO::Namelist.dump(root, separator: 'nl')
# &group
#   var1      = 1
#   variable2 = 1, 2, 3
#   v3        = .true.
#   x         = 1.0d-06
# /
```

Both are valid namelist. Some older readers are happier with one than the other,
which is the reason the choice exists.

<a name="group_end"></a>
`group_end`
-----------

* `'slash'` or `'/'` (default)
* `'end'` — terminate with `&end`

```ruby
puts FortIO::Namelist.dump(root, group_end: 'end')
# &group
#   ...
# &end
```

<a name="indent"></a>
`indent`
--------

Any String, used as the indentation of each variable definition. The default is
two spaces.

```ruby
puts FortIO::Namelist.dump(root, indent: ' ' * 4)
# &group
#     var1      = 1,
#     ...
# /
```

Combining options
-----------------

The options are independent and can be combined freely:

```ruby
puts FortIO::Namelist.dump(root,
                           uppercase:      true,
                           array_style:    'index',
                           logical_format: 'short',
                           float_format:   'd0',
                           group_end:      'end',
                           indent:         ' ' * 4)
```

[`examples/03_format_options.rb`](../examples/03_format_options.rb) prints every
option side by side.

Next: [Scanning](scanning.md).
