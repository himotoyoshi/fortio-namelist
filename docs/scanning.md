Scanning
========

```ruby
FortIO::Namelist.scan(input)   # -> Array of Hash
```

`scan` answers "what is in this file, and where?" without caring about the
values. Use it to build a table of contents, to tell a user which line a setting
came from, or to locate the region of a file you want to edit.

Return value
------------

One Hash per group, in file order:

```ruby
{
  group:     :physics,       # group name, lowercase Symbol
  lines:     2..7,           # 1-based Range, from the '&group' line to the terminator
  variables: [
    { name: "DT",  lineno: 3 },             # scalar
    { name: "ARR", lineno: 5, index: 1 },   # array element, 1-based subscript
    { name: "ARR", lineno: 6, index: "3:5" },  # index range
  ],
}
```

Unlike `parse`, `scan` keeps the **original case** of variable names, which is
what you want when reporting back about the user's file. Group names are still
downcased Symbols, so they match the keys `parse` returns.

* `lines` covers the whole group including its `&name` and `/` lines.
* `index` is absent for scalars, an Integer for a single subscript, a String like
  `"3:5"` for a range, and a comma-joined String for a multi-dimensional
  subscript.
* Each *assignment* produces an entry, so a variable assigned element by element
  appears once per element.
* Input with no group returns `[]`.

Example
-------

```ruby
require "fortio-namelist"

input = %{
&PHYSICS
  DT = 24
  DX = 5000
  ARR(1) = 10
  ARR(2) = 20
/
&RADIATION
  DTRADS = 312
/
}

FortIO::Namelist.scan(input).each do |entry|
  printf "&%-10s lines %d..%d\n", entry[:group], entry[:lines].first, entry[:lines].last
  entry[:variables].each do |var|
    printf "    %-8s line %-3d %s\n", var[:name], var[:lineno], var[:index] ? "index #{var[:index]}" : ""
  end
end

# &physics    lines 2..7
#     DT       line 3
#     DX       line 4
#     ARR      line 5   index 1
#     ARR      line 6   index 2
# &radiation  lines 8..10
#     DTRADS   line 9
```

Finding where a setting is defined
----------------------------------

```ruby
def locate (input, name)
  FortIO::Namelist.scan(input).each do |entry|
    entry[:variables].each do |var|
      return [entry[:group], var[:lineno]] if var[:name].casecmp?(name)
    end
  end
  nil
end

locate(File.read("config.nml"), "dtrads")   # => [:radiation, 9]
```

Cost
----

`scan` runs the same parser as `parse`; it is not a cheaper pre-pass, and a
syntax error raises just as it would in `parse`. Call it for the structure, not
for speed.

Next: [Editing](editing.md).
