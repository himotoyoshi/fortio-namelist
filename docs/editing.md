Editing an existing namelist
============================

```ruby
FortIO::Namelist.filter(input, **format_options) { |root| ... }   # -> String
```

`filter` is `parse` → your block → `dump` in one call. Modify the Hash in the
block; the return value is the new namelist String.

```ruby
require "fortio-namelist"

output = FortIO::Namelist.filter(File.read("config.nml")) do |root|
  root[:model][:nx]      = 256          # change a value
  root[:model][:comment] = "resumed"    # add a variable
  root[:grid][:levels] << 60            # extend an array
  root[:output].delete(:interval)       # remove a variable
end

File.write("config.nml", output)
```

The block's return value is ignored — mutate the Hash you are given.
`format_options` are the same as [`dump`](format-options.md)'s.

Deleting the last variable of a group leaves the group itself in place, as an
empty group (`&output` followed by `/`), which reads back as an empty Hash. Drop
the group key too if you want it gone entirely.

Adding and removing groups works the same way, since `root` is an ordinary Hash:

```ruby
FortIO::Namelist.filter(input) do |root|
  root[:extra] = {flag: true}   # appended after the existing groups
  root.delete(:obsolete)
end
```

Doing it by hand
----------------

`filter` is a convenience; when you need to do something between reading and
writing, use the two methods directly:

```ruby
root = FortIO::Namelist.parse(File.read("config.nml"))
root[:model][:nx] = choose_resolution(root)
File.write("config.nml", FortIO::Namelist.dump(root, uppercase: true))
```

What editing does *not* preserve
--------------------------------

This is a regenerate-from-data workflow, not a patch. The output is produced
entirely from the Hash, so **comments, blank lines and the original layout of the
input are lost**, and names are folded to a single case. For a hand-maintained
configuration file whose comments matter, that may be unacceptable.

Ways around it today:

* Keep the generated file separate from the hand-written one: read the
  hand-written master, `dump` a derived file for the Fortran program to consume.
* Use [`scan`](scanning.md) to find the exact line of the variable you want to
  change, and rewrite that line yourself with ordinary string handling. `scan`
  gives you the original name and line number, and `parse` on the result tells
  you whether you broke anything.

A layout-preserving `FortIO::Namelist.edit` is sketched in the project's
`TODO.md`, but is not implemented.

Writing files safely
--------------------

`dump` returns a String and never touches the filesystem, which makes it easy to
validate before overwriting anything:

```ruby
text = FortIO::Namelist.dump(root)
FortIO::Namelist.parse(text)          # raises if the result is not readable
File.write("config.nml", text)
```

Next: [Namelist syntax accepted](namelist-syntax.md).
