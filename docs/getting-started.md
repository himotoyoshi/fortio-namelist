Getting started
===============

Installation
------------

```bash
gem install fortio-namelist
```

Or in a `Gemfile`:

```ruby
gem "fortio-namelist"
```

Then, in your script:

```ruby
require "fortio-namelist"
```

The library has no runtime dependencies. Ruby 2.4 or later is required.

The two methods you need
------------------------

Almost everything is done with these two:

```ruby
FortIO::Namelist.parse(input, group: nil)   # namelist text  -> Hash
FortIO::Namelist.dump(root, **options)      # Hash           -> namelist text
```

`parse` takes a String (or anything with a `#read` method, such as a `File`) and
returns a Hash. `dump` takes that Hash and returns a String.

Two more methods cover the remaining cases:

```ruby
FortIO::Namelist.scan(input)                # structure and line numbers only
FortIO::Namelist.filter(input, **options)   # parse -> your block -> dump
```

A first script
--------------

Given `config.nml`:

```fortran
&model
  title = 'test run'
  nx    = 128
  dt    = 7.5
  debug = .false.
/
```

read it, change something, and write it back:

```ruby
require "fortio-namelist"

root = FortIO::Namelist.parse(File.read("config.nml"))
# => {model: {title: "test run", nx: 128, dt: 7.5, debug: false}}

root[:model][:nx]    = 256
root[:model][:debug] = true

File.write("config.nml", FortIO::Namelist.dump(root))
```

`config.nml` now contains:

```fortran
&model
  title = 'test run',
  nx    = 256,
  dt    = 7.5,
  debug = .true.
/
```

Note that the output is *regenerated*, not patched: comments, blank lines and the
original spacing of the input are not preserved. See
[Editing](editing.md) for what this means in practice.

The shape of the Hash
---------------------

The Hash is always two levels deep — groups on the outside, variables inside:

```ruby
{
  group1: { var1: value1, var2: value2 },
  group2: { var1: value1 },
}
```

Group and variable names are **lowercase Symbols**; values are plain Ruby objects.
Symbols were chosen as keys because they work well with the pattern matching
introduced in Ruby 2.7:

```ruby
case FortIO::Namelist.parse(text)
in {model: {nx: Integer => nx, ny: Integer => ny}}
  puts "grid is #{nx} x #{ny}"
end
```

Next: [Reading a namelist](reading.md).
