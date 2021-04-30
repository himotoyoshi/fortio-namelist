
fortio-namelist
===============

This is a Ruby library for reading and writing Fortran's namelist. 
Using this library, you can read a namelist string as a Hash object, 
or dump a Hash object to a namelist string.

Features
--------

* Flexible parsing using Racc
* Read a namelist string as a Ruby's Hash object (easy to convert to JSON or YAML)
* Dump a Hash object as namelist string (with )

Installation
------------

    gem install fortio-namelist

To use the library in your Ruby script, 

```ruby
require "fortio-namelist"
```

Usage
------

### Useful methods

The user only needs to remember the following two methods.

    FortIO::Namelist.parse(input, group: nil)
    FortIO::Namelist.dump(root, **format_options)

### Reading namelist string

To create a Hash object with namelist structure by reading a namelist string, use the following method.

    FortIO::Namelist.parse(input, group: nil)

The argument `input` is given as a string, but it also accepts objects with a method `#read` returns a string like an IO object ('duck typing'). 

If the keyword argument `group` is omitted, all namelist groups included in `input` will be read. To read only a specific group, give a group name to `group`. To load multiple groups, give an array of group names to `group`.

Use lowercase Symbol objects for both group and variable names. The Hash object of the return value has a two-level structure as follows.

    {
      group1: {
         var11: value11,
         var12: value12, 
            :          :
      }
      group2: {
         var21: value21,
         var22: value22,
            :          :
      }
          :        
          :
    }

The value can be Ruby's String, Integer, Float, Complex, TrueClass, or FalseClass objects, depending on the literal in the namelist. In the case that the value is an array, it will be expressed as an Array in Ruby.

Example:

```ruby
require 'fortio-namelist'

input = %{
&group1
  var1 = 11
  var2 = 12
/
&group2
  var1 = 12
  var2 = 22
/
&group3
  var1 = 31
  var2 = 32
/
}

### read all groups
root = FortIO::Namelist.parse(input)
# => {:group1=>{:var1=>11, :var2=>12},
#     :group2=>{:var1=>12, :var2=>22},
#     :group3=>{:var1=>31, :var2=>32}}

### read only "group2"
root = FortIO::Namelist.parse(input, group: "group2")
# => {:group2=>{:var1=>12, :var2=>22}}

### read only "group1" and "group3"
root = FortIO::Namelist.parse(input, group: ["group1", "group3"])
# => {:group1=>{:var1=>11, :var2=>12}, 
#     :group3=>{:var1=>31, :var2=>32}}

```

### Generating namelist string from Hash object with namelist structure

To generate a namelist string from a Hash object with a namelist structure, use the following method.

    FortIO::Namelist.dump(root, **format_options)

The argument `root` is given as a Hash object. The return value is a namelist string. 

Example:

```ruby
require 'fortio-namelist'

root = {group1: {var1: 11, var2: 12},
        group2: {var1: 12, var2: 22},
        group3: {var1: 31, var2: 32}}

puts FortIO::Namelist.dump(root)
```

This script print a namelist format string to stdout.

    &group1
      var1 = 11,
      var2 = 12
    /
    &group2
      var1 = 12,
      var2 = 22
    /
    &group3
      var1 = 31,
      var2 = 32
    /

### Format options for `FortIO::Namelist.dump`

You can finely control the output namelist string with the following keyword arguments (the first one is the default).

#### `array_style` specifies the notation for array elements

* 'stream' : (default)
* 'index'  :

Example:

```ruby
root = {group: {var1: [1,2,3], var2: ["a","b","c"]}}

puts FortIO::Namelist.dump(root, array_style: 'stream')
# =>
# &group
#   var1 = 1, 2, 3,
#   var2 = 'a', 'b', 'c'
# /

puts FortIO::Namelist.dump(root, array_style: 'index')
# =>
# &group
#   var1(1) = 1,
#   var1(2) = 2,
#   var1(3) = 3,
#   var2(1) = 'a',
#   var2(2) = 'b',
#   var2(3) = 'c'
# /
```

#### `logical_format` specifies boolean literals

* 'normal' : normal notation like `.true.`, `.false` (default)
* 'short'  : short notation like `t`, `f`

```ruby
root = {group: {var1: true, var2: false}}

puts FortIO::Namelist.dump(root, logical_format: 'normal')
# =>
# &group
#   var1 = .true.,
#   var2 = .false.
# /

puts FortIO::Namelist.dump(root, logical_format: 'short')
# =>
# &group
#   var1 = t,
#   var2 = f
# /
```

#### `float_format` specifies the notation for floating point numbers

* 'normal' : format with "%g" (default)
* 'd0'     : format with "%g" followed by 'd0'
* 'exp'    : exponential notation

```ruby
root = {group: {var1: 1.0, var2: 12.75, var3: 50.0e-8}}

puts FortIO::Namelist.dump(root, float_format: 'normal')
# =>
# &group
#   var1 = 1,
#   var2 = 12.75,
#   var3 = 5d-07
# /

puts FortIO::Namelist.dump(root, float_format: 'd0')
# =>
# &group
#   var1 = 1d0,
#   var2 = 12.75d0,
#   var3 = 5d-07
# /

puts FortIO::Namelist.dump(root, float_format: 'exp')
# =>
# &group
#   var1 = 1d+00,
#   var2 = 1.275d+01,
#   var3 = 5d-07
# /
```

#### `alignment` specifies how variable identifiers are aligned

* 'left'    : aligned, left-justified, position of '=' can be specified by number eg. 'left:7' (default)
* 'right'   : aligned, right-justified, position of '=' can be specified by number eg. 'right:7'
* 'none'    : not aligned
* 'stream'  : not aligned, stream style, length of each line can be specified by number eg. 'stream:70'

```ruby
root = {group: {var1: 1, variable2: [1,2,3], v3: true}}

puts FortIO::Namelist.dump(root, alignment: 'left')
# =>
# &group
#   var1      = 1,
#   variable2 = 1, 2, 3,
#   v3        = .true.
# /

puts FortIO::Namelist.dump(root, alignment: 'left:7')
# =>
# &group
#   var1  = 1,
#   variable2 = 1, 2, 3,
#   v3    = .true.
# /

puts FortIO::Namelist.dump(root, alignment: 'right')
# =>
# &group
#        var1 = 1,
#   variable2 = 1, 2, 3,
#          v3 = .true.
# /

puts FortIO::Namelist.dump(root, alignment: 'right:7')
# =>
# &group
#    var1 = 1,
#   variable2 = 1, 2, 3,
#      v3 = .true.
# /

puts FortIO::Namelist.dump(root, alignment: 'none')
# =>
# &group
#   var1 = 1,
#   variable2 = 1, 2, 3,
#   v3 = .true.
# /

puts FortIO::Namelist.dump(root, alignment: 'stream')
# =>
# &group
#   var1 = 1, variable2 = 1, 2, 3, v3 = .true.,
# /
```

#### `uppercase` specifies whether variable names, etc. should be uppercase or lowercase.

* false : (default)
* true  :  

```ruby
root = {group: {var1: 1, var2: "a"}}

puts FortIO::Namelist.dump(root, uppercase: false)
# =>
# &group
#   var1 = 1,
#   val2 = 'a'
# /

puts FortIO::Namelist.dump(root, uppercase: true)
# =>
# &GROUP
#   VAR1 = 1,
#   VAL2 = 'a'
# /
```

#### `separator` specifies the separator between variable definitions

* "comma", "," : comma + NL separeted (default)
* "nl", "\n"   : NL separated

```ruby
root = {group1: {var1: 1, var2: "a", var3: true},
        group2: {var1: 2, var2: "b", var3: false}}

puts FortIO::Namelist.dump(root, separator: "comma")
# =>
# &group1
#   var1 = 1,
#   var2 = 'a',
#   var3 = .true.
# /
# &group2
#   var1 = 2,
#   var2 = 'b',
#   var3 = .false.
# /

puts FortIO::Namelist.dump(root, separator: "nl")
# =>
# &group1
#   var1 = 1
#   var2 = 'a'
#   var3 = .true.
# /
# &group2
#   var1 = 2
#   var2 = 'b'
#   var3 = .false.
# /
```

#### `group_end` specifies the group terminator

* "slash", "/" : end with `/` (default)
* "end"        : end with `&end`

```ruby
root = {group: {var1: true, var2: false}}

puts FortIO::Namelist.dump(root, group_end: 'slash')
# =>
# &group
#   var1 = .true.,
#   var2 = .false.
# /

puts FortIO::Namelist.dump(root, group_end: 'end')
# =>
# &group
#   var1 = .true.,
#   var2 = .false.
# &end
```

#### `indent` specifies the indentation for variable definition
                    
* ' '*2 : two spaces (default)

```ruby
root = {group: {var1: true, var2: false}}

puts FortIO::Namelist.dump(root, indent: ' '*2)
# =>
# &group
#   var1 = .true.,
#   var2 = .false.
# /

puts FortIO::Namelist.dump(root, indent: ' '*4)
# =>
# &group
#     var1 = .true.,
#     var2 = .false.
# /
```