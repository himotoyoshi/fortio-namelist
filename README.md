
fortio-namelist
===============

This is a library for reading and writing data in Fortran's namelist format. With this library, you can read Fortran's namelist format data from Ruby and convert it to Hash objects, and vice versa. 

Features
--------

* Flexible parsing using Racc to support various dialects
* Options for controlling the format of the dump
* Represents the structure of a name list with a Hash object (easy to convert to JSON or YAML format)

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
root = FortIO::Namelist.read(input)
# => {:group1=>{:var1=>11, :var2=>12},
#     :group2=>{:var1=>12, :var2=>22},
#     :group3=>{:var1=>31, :var2=>32}}

### read only "group2"
root = FortIO::Namelist.read(input, group: "group2")
# => {:group2=>{:var1=>12, :var2=>22}}

### read only "group1" and "group3"
root = FortIO::Namelist.read(input, group: ["group1", "group3"])
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

#### `array_style`

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

#### `logical_format`

* 'normal' : (default)
* 'short'

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
#   var1 = .true.,
#   var2 = .false.
# /
```

#### `float_format`

* 'normal'
* 'd0'
* 'exp'

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

#### `alignment`

* 'left'    : eg. 'left:7' (default)
* 'right'   : eg. 'right:7'
* 'none'    : 
* 'stream'  : eg. 'stream:70'

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

#### `uppercase`

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

#### `separator`

* "comma", ","
* "nl", "\n"

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

#### `group_end`

* "slash", "/" : 
* "end"        :

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

#### `indent`
                    
* ' '*2

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