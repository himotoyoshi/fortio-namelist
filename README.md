
fortio-namelist
===============

This is a library for reading and writing data in Fortran's namelist format. With this library, you can read Fortran's namelist format data from Ruby and convert it to Hash objects, and vice versa. 

Features
--------

* Flexible parsing enables reading of namelists in various formats.
* Various options to control the output namelist string in the format of your choice.
* Able to convert namelist format to JSON or YAML format using Ruby's standard library

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

It is enough for the user to remember the following two methods.

    FortIO::Namelist.read(input, group: nil)
    FortIO::Namelist.dump(root, **format_options)

### Reading namelist format string

To create a Hash object with Namelist structure by reading a string in namelist format, use the following method.

    FortIO::Namelist.read(input, group: nil)

The argument `input` is given as a string, but it also accepts objects with a method `#read` returns a string like an IO object. 

If the keyword argument `group` is omitted, all namelist groups included in `input` will be read. To read only a specific group, give a group name to `group`. To load multiple groups, give an array of group names to `group`.

The Hash object of the return value has a two-level structure as follows.

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

Group names and variable names are given as String objects. The value is Ruby's String, Integer, Float, Complex, TrueClass, or FalseClass objects, depending on the literal in the namelist. In the case of an array, it will be an Array object with the above objects as elements.

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
=> {:group1=>{:var1=>11, :var2=>12},
    :group2=>{:var1=>12, :var2=>22},
    :group3=>{:var1=>31, :var2=>32}}


### read only "group2"
root = FortIO::Namelist.read(input, group: "group2")
=> {:group2=>{:var1=>12, :var2=>22}}


### read only "group1" and "group3"
root = FortIO::Namelist.read(input, group: ["group1", "group3"])
=> {:group1=>{:var1=>11, :var2=>12}, 
    :group3=>{:var1=>31, :var2=>32}}

```

### Generating namelist format string from Hash object with namelist structure

To generate a namelist format string from a Hash object with a namelist structure, use the following method.

    FortIO::Namelist.dump(root, **format_options)

The argument `root` is given as a Hash object. The return value is a string in namelist format. You can finely control the output namelist string with the following keyword arguments (the first one is the default).

    array_style:    'stream'
                    'index'   
                    
    logical_format: 'normal'
                    'short'

    float_format:   'normal'
                    'd0'

    alignment:      'left'
                    'right'
                    'compact'
                    Integer

    uppercase:      false
                    true
    
    separator:      "comma", ","
                    "nl", "\n"

    group_end:      "slash", "/"
                    "end"
                    
    indent:         ' '*2

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
