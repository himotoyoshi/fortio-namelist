fortio-namelist
===============

This is a library for reading and writing data in Fortran namelist format. With this library, you can read Fortran namelist format data from Ruby and convert it to Hash objects, and vice versa. Once the namelist format is read as a Hash object, it is easy to modify the contents or convert it to another format such as JSON, YAML, etc. When converting from a Hash object to namelist format, options can be specified to specify the case of variable names and the format of logical values and floating point numbers. The namelist format is often used as a configuration file to control a calculation program using Fortran. This library can be especially useful for batch processing by sequentially changing the settings of a calculation program. It can also improve the appearance of the namelist output from a Fortran program to make it easier to check. Since the namelist format and JSON (or YAML) can be converted to each other, it is also a good idea to use it to create web interfaces that facilitate the configuration of programs.

Installation
------------

    gem install fortio-namelist

To use the library in your Ruby script, 

```ruby
require "fortio-namelist"
```

Description
-----------

# Reading namelist data 

    FortIO::Namelist.read(input, name: nil)

# Generate namelist format from Hash object

    FortIO::Namelist.generate(hash, name: "namelist", array_format: 'stream')

# Generate namelist format string from Hash containing multiple datasets

    FortIO::Namelist.dump(root, **format_options)

# Reads namelist data, evaluates it with block, and converts the result to namelist

    FortIO::Namelist.filter(input, **format_options) { |hash| ... }

# Format options accepted by generate, dump, and filter

    array_style:    'index'   
                    'stream'

    logical_format: 'normal'
                    'short'

    float_format:   'normal'
                    'd0'

    alignment:      'compact'
                    'left'
                    'right'
                    Integer

    uppercase:      false
                    true
    
    comma:          false
                    true
                    
    slash:          true
                    false
                    
    indent:         ' '*2
                    