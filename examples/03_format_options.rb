#
#  03_format_options.rb - controlling the output format of dump
#
#     ruby 03_format_options.rb
#
require "fortio-namelist"

root = {
  config: {
    dt:        7.5,
    viscosity: 1.0e-6,
    restart:   false,
    levels:    [10, 20, 30],
    title:     "run A",
  },
}

def show (label, root, **format_options)
  puts "=== #{label} ==="
  puts FortIO::Namelist.dump(root, **format_options)
  puts
end

# how array elements are written
show "array_style: 'stream' (default)", root, array_style: "stream"
show "array_style: 'index'",            root, array_style: "index"

# how .true. / .false. are written
show "logical_format: 'normal' (default)", root, logical_format: "normal"
show "logical_format: 'short'",            root, logical_format: "short"

# how floating point numbers are written
show "float_format: 'normal' (default)", root, float_format: "normal"
show "float_format: 'd0'",               root, float_format: "d0"
show "float_format: 'exp'",              root, float_format: "exp"

# where the '=' signs go
show "alignment: 'left' (default)", root, alignment: "left"
show "alignment: 'right'",          root, alignment: "right"
show "alignment: 'none'",           root, alignment: "none"
show "alignment: 'stream:50'",      root, alignment: "stream:50"

# upper case names, '&end' terminator, wider indent, NL separator
show "uppercase: true",  root, uppercase: true
show "group_end: 'end'", root, group_end: "end"
show "indent: ' '*4",    root, indent: " " * 4
show "separator: 'nl'",  root, separator: "nl"

# options can of course be combined
show "combination", root,
     uppercase:      true,
     array_style:    "index",
     logical_format: "short",
     float_format:   "d0",
     group_end:      "end",
     indent:         " " * 4
