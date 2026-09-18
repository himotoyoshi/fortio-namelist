#
#  01_read.rb - reading a namelist file into a Hash
#
#     ruby 01_read.rb
#
require "fortio-namelist"
require "pp"

input = File.read(File.join(__dir__, "sample.nml"))

# ---------------------------------------------------------------
#  Read every group in the file.
#
#  The result is a two-level Hash:
#    { group_name => { variable_name => value, ... }, ... }
#
#  Group and variable names become lowercase Symbols, values become
#  ordinary Ruby objects (String, Integer, Float, true/false, Array).
# ---------------------------------------------------------------

root = FortIO::Namelist.parse(input)

puts "=== all groups ==="
pp root
puts

puts "=== picking values out of the Hash ==="
printf "title     : %s\n",   root[:model][:title]
printf "nx, ny    : %d x %d\n", root[:model][:nx], root[:model][:ny]
printf "dt        : %s (%s)\n", root[:model][:dt], root[:model][:dt].class
printf "verbose   : %s (%s)\n", root[:model][:verbose], root[:model][:verbose].class
printf "levels    : %s\n",   root[:grid][:levels].inspect
printf "labels[1] : %s\n",   root[:grid][:labels][1]
puts

# ---------------------------------------------------------------
#  Read only the group(s) you care about.
#  Group names may be given as a String, a Symbol, or an Array.
# ---------------------------------------------------------------

puts "=== only &grid ==="
pp FortIO::Namelist.parse(input, group: "grid")
puts

puts "=== only &model and &output ==="
pp FortIO::Namelist.parse(input, group: ["model", "output"])
puts

# ---------------------------------------------------------------
#  Anything that responds to #read works as well, so an IO object
#  can be handed over directly.
# ---------------------------------------------------------------

puts "=== reading from an IO object ==="
File.open(File.join(__dir__, "sample.nml")) do |file|
  pp FortIO::Namelist.parse(file, group: :output)
end
