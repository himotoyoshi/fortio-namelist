#
#  05_filter.rb - editing an existing namelist
#
#     ruby 05_filter.rb
#
require "fortio-namelist"

input = File.read(File.join(__dir__, "sample.nml"))

# ---------------------------------------------------------------
#  FortIO::Namelist.filter does parse -> yield -> dump in one go.
#  Modify the Hash inside the block; the return value is the
#  resulting namelist string.
# ---------------------------------------------------------------

output = FortIO::Namelist.filter(input, uppercase: true) do |root|
  root[:model][:nx]      = 256          # change a value
  root[:model][:ny]      = 128
  root[:model][:restart] = true
  root[:model][:comment] = "resumed"    # add a new variable
  root[:grid][:levels] << 60            # extend an array
  root[:output].delete(:interval)       # remove a variable
end

puts "=== filtered namelist ==="
puts output

# ---------------------------------------------------------------
#  The same thing written out by hand, in case you need to do
#  something between the reading and the writing.
# ---------------------------------------------------------------

root = FortIO::Namelist.parse(input)
root[:model][:title] = root[:model][:title] + " (rev 2)"
same = FortIO::Namelist.dump(root, uppercase: false)

puts "=== parse + dump ==="
puts same
