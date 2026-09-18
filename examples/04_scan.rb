#
#  04_scan.rb - looking at the structure of a namelist without
#               caring about the values
#
#     ruby 04_scan.rb
#
require "fortio-namelist"
require "pp"

input = File.read(File.join(__dir__, "sample.nml"))

# ---------------------------------------------------------------
#  FortIO::Namelist.scan returns one entry per group:
#
#    { group:     :name,
#      lines:     first..last,        # 1-based line range
#      variables: [ { name: "DT", lineno: 3 },            # scalar
#                   { name: "ARR", lineno: 5, index: 1 }  # array element
#                 ] }
#
#  Variable names keep their original case here, which makes scan
#  handy for reporting back to the user about the original file.
# ---------------------------------------------------------------

result = FortIO::Namelist.scan(input)

puts "=== raw scan result ==="
pp result
puts

puts "=== as a table of contents ==="
result.each do |entry|
  printf "&%-8s  lines %3d..%-3d (%d variables)\n",
         entry[:group], entry[:lines].first, entry[:lines].last,
         entry[:variables].size
  entry[:variables].each do |var|
    if var[:index]
      printf "    %-10s line %3d  index %s\n", var[:name], var[:lineno], var[:index]
    else
      printf "    %-10s line %3d\n", var[:name], var[:lineno]
    end
  end
end
puts

# ---------------------------------------------------------------
#  A typical use: tell the user where a setting was defined.
# ---------------------------------------------------------------

wanted = "INTERVAL"

result.each do |entry|
  entry[:variables].each do |var|
    if var[:name].downcase == wanted.downcase
      puts "#{wanted} is set in &#{entry[:group]} at line #{var[:lineno]}"
    end
  end
end
