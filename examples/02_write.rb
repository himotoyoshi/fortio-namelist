#
#  02_write.rb - building a namelist string from a Hash
#
#     ruby 02_write.rb
#
require "fortio-namelist"
require "tmpdir"

# ---------------------------------------------------------------
#  A Hash with the namelist structure
#    { group_name => { variable_name => value, ... }, ... }
#  is all that FortIO::Namelist.dump needs.
#
#  Values may be String, Integer, Float, Complex, true/false,
#  or an Array of those.
# ---------------------------------------------------------------

root = {
  model: {
    title:     "Shallow water test run",
    nx:        128,
    ny:        64,
    dt:        7.5,
    viscosity: 1.0e-6,
    restart:   false,
    verbose:   true,
  },
  grid: {
    levels:  [10, 20, 30, 40, 50],
    spacing: [1000.0, 2000.0, 4000.0],
    labels:  ["top", "middle", "bottom"],
  },
}

puts "=== dump (default format) ==="
puts FortIO::Namelist.dump(root)

# ---------------------------------------------------------------
#  The return value is just a String, so writing it out is
#  ordinary Ruby file I/O.
#
#  This example writes into a temporary directory, so that it runs
#  even from a read-only place such as an installed gem's directory.
# ---------------------------------------------------------------

path = File.join(Dir.tmpdir, "written.nml")
File.write(path, FortIO::Namelist.dump(root))
puts "=== wrote #{path} ==="
puts

begin
  # -------------------------------------------------------------
  #  Round trip: read back what we have just written and make sure
  #  it gives the same Hash.
  # -------------------------------------------------------------

  again = FortIO::Namelist.parse(File.read(path))

  puts "=== round trip ==="
  puts "same Hash? -> #{again == root}"
ensure
  File.unlink(path)
end
