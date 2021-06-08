require "fortio-namelist"
require "rspec-power_assert"

describe "FortIO::Namelist" do
  
  example "dumping float" do 
    root = {example: {v1: 1.234567890123456, v2: 0.123456789012345, v3: 1.234567890123456e10}}
    output = FortIO::Namelist.dump(root)

    answer = <<HERE
&example
  v1 = 1.234567890123456,
  v2 = 0.123456789012345,
  v3 = 12345678901.23456
/
HERE

    is_asserted_by { output == answer  }
  end

  example "dumping float 'd0'" do 
    root = {example: {v1: 1.234567890123456, v2: 0.123456789012345, v3: 1.234567890123456e10}}
    output = FortIO::Namelist.dump(root, float_format: 'd0')

    answer = <<HERE
&example
  v1 = 1.234567890123456d0,
  v2 = 0.123456789012345d0,
  v3 = 12345678901.23456d0
/
HERE

    is_asserted_by { output == answer  }
  end

  example "dumping float 'exp'" do 
    root = {example: {v1: 1.234567890123456, v2: 0.123456789012345, v3: 1.234567890123456e10}}
    output = FortIO::Namelist.dump(root, float_format: 'exp')

    answer = <<HERE
&example
  v1 = 1.234567890123456d+00,
  v2 = 1.23456789012345d-01,
  v3 = 1.234567890123456d+10
/
HERE

    is_asserted_by { output == answer  }
  end

end