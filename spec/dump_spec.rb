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


  example "dumping float 'exp' keeps the mantissa normalized" do
    root = {example: {v1: 1.0e-6, v2: 5.0e-7, v3: 1.0, v4: 0.0, v5: -1.0e-6}}
    output = FortIO::Namelist.dump(root, float_format: 'exp')

    answer = <<HERE
&example
  v1 = 1d-06,
  v2 = 5d-07,
  v3 = 1d+00,
  v4 = 0d+00,
  v5 = -1d-06
/
HERE

    is_asserted_by { output == answer  }
  end

  example "float values survive a dump/parse round trip in every format" do
    values = [1.0, 12.75, 1.0e-6, 5.0e-7, 0.0, -1.0e-6, 1.0e20, 1.0/3,
              6.02214076e23, -0.1, 1.234567890123456e10,
              Float::MIN, Float::MAX]
    ['normal', 'd0', 'exp'].each do |float_format|
      values.each do |value|
        text = FortIO::Namelist.dump({example: {v: value}}, float_format: float_format)
        is_asserted_by { FortIO::Namelist.parse(text)[:example][:v] == value }
      end
    end
  end

end
