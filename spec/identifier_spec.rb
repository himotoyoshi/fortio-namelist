require "fortio-namelist"
require "rspec-power_assert"

describe "FortIO::Namelist" do
  
  example "only letter" do 
    input = %{
&example
  a = 1
  ab = 1
  abc = 1
/
    }
    nml = FortIO::Namelist.parse(input)
    is_asserted_by { nml.has_key? :example  }
    is_asserted_by { nml[:example].is_a? Hash  }
    is_asserted_by { nml[:example].keys.size == 3 }
    is_asserted_by { nml[:example].keys == [:a,:ab,:abc] }
  end

  example "letter number" do 
    input = %{
&example
  a1 = 1
  ab12 = 1
  abc123 = 1
/
    }
    nml = FortIO::Namelist.parse(input)
    is_asserted_by { nml.has_key? :example  }
    is_asserted_by { nml[:example].is_a? Hash  }
    is_asserted_by { nml[:example].keys.size == 3 }
    is_asserted_by { nml[:example].keys == [:a1, :ab12, :abc123] }
  end

  example "leter number underbar" do 
    input = %{
&example
  a_1 = 1
  ab_12 = 1
  abc_123 = 1
/
    }
    nml = FortIO::Namelist.parse(input)
    is_asserted_by { nml.has_key? :example  }
    is_asserted_by { nml[:example].is_a? Hash  }
    is_asserted_by { nml[:example].keys.size == 3 }
    is_asserted_by { nml[:example].keys == [:a_1, :ab_12, :abc_123] }
  end

  example "can't start with number" do 
    input = %{
&example
  1a = 1
/
    }
    expect { FortIO::Namelist.parse(input) }.to raise_error(RuntimeError)
  end

  example "can't start with underscore" do 
    input = %{
&example
  _a = 1
/
    }
    expect { FortIO::Namelist.parse(input) }.to raise_error(RuntimeError)
  end

  example "t and f can be used as indentifier" do 
    input = %{
&example
  t = 1
  f = 2
/
    }
    nml = FortIO::Namelist.parse(input)
    is_asserted_by { nml[:example][:t] == 1 }
    is_asserted_by { nml[:example][:f] == 2 }
  end

end