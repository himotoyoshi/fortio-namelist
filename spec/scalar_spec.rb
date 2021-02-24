require "fortio-namelist"
require "rspec-power_assert"

describe FortIO::Namelist do
  
  example "integer" do 
    input = %{
&example
  v1 = 1
  v2 = -1
  v3 = 01
  v4 = -01
/
    }
    nml = FortIO::Namelist.read(input)
    is_asserted_by { nml.has_key? "example"  }
    is_asserted_by { nml["example"].is_a? Hash  }
    is_asserted_by { nml["example"]["v1"] == 1  }
    is_asserted_by { nml["example"]["v2"] == -1  }
    is_asserted_by { nml["example"]["v3"] == 1  }
    is_asserted_by { nml["example"]["v4"] == -1  }
  end

  example "float positive" do 
    input = %{
&example
  v1 = 1.0
  v2 = 2.
  v3 = 3.0d0
  v4 = 4.d0
  v5 = 5d0
  v6 = 6.0d+0
  v7 = 7.0d+00
  v8 = 80.0d-1
  v9 = 90.0d-01
/
    }
    nml = FortIO::Namelist.read(input)
    is_asserted_by { nml.has_key? "example"  }
    is_asserted_by { nml["example"].is_a? Hash  }
    is_asserted_by { nml["example"]["v1"] == 1.0  }
    is_asserted_by { nml["example"]["v2"] == 2.0 }
    is_asserted_by { nml["example"]["v3"] == 3.0  }
    is_asserted_by { nml["example"]["v4"] == 4.0 }
    is_asserted_by { nml["example"]["v5"] == 5.0  }
    is_asserted_by { nml["example"]["v6"] == 6.0 }
    is_asserted_by { nml["example"]["v7"] == 7.0  }
    is_asserted_by { nml["example"]["v8"] == 8.0 }
    is_asserted_by { nml["example"]["v9"] == 9.0  }
  end

  example "float negative" do 
    input = %{
&example
  v1 = -1.0
  v2 = -2.
  v3 = -3.0d0
  v4 = -4.d0
  v5 = -5d0
  v6 = -6.0d+0
  v7 = -7.0d+00
  v8 = -80.0d-1
  v9 = -90.0d-01
/
    }
    nml = FortIO::Namelist.read(input)
    is_asserted_by { nml.has_key? "example"  }
    is_asserted_by { nml["example"].is_a? Hash  }
    is_asserted_by { nml["example"]["v1"] == -1.0  }
    is_asserted_by { nml["example"]["v2"] == -2.0 }
    is_asserted_by { nml["example"]["v3"] == -3.0  }
    is_asserted_by { nml["example"]["v4"] == -4.0 }
    is_asserted_by { nml["example"]["v5"] == -5.0  }
    is_asserted_by { nml["example"]["v6"] == -6.0 }
    is_asserted_by { nml["example"]["v7"] == -7.0  }
    is_asserted_by { nml["example"]["v8"] == -8.0 }
    is_asserted_by { nml["example"]["v9"] == -9.0  }
  end

  example "float 0 start" do 
    input = %{
&example
  v1 = 01.0
/
    }
    nml = FortIO::Namelist.read(input)
    is_asserted_by { nml.has_key? "example"  }
    is_asserted_by { nml["example"].is_a? Hash  }
    is_asserted_by { nml["example"]["v1"] == 1.0  }
  end

  example "string" do 
    input = %{
&example
  v1 = 'string'
  v2 = "string"
  v3 = string
/
    }
    nml = FortIO::Namelist.read(input)
    is_asserted_by { nml.has_key? "example"  }
    is_asserted_by { nml["example"].is_a? Hash  }
    is_asserted_by { nml["example"]["v1"] == "string"  }
    is_asserted_by { nml["example"]["v2"] == "string"  }
    is_asserted_by { nml["example"]["v3"] == "string"  }
  end

  example "no quotation mark" do 
    input = %{
&example
  v1 = string
  v2 = _string
  v3 = 0_string
/
    }
    nml = FortIO::Namelist.read(input)
    is_asserted_by { nml.has_key? "example"  }
    is_asserted_by { nml["example"].is_a? Hash  }
    is_asserted_by { nml["example"]["v1"] == "string"  }
    is_asserted_by { nml["example"]["v2"] == "_string"  }
    is_asserted_by { nml["example"]["v3"] == "0_string"  }
  end

  example "logical true" do 
    input = %{
&example
  v1 = .true.
  v2 = .TRUE.
  v3 = .t.
  v4 = .taaa.
  v5 = t
  v6 = T
/
    }
    nml = FortIO::Namelist.read(input)
    is_asserted_by { nml.has_key? "example"  }
    is_asserted_by { nml["example"].is_a? Hash  }
    is_asserted_by { nml["example"]["v1"] == true  }
    is_asserted_by { nml["example"]["v2"] == true  }
    is_asserted_by { nml["example"]["v3"] == true  }
    is_asserted_by { nml["example"]["v4"] == true  }
    is_asserted_by { nml["example"]["v5"] == true  }
    is_asserted_by { nml["example"]["v6"] == true  }
  end

  example "logical false" do 
    input = %{
&example
  v1 = .false.
  v2 = .FALSE.
  v3 = .f.
  v4 = .faaa.
  v5 = f
  v6 = F
/
    }
    nml = FortIO::Namelist.read(input)
    is_asserted_by { nml.has_key? "example"  }
    is_asserted_by { nml["example"].is_a? Hash  }
    is_asserted_by { nml["example"]["v1"] == false  }
    is_asserted_by { nml["example"]["v2"] == false  }
    is_asserted_by { nml["example"]["v3"] == false  }
    is_asserted_by { nml["example"]["v4"] == false  }
    is_asserted_by { nml["example"]["v5"] == false  }
    is_asserted_by { nml["example"]["v6"] == false  }
  end

  example "complex" do 
    input = %{
&example
  v1 = (1,1)
  v2 = (1d0,1d0)
/
    }
    nml = FortIO::Namelist.read(input)
    is_asserted_by { nml.has_key? "example"  }
    is_asserted_by { nml["example"].is_a? Hash  }
    is_asserted_by { nml["example"]["v1"] == 1+1i  }
    is_asserted_by { nml["example"]["v2"] == 1+1i  }
  end

end