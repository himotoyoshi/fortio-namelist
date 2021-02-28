require "fortio-namelist"
require "rspec-power_assert"

describe FortIO::Namelist do
  
  example "array stream" do 
    input = %{
&example
  v1 = 1,2,3,4,5,
  v2 = 6,7,8,9,10,
/
    }
    nml = FortIO::Namelist.read(input)
    is_asserted_by { nml.has_key? "example"  }
    is_asserted_by { nml["example"].is_a? Hash  }
    is_asserted_by { nml["example"]["v1"] = [1,2,3,4,5]  }
    is_asserted_by { nml["example"]["v2"] = [6,7,8,9,10]  }
  end

  example "array index" do 
    input = %{
&example
  v1(1) = 1
  v1(2) = 2
  v1(3) = 3
  v1(4) = 4
  v1(5) = 5
  v2(1:2) = 6,7
  v2(3:5) = 8,9,10,
/
    }
    nml = FortIO::Namelist.read(input)
    is_asserted_by { nml.has_key? "example"  }
    is_asserted_by { nml["example"].is_a? Hash  }
    is_asserted_by { nml["example"]["v1"] = [1,2,3,4,5]  }
    is_asserted_by { nml["example"]["v2"] = [6,7,8,9,10]  }
  end

  example "array partial" do 
    input = %{
&example
  v1(2) = 2
  v1(5) = 5
  v2(3:5) = 8,9,10,
/
    }
    nml = FortIO::Namelist.read(input)
    is_asserted_by { nml.has_key? "example"  }
    is_asserted_by { nml["example"].is_a? Hash  }
    is_asserted_by { nml["example"]["v1"] = [nil,2,nil,nil,5]  }
    is_asserted_by { nml["example"]["v2"] = [nil,nil,8,9,10]  }
  end

  example "array extend" do 
    input = %{
&example
  v1(2) = 2,3,4,5
/
    }
    nml = FortIO::Namelist.read(input)
    is_asserted_by { nml.has_key? "example"  }
    is_asserted_by { nml["example"].is_a? Hash  }
    is_asserted_by { nml["example"]["v1"] = [nil,2,3,4,5]  }
  end

  example "array reputation" do 
    input = %{
&example
  v1(1:5) = 2,4*2
  v2(2) = 4*2
  v3 = 5*.true.
/
    }
    nml = FortIO::Namelist.read(input)
    is_asserted_by { nml.has_key? "example"  }
    is_asserted_by { nml["example"].is_a? Hash  }
    is_asserted_by { nml["example"]["v1"] = [2,2,2,2,2]  }
    is_asserted_by { nml["example"]["v2"] = [nil,2,2,2,2]  }
    is_asserted_by { nml["example"]["v3"] = [true]*5  }
  end

  example "array of string" do 
    input = %{
&example
  v1 = "a",'b',"c"
/
    }
    nml = FortIO::Namelist.read(input)
    is_asserted_by { nml.has_key? "example"  }
    is_asserted_by { nml["example"].is_a? Hash  }
    is_asserted_by { nml["example"]["v1"] = ["a","b","c"]  }
  end

  example "don't permit identifier string in array stream" do 
    input = %{
&example
  v1 = "a", b, "c"
/
    }
    expect { FortIO::Namelist.read(input) }.to raise_error(RuntimeError)
  end

  example "empty element" do 
    input = %{
&example
  v1 = , , 3, , 5,
  v2 = , , 3, , 5
/
    }
    nml = FortIO::Namelist.read(input)
    is_asserted_by { nml.has_key? "example"  }
    is_asserted_by { nml["example"].is_a? Hash  }
    is_asserted_by { nml["example"]["v1"] = [nil, nil, 3, nil, 5]  }
    is_asserted_by { nml["example"]["v2"] = [nil, nil, 3, nil, 5]  }
  end


end