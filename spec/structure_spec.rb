require "fortio-namelist"
require "rspec-power_assert"

describe FortIO::Namelist do
  
  example "newline enumeration" do 
    input = %{
&example
  a = 1
  ab = 2
  abc = 3
/
    }
    nml = FortIO::Namelist.read(input)
    is_asserted_by { nml.has_key? "example"  }
    is_asserted_by { nml["example"].is_a? Hash  }
    is_asserted_by { nml["example"].keys.size == 3 }
    is_asserted_by { nml["example"].keys == ["a","ab","abc"] }
    is_asserted_by { nml["example"].values == [1,2,3] }
  end

  example "space enumeration" do 
    input = %{
&example  a = 1  ab = 2  abc = 3 /
    }
    nml = FortIO::Namelist.read(input)
    is_asserted_by { nml.has_key? "example"  }
    is_asserted_by { nml["example"].is_a? Hash  }
    is_asserted_by { nml["example"].keys.size == 3 }
    is_asserted_by { nml["example"].keys == ["a","ab","abc"] }
    is_asserted_by { nml["example"].values == [1,2,3] }
  end

  example "space enumeration 2" do 
    input = %{
&example
  a = 1  ab = 2  abc = 3
/
    }
    nml = FortIO::Namelist.read(input)
    is_asserted_by { nml.has_key? "example"  }
    is_asserted_by { nml["example"].is_a? Hash  }
    is_asserted_by { nml["example"].keys.size == 3 }
    is_asserted_by { nml["example"].keys == ["a","ab","abc"] }
    is_asserted_by { nml["example"].values == [1,2,3] }
  end

  example "comma enumeration" do 
    input = %{
&example  a = 1,  ab = 2,  abc = 3 /
    }
    nml = FortIO::Namelist.read(input)
    is_asserted_by { nml.has_key? "example"  }
    is_asserted_by { nml["example"].is_a? Hash  }
    is_asserted_by { nml["example"].keys.size == 3 }
    is_asserted_by { nml["example"].keys == ["a","ab","abc"] }
    is_asserted_by { nml["example"].values == [1,2,3] }
  end

  example "comma enumeration 2" do 
    input = %{
&example
  a = 1,  ab = 2,  abc = 3
/
    }
    nml = FortIO::Namelist.read(input)
    is_asserted_by { nml.has_key? "example"  }
    is_asserted_by { nml["example"].is_a? Hash  }
    is_asserted_by { nml["example"].keys.size == 3 }
    is_asserted_by { nml["example"].keys == ["a","ab","abc"] }
    is_asserted_by { nml["example"].values == [1,2,3] }
  end

  example "comma newline enumeration" do 
    input = %{
&example
  a = 1,
  ab = 2,
  abc = 3
/
}
    nml = FortIO::Namelist.read(input)
    is_asserted_by { nml.has_key? "example"  }
    is_asserted_by { nml["example"].is_a? Hash  }
    is_asserted_by { nml["example"].keys.size == 3 }
    is_asserted_by { nml["example"].keys == ["a","ab","abc"] }
    is_asserted_by { nml["example"].values == [1,2,3] }
  end

  example "last comma is permitted" do 
    input = %{
&example
  a = 1,
  ab = 2,
  abc = 3,
/
}
    nml = FortIO::Namelist.read(input)
    is_asserted_by { nml.has_key? "example"  }
    is_asserted_by { nml["example"].is_a? Hash  }
    is_asserted_by { nml["example"].keys.size == 3 }
    is_asserted_by { nml["example"].keys == ["a","ab","abc"] }
    is_asserted_by { nml["example"].values == [1,2,3] }
  end

  example "comma space hybrid" do 
    input = %{
&example
  a = 1 ab = 2,
  abc = 3
/
}
    nml = FortIO::Namelist.read(input)
    is_asserted_by { nml.has_key? "example"  }
    is_asserted_by { nml["example"].is_a? Hash  }
    is_asserted_by { nml["example"].keys.size == 3 }
    is_asserted_by { nml["example"].keys == ["a","ab","abc"] }
    is_asserted_by { nml["example"].values == [1,2,3] }
  end

  example "space before and after equal symbol" do 
    input = %{
&example
  a = 1
  ab= 2
  abc =3
/
}
    nml = FortIO::Namelist.read(input)
    is_asserted_by { nml.has_key? "example"  }
    is_asserted_by { nml["example"].is_a? Hash  }
    is_asserted_by { nml["example"].keys.size == 3 }
    is_asserted_by { nml["example"].keys == ["a","ab","abc"] }
    is_asserted_by { nml["example"].values == [1,2,3] }
  end


end