require "fortio-namelist"
require "rspec-power_assert"

describe FortIO::Namelist do
  
  example "namelist prefix &" do 
    input = %{
&example
&end
    }
    nml = FortIO::Namelist.read(input)
    is_asserted_by { nml.has_key? "example"  }
    is_asserted_by { nml["example"].is_a? Hash  }
    is_asserted_by { nml["example"].empty?  }
  end

  example "namelist prefix $" do 
    input = %{
$example
$end
    }
    nml = FortIO::Namelist.read(input)
    is_asserted_by { nml.has_key? "example"  }
  end

  example "end with /" do 
    input = %{
&example
/
    }
    nml = FortIO::Namelist.read(input)
    is_asserted_by { nml.has_key? "example"  }
  end

  example "only comment" do 
    input = %{
&example
  ! this is comment
/
    }
    nml = FortIO::Namelist.read(input)
    is_asserted_by { nml.has_key? "example"  }
  end

  example "only newlines" do 
    input = %{
&example


/
    }
    nml = FortIO::Namelist.read(input)
    is_asserted_by { nml.has_key? "example"  }
  end

  example "one line" do 
    input = %{
&example /
    }
    nml = FortIO::Namelist.read(input)
    is_asserted_by { nml.has_key? "example"  }
  end
  
  
end