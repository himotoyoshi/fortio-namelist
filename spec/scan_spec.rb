require "fortio-namelist"
require "rspec-power_assert"

describe "FortIO::Namelist.scan" do

  example "single group" do
    input = <<~NML
      &physics
        visc = 1e-4
        diff = 2e-5
      /
    NML
    result = FortIO::Namelist.scan(input)
    is_asserted_by { result.size == 1 }
    is_asserted_by { result[0][:group] == :physics }
    is_asserted_by { result[0][:variables].size == 2 }
    is_asserted_by { result[0][:variables][0] == { name: "visc", lineno: 2 } }
    is_asserted_by { result[0][:variables][1] == { name: "diff", lineno: 3 } }
    is_asserted_by { result[0][:lines].first == 1 }
    is_asserted_by { result[0][:lines].last == 4 }
  end

  example "multiple groups in order" do
    input = <<~NML
      &group1
        a = 1
      /
      &group2
        b = 2
      /
      &group3
        c = 3
      /
    NML
    result = FortIO::Namelist.scan(input)
    is_asserted_by { result.size == 3 }
    is_asserted_by { result[0][:group] == :group1 }
    is_asserted_by { result[1][:group] == :group2 }
    is_asserted_by { result[2][:group] == :group3 }
    is_asserted_by { result[0][:variables][0][:name] == "a" }
    is_asserted_by { result[1][:variables][0][:name] == "b" }
    is_asserted_by { result[2][:variables][0][:name] == "c" }
  end

  example "duplicate group names are both returned" do
    input = <<~NML
      &grp
        x = 1
      /
      &grp
        y = 2
      /
    NML
    result = FortIO::Namelist.scan(input)
    is_asserted_by { result.size == 2 }
    is_asserted_by { result[0][:group] == :grp }
    is_asserted_by { result[1][:group] == :grp }
    is_asserted_by { result[0][:variables][0][:name] == "x" }
    is_asserted_by { result[1][:variables][0][:name] == "y" }
  end

  example "duplicate variable names appear multiple times" do
    input = <<~NML
      &example
        v1 = 1
        v1 = 2
        v2 = 3
      /
    NML
    result = FortIO::Namelist.scan(input)
    is_asserted_by { result[0][:variables].size == 3 }
    is_asserted_by { result[0][:variables][0] == { name: "v1", lineno: 2 } }
    is_asserted_by { result[0][:variables][1] == { name: "v1", lineno: 3 } }
    is_asserted_by { result[0][:variables][2] == { name: "v2", lineno: 4 } }
  end

  example "empty group has no variables" do
    input = "&example /\n"
    result = FortIO::Namelist.scan(input)
    is_asserted_by { result.size == 1 }
    is_asserted_by { result[0][:group] == :example }
    is_asserted_by { result[0][:variables] == [] }
  end

  example "empty input returns empty array" do
    result = FortIO::Namelist.scan("")
    is_asserted_by { result == [] }
  end

  example "IO-like input" do
    input = StringIO.new("&grp\n  val = 42\n/\n")
    result = FortIO::Namelist.scan(input)
    is_asserted_by { result.size == 1 }
    is_asserted_by { result[0][:group] == :grp }
    is_asserted_by { result[0][:variables] == [{ name: "val", lineno: 2 }] }
  end

  example "preserves original variable name case" do
    input = "&MyGroup\n  VAR1 = 1\n  Var2 = 2\n/\n"
    result = FortIO::Namelist.scan(input)
    is_asserted_by { result[0][:group] == :mygroup }
    is_asserted_by { result[0][:variables][0][:name] == "VAR1" }
    is_asserted_by { result[0][:variables][1][:name] == "Var2" }
  end

  example "line ranges do not overlap" do
    input = <<~NML
      &first
        a = 1
      /
      &second
        b = 2
      /
    NML
    result = FortIO::Namelist.scan(input)
    is_asserted_by { result[0][:lines].last < result[1][:lines].first }
  end

  example "with comments and blank lines" do
    input = <<~NML
      ! header comment
      &physics
        ! inline
        visc = 1e-4
        diff = 2e-5
      /
    NML
    result = FortIO::Namelist.scan(input)
    is_asserted_by { result.size == 1 }
    is_asserted_by { result[0][:group] == :physics }
    is_asserted_by { result[0][:variables][0] == { name: "visc", lineno: 4 } }
    is_asserted_by { result[0][:variables][1] == { name: "diff", lineno: 5 } }
    is_asserted_by { result[0][:lines].first == 2 }
  end

  example "dollar prefix style" do
    input = "$example\n  v1 = 100\n$end\n"
    result = FortIO::Namelist.scan(input)
    is_asserted_by { result.size == 1 }
    is_asserted_by { result[0][:group] == :example }
    is_asserted_by { result[0][:variables] == [{ name: "v1", lineno: 2 }] }
  end

  example "array variables have index, scalars do not" do
    input = <<~NML
      &config
        arr(1) = 10
        arr(2) = 20
        scalar = 1
      /
    NML
    result = FortIO::Namelist.scan(input)
    is_asserted_by { result[0][:variables].size == 3 }
    is_asserted_by { result[0][:variables][0] == { name: "arr", lineno: 2, index: 1 } }
    is_asserted_by { result[0][:variables][1] == { name: "arr", lineno: 3, index: 2 } }
    is_asserted_by { result[0][:variables][2] == { name: "scalar", lineno: 4 } }
  end

  example "array range index" do
    input = <<~NML
      &config
        arr(3:5) = 8, 9, 10
      /
    NML
    result = FortIO::Namelist.scan(input)
    is_asserted_by { result[0][:variables][0] == { name: "arr", lineno: 2, index: "3:5" } }
  end

end
