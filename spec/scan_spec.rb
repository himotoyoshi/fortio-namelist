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
    is_asserted_by { result[0][:variables].keys == [:visc, :diff] }
    is_asserted_by { result[0][:variables][:visc] == 2 }
    is_asserted_by { result[0][:variables][:diff] == 3 }
    is_asserted_by { result[0][:lines].is_a? Range }
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
    is_asserted_by { result[0][:variables].keys == [:a] }
    is_asserted_by { result[1][:variables].keys == [:b] }
    is_asserted_by { result[2][:variables].keys == [:c] }
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
    is_asserted_by { result[0][:variables].keys == [:x] }
    is_asserted_by { result[1][:variables].keys == [:y] }
  end

  example "duplicate variable names keep first occurrence line" do
    input = <<~NML
      &example
        v1 = 1
        v1 = 2
        v2 = 3
      /
    NML
    result = FortIO::Namelist.scan(input)
    is_asserted_by { result[0][:variables].keys == [:v1, :v2] }
    is_asserted_by { result[0][:variables][:v1] == 2 }
    is_asserted_by { result[0][:variables][:v2] == 4 }
  end

  example "empty group has no variables" do
    input = "&example /\n"
    result = FortIO::Namelist.scan(input)
    is_asserted_by { result.size == 1 }
    is_asserted_by { result[0][:group] == :example }
    is_asserted_by { result[0][:variables] == {} }
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
    is_asserted_by { result[0][:variables] == {val: 2} }
  end

  example "group name is lowercased" do
    input = "&MyGroup\n  VAR1 = 1\n/\n"
    result = FortIO::Namelist.scan(input)
    is_asserted_by { result[0][:group] == :mygroup }
    is_asserted_by { result[0][:variables].keys == [:var1] }
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
    is_asserted_by { result[0][:variables].keys == [:visc, :diff] }
    is_asserted_by { result[0][:variables][:visc] == 4 }
    is_asserted_by { result[0][:variables][:diff] == 5 }
    is_asserted_by { result[0][:lines].first == 2 }
  end

  example "dollar prefix style" do
    input = "$example\n  v1 = 100\n$end\n"
    result = FortIO::Namelist.scan(input)
    is_asserted_by { result.size == 1 }
    is_asserted_by { result[0][:group] == :example }
    is_asserted_by { result[0][:variables] == {v1: 2} }
  end

  example "array variables record first occurrence only" do
    input = <<~NML
      &config
        arr(1) = 10
        arr(2) = 20
        scalar = 1
      /
    NML
    result = FortIO::Namelist.scan(input)
    is_asserted_by { result[0][:variables].keys == [:arr, :scalar] }
    is_asserted_by { result[0][:variables][:arr] == 2 }
    is_asserted_by { result[0][:variables][:scalar] == 4 }
  end

end
