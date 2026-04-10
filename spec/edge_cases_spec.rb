require "fortio-namelist"
require "rspec-power_assert"

describe "FortIO::Namelist edge cases" do

  # --- パースエラー系 ---

  context "parse errors" do

    example "missing group name" do
      input = "& \n  v1 = 1\n/\n"
      expect { FortIO::Namelist.parse(input) }.to raise_error(RuntimeError)
    end

    example "no closing slash or &end" do
      input = "&example\n  v1 = 1\n"
      expect { FortIO::Namelist.parse(input) }.to raise_error(RuntimeError)
    end

    example "invalid group terminator like &foo" do
      input = "&example\n  v1 = 1\n&foo\n"
      expect { FortIO::Namelist.parse(input) }.to raise_error(RuntimeError)
    end

    example "value without variable name" do
      input = "&example\n  = 1\n/\n"
      expect { FortIO::Namelist.parse(input) }.to raise_error(RuntimeError)
    end

    example "completely empty string returns empty hash" do
      nml = FortIO::Namelist.parse("")
      is_asserted_by { nml == {} }
    end

    example "only whitespace and comments returns empty hash" do
      nml = FortIO::Namelist.parse("  \n! just a comment\n  \n")
      is_asserted_by { nml == {} }
    end

    example "unmatched single quote in string" do
      input = "&example\n  v1 = 'hello\n/\n"
      expect { FortIO::Namelist.parse(input) }.to raise_error(RuntimeError)
    end

    example "unmatched double quote in string" do
      input = "&example\n  v1 = \"hello\n/\n"
      expect { FortIO::Namelist.parse(input) }.to raise_error(RuntimeError)
    end

  end

  # --- 存在しないグループの指定 ---

  context "non-existent group" do

    example "requesting a group that does not exist" do
      input = "&group1\n  v1 = 1\n/\n"
      expect { FortIO::Namelist.parse(input, group: "nonexistent") }.to raise_error(RuntimeError, /no definition/)
    end

    example "requesting one valid and one invalid group" do
      input = "&group1\n  v1 = 1\n/\n"
      expect { FortIO::Namelist.parse(input, group: ["group1", "nope"]) }.to raise_error(RuntimeError, /no definition/)
    end

  end

  # --- 重複キー ---

  context "duplicate variable names" do

    example "later assignment overwrites earlier one" do
      input = "&example\n  v1 = 1\n  v1 = 999\n/\n"
      nml = FortIO::Namelist.parse(input)
      is_asserted_by { nml[:example][:v1] == 999 }
    end

    example "duplicate group names are both accessible" do
      input = "&grp\n  a = 1\n/\n&grp\n  a = 2\n/\n"
      nml = FortIO::Namelist.parse(input)
      # 後のグループが上書きするかマージされるか確認
      is_asserted_by { nml[:grp].is_a? Hash }
      is_asserted_by { nml[:grp][:a] == 2 }
    end

  end

  # --- group名のケース変換 ---

  context "group name case insensitivity" do

    example "group name is lowercased" do
      input = "&MyGroup\n  val = 42\n/\n"
      nml = FortIO::Namelist.parse(input)
      is_asserted_by { nml.has_key?(:mygroup) }
      is_asserted_by { nml[:mygroup][:val] == 42 }
    end

    example "group option with uppercase is downcased" do
      input = "&HELLO\n  x = 1\n/\n"
      nml = FortIO::Namelist.parse(input, group: "HELLO")
      is_asserted_by { nml.has_key?(:hello) }
      is_asserted_by { nml[:hello][:x] == 1 }
    end

    example "group option with lowercase works" do
      input = "&HELLO\n  x = 1\n/\n"
      nml = FortIO::Namelist.parse(input, group: "hello")
      is_asserted_by { nml.has_key?(:hello) }
      is_asserted_by { nml[:hello][:x] == 1 }
    end

  end

  # --- 変数名のケース変換 ---

  context "variable name case insensitivity" do

    example "variable names are lowercased" do
      input = "&example\n  MyVar = 10\n  ALLCAPS = 20\n/\n"
      nml = FortIO::Namelist.parse(input)
      is_asserted_by { nml[:example].has_key?(:myvar) }
      is_asserted_by { nml[:example].has_key?(:allcaps) }
    end

  end

  # --- dump のオプション検証エラー ---

  context "dump option validation" do

    let(:root) { {group: {v1: 1}} }

    example "invalid array_style raises error" do
      expect { FortIO::Namelist.dump(root, array_style: "invalid") }.to raise_error(RuntimeError, /array_style/)
    end

    example "invalid separator raises error" do
      expect { FortIO::Namelist.dump(root, separator: "semicolon") }.to raise_error(RuntimeError, /separator/)
    end

    example "invalid alignment raises error" do
      expect { FortIO::Namelist.dump(root, alignment: "center") }.to raise_error(RuntimeError, /alignment/)
    end

    example "invalid group_end raises error" do
      expect { FortIO::Namelist.dump(root, group_end: "done") }.to raise_error(RuntimeError, /group_end/)
    end

    example "invalid float_format raises error" do
      root_f = {group: {v1: 1.0}}
      expect { FortIO::Namelist.dump(root_f, float_format: "hex") }.to raise_error(RuntimeError, /float_format/)
    end

    example "invalid logical_format raises error" do
      root_l = {group: {v1: true}}
      expect { FortIO::Namelist.dump(root_l, logical_format: "yesno") }.to raise_error(RuntimeError, /logical_format/)
    end

  end

  # --- dump → parse ラウンドトリップ ---

  context "round-trip (dump then parse)" do

    example "integers survive round-trip" do
      original = {grp: {a: 1, b: -99, c: 0}}
      text = FortIO::Namelist.dump(original)
      recovered = FortIO::Namelist.parse(text)
      is_asserted_by { recovered[:grp][:a] == 1 }
      is_asserted_by { recovered[:grp][:b] == -99 }
      is_asserted_by { recovered[:grp][:c] == 0 }
    end

    example "floats survive round-trip" do
      original = {grp: {x: 3.14, y: -0.001, z: 1.0e10}}
      text = FortIO::Namelist.dump(original)
      recovered = FortIO::Namelist.parse(text)
      is_asserted_by { recovered[:grp][:x] == 3.14 }
      is_asserted_by { recovered[:grp][:y] == -0.001 }
      is_asserted_by { (recovered[:grp][:z] - 1.0e10).abs < 1.0 }
    end

    example "booleans survive round-trip" do
      original = {grp: {a: true, b: false}}
      text = FortIO::Namelist.dump(original)
      recovered = FortIO::Namelist.parse(text)
      is_asserted_by { recovered[:grp][:a] == true }
      is_asserted_by { recovered[:grp][:b] == false }
    end

    example "strings with single quotes survive round-trip" do
      original = {grp: {s: "hello world"}}
      text = FortIO::Namelist.dump(original)
      recovered = FortIO::Namelist.parse(text)
      is_asserted_by { recovered[:grp][:s] == "hello world" }
    end

    example "string containing single quote uses double quotes" do
      original = {grp: {s: "it's"}}
      text = FortIO::Namelist.dump(original)
      recovered = FortIO::Namelist.parse(text)
      is_asserted_by { recovered[:grp][:s] == "it's" }
    end

    example "arrays survive round-trip" do
      original = {grp: {arr: [1, 2, 3]}}
      text = FortIO::Namelist.dump(original)
      recovered = FortIO::Namelist.parse(text)
      is_asserted_by { recovered[:grp][:arr] == [1, 2, 3] }
    end

  end

  # --- コメント ---

  context "comments" do

    example "inline comment after value" do
      input = "&example\n  v1 = 42  ! this is a comment\n/\n"
      nml = FortIO::Namelist.parse(input)
      is_asserted_by { nml[:example][:v1] == 42 }
    end

    example "comment line between variables" do
      input = "&example\n  v1 = 1\n! middle comment\n  v2 = 2\n/\n"
      nml = FortIO::Namelist.parse(input)
      is_asserted_by { nml[:example][:v1] == 1 }
      is_asserted_by { nml[:example][:v2] == 2 }
    end

    example "comment before group" do
      input = "! header comment\n&example\n  v1 = 1\n/\n"
      nml = FortIO::Namelist.parse(input)
      is_asserted_by { nml[:example][:v1] == 1 }
    end

  end

  # --- $ prefix スタイル ---

  context "dollar prefix style" do

    example "$group ... $end" do
      input = "$example\n  v1 = 100\n$end\n"
      nml = FortIO::Namelist.parse(input)
      is_asserted_by { nml[:example][:v1] == 100 }
    end

  end

  # --- IO-like オブジェクト (duck typing) ---

  context "IO-like input" do

    example "accepts object with #read method" do
      input = StringIO.new("&example\n  v1 = 7\n/\n")
      nml = FortIO::Namelist.parse(input)
      is_asserted_by { nml[:example][:v1] == 7 }
    end

  end

  # --- filter メソッド ---

  context "filter method" do

    example "modifies values via block" do
      input = "&settings\n  count = 5\n  name = 'old'\n/\n"
      output = FortIO::Namelist.filter(input) do |config|
        config[:settings][:count] = 99
        config[:settings][:name] = "new"
      end
      recovered = FortIO::Namelist.parse(output)
      is_asserted_by { recovered[:settings][:count] == 99 }
      is_asserted_by { recovered[:settings][:name] == "new" }
    end

  end

  # --- group引数の型チェック ---

  context "invalid group argument type" do

    example "numeric group argument raises error" do
      input = "&example\n  v1 = 1\n/\n"
      expect { FortIO::Namelist.parse(input, group: 123) }.to raise_error(RuntimeError, /invalid/)
    end

  end

end
