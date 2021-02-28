#
# DO NOT MODIFY!!!!
# This file is automatically generated by Racc 1.5.2
# from Racc grammar file "".
#

require 'racc/parser.rb'


require "strscan"
require "stringio"

module FortIO
end

module FortIO::Namelist

  class Scanner 
  
    def initialize (text)
      @s = StringScanner.new(text)
      @in_namelist = nil
    end

    attr_accessor :in_namelist

    def debug_info
      lines  = @s.string.split(/\n/)
      lineno = @s.string[0...@s.pos].split(/\n/).size
      info = ""
      if lineno > 1
        info << format("   %4i: %s\n", lineno-1, lines[lineno-2])
      end
      info << format(">> %4i: %s\n", lineno, lines[lineno-1])
      if lineno <= lines.size - 1
        info << format("   %4i: %s\n", lineno+1, lines[lineno])
      end
      info
    end

    def yylex
      while @s.rest?
        unless @in_namelist
          case
          when @s.scan(/\A([\$&])/)              ### {$|&}
            @in_namelist = "dummy"
            return [
              @s[0], 
              nil
            ]
          when @s.scan(/\A[^\$&]/)
            next
          end       
        else
          case
          when @s.scan(/\A\(/)
            return [
              '(',
              nil
            ]
          when @s.scan(/\A\)/)
            return [
              ')',
              nil
            ]
          when @s.scan(/\A\:/)
            return [
              ':',
              nil
            ]
          when @s.scan(/\A[+-]?(\d+)\.(\d+)?([ED][+-]?(\d+))?/i) ### float
            return [                              ### 1.2E+3, 1.E+3, 1.2E3
              :FLOAT,                             ### 1.2, 1.
              @s[0].sub(/D/i,'e').sub(/\.e/,".0e").to_f
            ]
          when @s.scan(/\A[+-]?\.(\d+)([ED][+-]?(\d+))?/i)       ### float
            return [                              ### .2E+3, -.2E+3, .2E3
              :FLOAT,                             ### .2, -.2
              @s[0].sub(/D/i,'e').sub(/\./, '0.').to_f
            ]
          when @s.scan(/\A[+-]?(\d+)[ED][+-]?(\d+)/i)            ### float
            return [                              ### 12E+3, 12E3, 0E0
              :FLOAT, 
              @s[0].sub(/D/i,'e').to_f
            ]
          when @s.scan(/\A\d+[a-z_]\w*/i)         ### STRING
            return [
              :STRING,
              @s[0]
            ]
          when @s.scan(/\A[\-\+]?\d+/)            ### digits
            return [
              :DIGITS, 
              Integer(@s[0])
            ]
          when @s.scan(/\A'((?:''|[^'])*)'/)      ### 'quoted string'
            return [
              :STRING, 
              @s[1].gsub(/''/, "'")
            ]
          when @s.scan(/\A"((?:""|[^"])*)"/)      ### 'double-quoted string'
            return [
              :STRING, 
              @s[1].gsub(/""/, '"')
            ]
          when @s.scan(/\A,/)                     ### ,
            @s.scan(/\A[ \t]+/)
            while @s.scan(/\A\n[ \t]*/) or @s.scan(/\A\![^\n]*/)
              ### skip comment
            end
            if @s.scan(/\A\&[ \t]*\n[ \t]*\&/)  ### & &
              return [
                ',',
                nil
              ]
            elsif @s.match?(/\A[a-z]\w*/i) or @s.match?(/\A[\&\$\/\!]/)
              return [
                :COMMA, 
                nil
              ]
            elsif @s.match?(/\A,/)
              return [
                :NIL,
                nil
              ]
            else
              return [
                ',',
                nil
              ]
            end
          when @s.scan(/\A\&[ \t]*\n[ \t]*\&/)      ### & &
            next            
          when @s.scan(/\A[\$&\/=\(\):*]/)        ### {$|&|/|,|=|(|)|:|*}
            return [
              @s[0], 
              nil
            ]
          when @s.scan(/\A_\w*/i)                 ### STRING
            return [
              :STRING,
              @s[0]
            ]
          when @s.scan(/\A\.t.*?\./i)             ### LOGICAL true
            return [                 
              :LOGICAL,
              true,
            ]
          when @s.scan(/\A\.f.*?\./i)             ### LOGICAL false
            return [
              :LOGICAL,
              false,
            ]
          when @s.match?(/\At[^\w]/i)             ### LOGICAL true
            @s.scan(/\At/i)
            ms = @s[0]
            if @s.match?(/\A[ \t]*=/)
              return [
                :IDENT,
                ms
              ]
            else
              return [                 
                :LOGICAL,
                true,
              ]
            end
          when @s.match?(/\Af[^\w]/i)             ### LOGICAL false
            @s.scan(/\Af/i)
            ms = @s[0]
            if @s.match?(/\A[ \t]*=/)
              return [
                :IDENT,
                ms
              ]
            else
              return [                 
                :LOGICAL,
                false,
              ]
            end
          when @s.scan(/\A[a-z]\w*/i)             ### IDENT or LOGICAL
            return [
              :IDENT,
              @s[0]
            ]
          when @s.scan(/\A\n/)                    ### newline
            return [
              :NL,
              nil
            ]
            next
          when @s.scan(/\A[ \t]+/)                ### blank
            next
          when @s.scan(/\A![^\n]*?\n/)            ### comment
            next
          else
            @s.rest =~ /\A(.*)$/
            raise "namelist parse error ('#{$1}')\n" + debug_info
          end
        end
      end
    end

  end
end

module FortIO
  module Namelist
    class Parser < Racc::Parser

module_eval(<<'...end fortran_namelist.y/module_eval...', 'fortran_namelist.y', 125)

  def parse (str)
    @scan = FortIO::Namelist::Scanner.new(str)
    @root = {}
    begin
      @yydebug = true
      do_parse
    rescue Racc::ParseError => err
      message = ""
      message << "namelist " << err.message[1..-1] 
      if @scan.in_namelist and @scan.in_namelist != "dummy"
        message << " in &#{@scan.in_namelist} ... &end"
      end
      message << "\n"
      message << @scan.debug_info
      raise RuntimeError, message
    end
    return @root
  end

  def next_token
    return @scan.yylex
  end

...end fortran_namelist.y/module_eval...
##### State transition tables begin ###

racc_action_table = [
    44,    30,    13,    59,    14,    43,    15,    35,    36,    38,
    58,    39,    40,    41,    44,     7,    22,     4,     5,    43,
    22,    35,    36,    38,    44,    39,    40,    41,    51,    43,
    24,    35,    36,    38,    52,    39,    40,    41,    43,    46,
    49,    50,    38,    43,    39,    40,    41,    38,    43,    39,
    40,    41,    64,    53,    39,    40,    41,     4,     5,    21,
     4,     5,    18,     4,     5,    21,    54,    54,    18,    57,
    55,    55,    10,    13,    10,    13,    25,    26,    61,    62,
    65,    66,    67,    46,    71,    72,    46 ]

racc_action_check = [
    25,    25,    25,    46,     6,    25,     7,    25,    25,    25,
    46,    25,    25,    25,    29,     1,    29,     1,     1,    29,
    11,    29,    29,    29,    66,    29,    29,    29,    33,    66,
    19,    66,    66,    66,    33,    66,    66,    66,    32,    26,
    32,    32,    32,    49,    32,    32,    32,    49,    53,    49,
    49,    49,    53,    38,    53,    53,    53,     9,     9,     9,
     0,     0,     9,    23,    23,    23,    43,    65,    23,    45,
    43,    65,     3,     3,    16,    16,    21,    21,    51,    52,
    56,    57,    58,    59,    67,    69,    71 ]

racc_action_pointer = [
    58,    15,   nil,    67,   nil,   nil,     0,     6,   nil,    55,
   nil,    14,   nil,   nil,   nil,   nil,    69,   nil,   nil,    26,
   nil,    68,   nil,    61,   nil,    -4,    26,   nil,   nil,    10,
   nil,   nil,    29,    23,   nil,   nil,   nil,   nil,    39,   nil,
   nil,   nil,   nil,    53,   nil,    59,    -8,   nil,   nil,    34,
   nil,    74,    64,    39,   nil,   nil,    69,    73,    69,    70,
   nil,   nil,   nil,   nil,   nil,    54,    20,    73,   nil,    75,
   nil,    73,   nil,   nil ]

racc_action_default = [
   -48,   -48,    -2,   -13,    -5,    -6,   -48,   -48,    -1,   -48,
    -8,    -9,   -10,   -11,    -7,    74,   -13,    -4,   -14,   -48,
   -16,   -48,   -12,   -48,   -15,   -48,   -48,    -3,   -17,   -48,
   -18,   -19,   -22,   -23,   -24,   -25,   -26,   -30,   -34,   -32,
   -33,   -35,   -36,   -48,   -40,   -48,   -44,   -20,   -27,   -48,
   -29,   -43,   -48,   -48,   -37,   -38,   -48,   -48,   -48,   -48,
   -28,   -41,   -42,   -31,   -34,   -48,   -48,   -45,   -46,   -48,
   -21,   -48,   -39,   -47 ]

racc_goto_table = [
    45,    31,    56,    48,    17,    47,     9,    19,    20,     2,
     8,     1,    16,    29,    63,   nil,   nil,   nil,    27,    23,
    60,    19,    28,   nil,    69,   nil,   nil,   nil,   nil,   nil,
   nil,   nil,   nil,    68,   nil,   nil,   nil,   nil,   nil,   nil,
   nil,   nil,    70,   nil,   nil,    73 ]

racc_goto_check = [
    12,    11,    18,    15,     6,    11,     4,     7,    10,     2,
     2,     1,     5,     8,    16,   nil,   nil,   nil,     6,     4,
    15,     7,    10,   nil,    18,   nil,   nil,   nil,   nil,   nil,
   nil,   nil,   nil,    12,   nil,   nil,   nil,   nil,   nil,   nil,
   nil,   nil,    11,   nil,   nil,    12 ]

racc_goto_pointer = [
   nil,    11,     9,   nil,     3,     3,    -5,    -2,   -12,   nil,
    -1,   -24,   -26,   nil,   nil,   -29,   -39,   nil,   -41 ]

racc_goto_default = [
   nil,   nil,   nil,     3,   nil,   nil,   nil,     6,    11,    12,
   nil,   nil,   nil,    32,    33,    34,    37,    42,   nil ]

racc_reduce_table = [
  0, 0, :racc_error,
  2, 20, :_reduce_none,
  1, 20, :_reduce_none,
  5, 21, :_reduce_3,
  3, 21, :_reduce_4,
  1, 26, :_reduce_none,
  1, 26, :_reduce_none,
  2, 22, :_reduce_7,
  1, 23, :_reduce_none,
  1, 23, :_reduce_none,
  1, 23, :_reduce_none,
  1, 27, :_reduce_none,
  2, 27, :_reduce_none,
  0, 28, :_reduce_none,
  1, 25, :_reduce_none,
  2, 25, :_reduce_15,
  1, 24, :_reduce_16,
  3, 24, :_reduce_17,
  3, 29, :_reduce_18,
  3, 29, :_reduce_19,
  4, 29, :_reduce_20,
  6, 29, :_reduce_21,
  1, 30, :_reduce_none,
  1, 30, :_reduce_none,
  1, 32, :_reduce_none,
  1, 32, :_reduce_25,
  1, 32, :_reduce_26,
  2, 32, :_reduce_27,
  3, 32, :_reduce_28,
  2, 32, :_reduce_29,
  1, 34, :_reduce_30,
  3, 34, :_reduce_31,
  1, 35, :_reduce_none,
  1, 35, :_reduce_none,
  1, 35, :_reduce_none,
  1, 35, :_reduce_none,
  1, 35, :_reduce_none,
  1, 37, :_reduce_none,
  1, 37, :_reduce_none,
  5, 36, :_reduce_39,
  1, 33, :_reduce_40,
  3, 33, :_reduce_41,
  3, 33, :_reduce_42,
  2, 33, :_reduce_43,
  1, 31, :_reduce_44,
  3, 31, :_reduce_45,
  3, 31, :_reduce_46,
  5, 31, :_reduce_47 ]

racc_reduce_n = 48

racc_shift_n = 74

racc_token_table = {
  false => 0,
  :error => 1,
  "&" => 2,
  "$" => 3,
  :IDENT => 4,
  :COMMA => 5,
  :NL => 6,
  "/" => 7,
  "=" => 8,
  "(" => 9,
  ")" => 10,
  "," => 11,
  :NIL => 12,
  :DIGITS => 13,
  "*" => 14,
  :STRING => 15,
  :LOGICAL => 16,
  :FLOAT => 17,
  ":" => 18 }

racc_nt_base = 19

racc_use_result_var = true

Racc_arg = [
  racc_action_table,
  racc_action_check,
  racc_action_default,
  racc_action_pointer,
  racc_goto_table,
  racc_goto_check,
  racc_goto_default,
  racc_goto_pointer,
  racc_nt_base,
  racc_reduce_table,
  racc_token_table,
  racc_shift_n,
  racc_reduce_n,
  racc_use_result_var ]

Racc_token_to_s_table = [
  "$end",
  "error",
  "\"&\"",
  "\"$\"",
  "IDENT",
  "COMMA",
  "NL",
  "\"/\"",
  "\"=\"",
  "\"(\"",
  "\")\"",
  "\",\"",
  "NIL",
  "DIGITS",
  "\"*\"",
  "STRING",
  "LOGICAL",
  "FLOAT",
  "\":\"",
  "$start",
  "namelist",
  "group",
  "group_header",
  "separator",
  "varlist",
  "group_end",
  "group_prefix",
  "nls",
  "blank",
  "vardef",
  "rvalues",
  "array_spec",
  "rlist",
  "ident_list",
  "element",
  "constant",
  "complex",
  "real" ]

Racc_debug_parser = false

##### State transition tables end #####

# reduce 0 omitted

# reduce 1 omitted

# reduce 2 omitted

module_eval(<<'.,.,', 'fortran_namelist.y', 24)
  def _reduce_3(val, _values, result)
     @root[val[0]] = val[2]; @scan.in_namelist = nil
    result
  end
.,.,

module_eval(<<'.,.,', 'fortran_namelist.y', 26)
  def _reduce_4(val, _values, result)
     @root[val[0]] = []; @scan.in_namelist = nil
    result
  end
.,.,

# reduce 5 omitted

# reduce 6 omitted

module_eval(<<'.,.,', 'fortran_namelist.y', 34)
  def _reduce_7(val, _values, result)
     result = val[1].downcase; @scan.in_namelist = val[1].downcase
    result
  end
.,.,

# reduce 8 omitted

# reduce 9 omitted

# reduce 10 omitted

# reduce 11 omitted

# reduce 12 omitted

# reduce 13 omitted

# reduce 14 omitted

module_eval(<<'.,.,', 'fortran_namelist.y', 50)
  def _reduce_15(val, _values, result)
     raise Racc::ParseError, "\nparse error (&)" unless val[1] =~ /\Aend\Z/i
    result
  end
.,.,

module_eval(<<'.,.,', 'fortran_namelist.y', 53)
  def _reduce_16(val, _values, result)
     result = [val[0]]
    result
  end
.,.,

module_eval(<<'.,.,', 'fortran_namelist.y', 55)
  def _reduce_17(val, _values, result)
     result = val[0] + [val[2]]
    result
  end
.,.,

module_eval(<<'.,.,', 'fortran_namelist.y', 59)
  def _reduce_18(val, _values, result)
     result = ParamDef.new(val[0].downcase, nil, "")
    result
  end
.,.,

module_eval(<<'.,.,', 'fortran_namelist.y', 61)
  def _reduce_19(val, _values, result)
     result = ParamDef.new(val[0].downcase, nil, val[2])
    result
  end
.,.,

module_eval(<<'.,.,', 'fortran_namelist.y', 63)
  def _reduce_20(val, _values, result)
     result = ParamDef.new(val[0].downcase, nil, val[3])
    result
  end
.,.,

module_eval(<<'.,.,', 'fortran_namelist.y', 65)
  def _reduce_21(val, _values, result)
     result = ParamDef.new(val[0].downcase, val[2], val[5])
    result
  end
.,.,

# reduce 22 omitted

# reduce 23 omitted

# reduce 24 omitted

module_eval(<<'.,.,', 'fortran_namelist.y', 73)
  def _reduce_25(val, _values, result)
     result = [nil]
    result
  end
.,.,

module_eval(<<'.,.,', 'fortran_namelist.y', 74)
  def _reduce_26(val, _values, result)
     result = [nil]
    result
  end
.,.,

module_eval(<<'.,.,', 'fortran_namelist.y', 76)
  def _reduce_27(val, _values, result)
     result = val[0] + val[1]
    result
  end
.,.,

module_eval(<<'.,.,', 'fortran_namelist.y', 78)
  def _reduce_28(val, _values, result)
     result = val[0] + val[2]
    result
  end
.,.,

module_eval(<<'.,.,', 'fortran_namelist.y', 80)
  def _reduce_29(val, _values, result)
     result = val[0] + [nil]
    result
  end
.,.,

module_eval(<<'.,.,', 'fortran_namelist.y', 83)
  def _reduce_30(val, _values, result)
     result = [val[0]]
    result
  end
.,.,

module_eval(<<'.,.,', 'fortran_namelist.y', 85)
  def _reduce_31(val, _values, result)
     result = [val[2]] * val[0]
    result
  end
.,.,

# reduce 32 omitted

# reduce 33 omitted

# reduce 34 omitted

# reduce 35 omitted

# reduce 36 omitted

# reduce 37 omitted

# reduce 38 omitted

module_eval(<<'.,.,', 'fortran_namelist.y', 100)
  def _reduce_39(val, _values, result)
     result = Complex(val[1],val[3])
    result
  end
.,.,

module_eval(<<'.,.,', 'fortran_namelist.y', 104)
  def _reduce_40(val, _values, result)
     result = [val[0]]
    result
  end
.,.,

module_eval(<<'.,.,', 'fortran_namelist.y', 106)
  def _reduce_41(val, _values, result)
     result = val[0] + [val[2]]
    result
  end
.,.,

module_eval(<<'.,.,', 'fortran_namelist.y', 108)
  def _reduce_42(val, _values, result)
     result = val[0] + [val[2]]
    result
  end
.,.,

module_eval(<<'.,.,', 'fortran_namelist.y', 110)
  def _reduce_43(val, _values, result)
     result = val[0]
    result
  end
.,.,

module_eval(<<'.,.,', 'fortran_namelist.y', 113)
  def _reduce_44(val, _values, result)
     result = [val[0]-1]
    result
  end
.,.,

module_eval(<<'.,.,', 'fortran_namelist.y', 115)
  def _reduce_45(val, _values, result)
     result = [(val[0]-1)..(val[2]-1)]
    result
  end
.,.,

module_eval(<<'.,.,', 'fortran_namelist.y', 117)
  def _reduce_46(val, _values, result)
     result = [val[0]-1] + val[2]
    result
  end
.,.,

module_eval(<<'.,.,', 'fortran_namelist.y', 119)
  def _reduce_47(val, _values, result)
     result = [(val[0]-1)..(val[2]-1)] + val[4]
    result
  end
.,.,

def _reduce_none(val, _values, result)
  val[0]
end

    end   # class Parser
  end   # module Namelist
end   # module FortIO


