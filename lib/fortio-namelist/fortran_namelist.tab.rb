#
# DO NOT MODIFY!!!!
# This file is automatically generated by Racc 1.4.16
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
          when @s.scan(/\A\d+[a-z_]\w*/i)         ### STRING-Like
            return [
              :STRINGLIKE,
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
            elsif @s.match?(/\A[a-z]\w*\s*,/i) 
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
          when @s.scan(/\A_\w*/i)                 ### STRING-Like
            return [
              :STRINGLIKE,
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

module_eval(<<'...end fortran_namelist.y/module_eval...', 'fortran_namelist.y', 132)

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
    48,    34,    65,    47,     4,     5,    39,    51,    41,    63,
    42,    43,    46,    49,    48,    66,    62,    47,    14,    15,
    39,    13,    41,    47,    42,    43,    46,    49,    59,    47,
    42,    43,    46,    24,    41,    26,    42,    43,    46,    47,
    24,    33,    56,    55,    41,    13,    42,    43,    46,    13,
    13,    29,    59,    30,    10,    51,    46,    30,     4,     5,
    21,    13,    18,     4,     5,    21,     7,    18,    24,     4,
     5,    59,    23,    13,    13,    46,    57,    10,    58,    61,
    24,    24,    68,    70,    51,    74,    75 ]

racc_action_check = [
    28,    28,    57,    28,     0,     0,    28,    74,    28,    51,
    28,    28,    28,    28,    69,    57,    51,    69,     6,     7,
    69,    10,    69,    58,    69,    69,    69,    69,    58,    55,
    58,    58,    58,    27,    55,    19,    55,    55,    55,    36,
    22,    27,    36,    36,    36,    21,    36,    36,    36,    61,
     3,    21,    47,    21,     3,    29,    47,    61,     9,     9,
     9,    30,     9,    25,    25,    25,     1,    25,    11,     1,
     1,    68,    11,    16,    33,    68,    37,    16,    41,    50,
    52,    53,    60,    62,    63,    70,    72 ]

racc_action_pointer = [
     1,    66,   nil,    48,   nil,   nil,    13,    19,   nil,    55,
    19,    66,   nil,   nil,   nil,   nil,    71,   nil,   nil,    30,
   nil,    43,    38,   nil,   nil,    60,   nil,    31,    -5,    42,
    59,   nil,   nil,    72,   nil,   nil,    31,    64,   nil,   nil,
   nil,    64,   nil,   nil,   nil,   nil,   nil,    39,   nil,   nil,
    70,    -3,    78,    79,   nil,    21,   nil,    -3,    15,   nil,
    70,    47,    70,    71,   nil,   nil,   nil,   nil,    58,     9,
    73,   nil,    77,   nil,    -6,   nil,   nil ]

racc_action_default = [
   -51,   -51,    -2,   -15,    -5,    -6,   -51,   -51,    -1,   -51,
    -8,   -11,   -12,   -13,    -7,    77,   -15,    -4,   -16,   -51,
   -18,   -51,    -9,   -10,   -14,   -51,   -17,   -51,   -51,   -51,
   -23,    -3,   -19,   -25,   -20,   -21,   -27,   -28,   -29,   -30,
   -34,   -40,   -36,   -37,   -38,   -39,   -41,   -51,   -43,   -44,
   -51,   -47,   -24,   -26,   -31,   -51,   -33,   -51,   -51,   -40,
   -51,   -51,   -51,   -51,   -32,   -45,   -46,   -35,   -51,   -51,
   -48,   -49,   -51,   -22,   -51,   -42,   -50 ]

racc_goto_table = [
    22,    50,    35,    60,    28,    20,    54,     2,     8,     9,
    17,    27,    19,     1,    16,    67,   nil,   nil,   nil,   nil,
    52,    32,    25,    53,    72,    64,    31,   nil,    19,   nil,
   nil,   nil,   nil,   nil,   nil,    71,   nil,   nil,   nil,   nil,
   nil,   nil,   nil,    73,    69,   nil,    76,   nil,   nil,   nil,
   nil,    27 ]

racc_goto_check = [
     8,    13,    12,    18,    11,    10,    16,     2,     2,     4,
     6,     8,     7,     1,     5,    17,   nil,   nil,   nil,   nil,
     8,    10,     4,     8,    18,    16,     6,   nil,     7,   nil,
   nil,   nil,   nil,   nil,   nil,    13,   nil,   nil,   nil,   nil,
   nil,   nil,   nil,    12,    11,   nil,    13,   nil,   nil,   nil,
   nil,     8 ]

racc_goto_pointer = [
   nil,    13,     7,   nil,     6,     5,     1,     3,   -10,   nil,
    -4,   -17,   -26,   -28,   nil,   nil,   -30,   -43,   -44,   nil ]

racc_goto_default = [
   nil,   nil,   nil,     3,   nil,   nil,   nil,     6,    11,    12,
   nil,   nil,   nil,   nil,    36,    37,    38,    40,    44,    45 ]

racc_reduce_table = [
  0, 0, :racc_error,
  2, 21, :_reduce_none,
  1, 21, :_reduce_none,
  5, 22, :_reduce_3,
  3, 22, :_reduce_4,
  1, 27, :_reduce_none,
  1, 27, :_reduce_none,
  2, 23, :_reduce_7,
  1, 24, :_reduce_none,
  2, 24, :_reduce_none,
  2, 24, :_reduce_none,
  1, 24, :_reduce_none,
  1, 24, :_reduce_none,
  1, 28, :_reduce_none,
  2, 28, :_reduce_none,
  0, 29, :_reduce_none,
  1, 26, :_reduce_none,
  2, 26, :_reduce_17,
  1, 25, :_reduce_18,
  3, 25, :_reduce_19,
  3, 30, :_reduce_20,
  3, 30, :_reduce_21,
  6, 30, :_reduce_22,
  1, 31, :_reduce_none,
  2, 31, :_reduce_none,
  2, 31, :_reduce_none,
  3, 31, :_reduce_none,
  1, 32, :_reduce_none,
  1, 32, :_reduce_none,
  1, 34, :_reduce_none,
  1, 34, :_reduce_30,
  2, 34, :_reduce_31,
  3, 34, :_reduce_32,
  2, 34, :_reduce_33,
  1, 36, :_reduce_34,
  3, 36, :_reduce_35,
  1, 37, :_reduce_none,
  1, 37, :_reduce_none,
  1, 37, :_reduce_none,
  1, 37, :_reduce_none,
  1, 38, :_reduce_none,
  1, 38, :_reduce_none,
  5, 39, :_reduce_42,
  1, 35, :_reduce_43,
  1, 35, :_reduce_44,
  3, 35, :_reduce_45,
  3, 35, :_reduce_46,
  1, 33, :_reduce_47,
  3, 33, :_reduce_48,
  3, 33, :_reduce_49,
  5, 33, :_reduce_50 ]

racc_reduce_n = 51

racc_shift_n = 77

racc_token_table = {
  false => 0,
  :error => 1,
  :NL => 2,
  "&" => 3,
  "$" => 4,
  :IDENT => 5,
  :COMMA => 6,
  "/" => 7,
  "(" => 8,
  ")" => 9,
  "=" => 10,
  :NIL => 11,
  "," => 12,
  :DIGITS => 13,
  "*" => 14,
  :STRING => 15,
  :LOGICAL => 16,
  :FLOAT => 17,
  :STRINGLIKE => 18,
  ":" => 19 }

racc_nt_base = 20

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
  "NL",
  "\"&\"",
  "\"$\"",
  "IDENT",
  "COMMA",
  "\"/\"",
  "\"(\"",
  "\")\"",
  "\"=\"",
  "NIL",
  "\",\"",
  "DIGITS",
  "\"*\"",
  "STRING",
  "LOGICAL",
  "FLOAT",
  "STRINGLIKE",
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
  "equal",
  "rvalues",
  "array_spec",
  "rlist",
  "ident_list",
  "element",
  "constant",
  "real",
  "complex" ]

Racc_debug_parser = false

##### State transition tables end #####

# reduce 0 omitted

# reduce 1 omitted

# reduce 2 omitted

module_eval(<<'.,.,', 'fortran_namelist.y', 28)
  def _reduce_3(val, _values, result)
     @root[val[0]] = val[2]; @scan.in_namelist = nil
    result
  end
.,.,

module_eval(<<'.,.,', 'fortran_namelist.y', 30)
  def _reduce_4(val, _values, result)
     @root[val[0]] = []; @scan.in_namelist = nil
    result
  end
.,.,

# reduce 5 omitted

# reduce 6 omitted

module_eval(<<'.,.,', 'fortran_namelist.y', 38)
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

# reduce 15 omitted

# reduce 16 omitted

module_eval(<<'.,.,', 'fortran_namelist.y', 56)
  def _reduce_17(val, _values, result)
     raise Racc::ParseError, "\nparse error (&)" unless val[1] =~ /\Aend\Z/i
    result
  end
.,.,

module_eval(<<'.,.,', 'fortran_namelist.y', 59)
  def _reduce_18(val, _values, result)
     result = [val[0]]
    result
  end
.,.,

module_eval(<<'.,.,', 'fortran_namelist.y', 61)
  def _reduce_19(val, _values, result)
     result = val[0] + [val[2]]
    result
  end
.,.,

module_eval(<<'.,.,', 'fortran_namelist.y', 65)
  def _reduce_20(val, _values, result)
     result = ParamDef.new(val[0].downcase, nil, "")
    result
  end
.,.,

module_eval(<<'.,.,', 'fortran_namelist.y', 67)
  def _reduce_21(val, _values, result)
     result = ParamDef.new(val[0].downcase, nil, val[2])
    result
  end
.,.,

module_eval(<<'.,.,', 'fortran_namelist.y', 69)
  def _reduce_22(val, _values, result)
     result = ParamDef.new(val[0].downcase, val[2], val[5])
    result
  end
.,.,

# reduce 23 omitted

# reduce 24 omitted

# reduce 25 omitted

# reduce 26 omitted

# reduce 27 omitted

# reduce 28 omitted

# reduce 29 omitted

module_eval(<<'.,.,', 'fortran_namelist.y', 83)
  def _reduce_30(val, _values, result)
     result = [nil, nil]
    result
  end
.,.,

module_eval(<<'.,.,', 'fortran_namelist.y', 85)
  def _reduce_31(val, _values, result)
     result = val[0] + val[1]
    result
  end
.,.,

module_eval(<<'.,.,', 'fortran_namelist.y', 87)
  def _reduce_32(val, _values, result)
     result = val[0] + val[2]
    result
  end
.,.,

module_eval(<<'.,.,', 'fortran_namelist.y', 89)
  def _reduce_33(val, _values, result)
     result = val[0] + [nil]
    result
  end
.,.,

module_eval(<<'.,.,', 'fortran_namelist.y', 92)
  def _reduce_34(val, _values, result)
     result = [val[0]]
    result
  end
.,.,

module_eval(<<'.,.,', 'fortran_namelist.y', 94)
  def _reduce_35(val, _values, result)
     result = [val[2]] * val[0]
    result
  end
.,.,

# reduce 36 omitted

# reduce 37 omitted

# reduce 38 omitted

# reduce 39 omitted

# reduce 40 omitted

# reduce 41 omitted

module_eval(<<'.,.,', 'fortran_namelist.y', 108)
  def _reduce_42(val, _values, result)
     result = Complex(val[1],val[3])
    result
  end
.,.,

module_eval(<<'.,.,', 'fortran_namelist.y', 111)
  def _reduce_43(val, _values, result)
     result = [val[0]]
    result
  end
.,.,

module_eval(<<'.,.,', 'fortran_namelist.y', 113)
  def _reduce_44(val, _values, result)
     result = [val[0]]
    result
  end
.,.,

module_eval(<<'.,.,', 'fortran_namelist.y', 115)
  def _reduce_45(val, _values, result)
     result = val[0] + [val[2]]
    result
  end
.,.,

module_eval(<<'.,.,', 'fortran_namelist.y', 117)
  def _reduce_46(val, _values, result)
     result = val[0] + [val[2]]
    result
  end
.,.,

module_eval(<<'.,.,', 'fortran_namelist.y', 120)
  def _reduce_47(val, _values, result)
     result = [val[0]-1]
    result
  end
.,.,

module_eval(<<'.,.,', 'fortran_namelist.y', 122)
  def _reduce_48(val, _values, result)
     result = [(val[0]-1)..(val[2]-1)]
    result
  end
.,.,

module_eval(<<'.,.,', 'fortran_namelist.y', 124)
  def _reduce_49(val, _values, result)
     result = [val[0]-1] + val[2]
    result
  end
.,.,

module_eval(<<'.,.,', 'fortran_namelist.y', 126)
  def _reduce_50(val, _values, result)
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


