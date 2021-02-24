# ----------------------------------------------------------------------------
#
#  fortran_namelist.y
#
#  This file is part of simple-fortio library.
#
#  Copyright (C) 2005-2021 Hiroki Motoyoshi
#
# ----------------------------------------------------------------------------

#
#  racc fortran_namelist.y -> fortan_namelist.tab.rb
#

class FortIO::Namelist::Parser

prechigh
  left '='
  left ','
  nonassoc COMMA
  nonassoc NL
preclow

rule

  namelist_all : 
               | namelist
               | namelist namelist_all

  namelist : 
                 header tailer 
                           { @root[val[0]] = []; @scan.in_namelist = nil }
               | header paramlist tailer 
                           { @root[val[0]] = val[1]; @scan.in_namelist = nil }

  prefix :       '&' 
               | '$'

  header :     
                 prefix IDENT { result = val[1].downcase; @scan.in_namelist = val[1].downcase }
               | header NL

  tailer :
                 '/'
               | prefix IDENT { raise Racc::ParseError, "\nparse error (&)" unless val[1] =~ /\Aend\Z/i }

  separator:     COMMA
               | NL  

  paramlist:
                 paramdef  { result = [val[0]] }
               | paramlist paramdef
                           { result = val[0] + [val[1]] }
               | paramlist separator
                           { result = val[0] }

  paramdef:  
                 IDENT '=' separator
                           { result = ParamDef.new(val[0].downcase, nil, "") }
               | IDENT '=' NIL
                           { result = ParamDef.new(val[0].downcase, nil, "") }
               | IDENT '=' rvalues 
                           { result = ParamDef.new(val[0].downcase, nil, val[2]) }
               | IDENT '(' array_spec ')' '=' rvalues  
                           { result = ParamDef.new(val[0].downcase, val[2], val[5]) }

  rvalues :      
                abbreb    { result = val[0] }
               | rvalues abbreb      
                           { result = val[0] + val[1] }
               | rvalues ',' abbreb
                           { result = val[0] + val[2] }
               | rvalues NL abbreb
                           { result = val[0] + val[2] }
               | rvalues NIL
                           { result = val[0] + [nil] }
               | NIL rvalues 
                           { result = [nil] + val[1] }
               | ',' rvalues 
                           { result = [nil] + val[1] }
               | rvalues NL 
                           { result = val[0] }
               | IDENT
                           { result = val[0] }

  abbreb :
                 constant { result = [val[0]] }
               | DIGITS '*' constant
                          { result = [val[2]] * val[0] }

  constant :
                 STRING
               | LOGICAL
               | DIGITS
               | FLOAT
               | complex

  real :         DIGITS
               | FLOAT

  complex :      '(' real ',' real ')' { result = Complex(val[1],val[3]) }

  array_spec :
                 DIGITS    { result = [val[0]-1] }
               | DIGITS ':' DIGITS     
                           { result = [(val[0]-1)..(val[2]-1)] }
               | DIGITS ',' array_spec 
                           { result = [val[0]-1] + val[2] }
               | DIGITS ':' DIGITS ',' array_spec
                           { result = [(val[0]-1)..(val[2]-1)] + val[4] }

end

---- inner

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

---- header

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

---- footer

