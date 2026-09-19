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

rule

  namelist : 
                 namelist group
               | group

  group :
                 group_header separator varlist separator group_end
                           { @root[val[0]] = val[2]; @scan.in_namelist = nil; @scan_result << { group: val[0], lines: @group_start_line..@scan.current_lineno, variables: @current_vars } }
               | group_header separator group_end
                           { @root[val[0]] = []; @scan.in_namelist = nil; @scan_result << { group: val[0], lines: @group_start_line..@scan.current_lineno, variables: [] } }

  group_prefix : 
                 '&' 
               | '$'

  group_header :
                 group_prefix IDENT
                           { result = val[1].downcase.intern; @scan.in_namelist = val[1].downcase.intern; @group_start_line = @scan.current_lineno; @current_vars = [] }

  separator :
                 COMMA
               | COMMA nls
               | nls COMMA
               | nls
               | blank

  nls : 
                 NL
               | nls NL

  blank : 

  group_end :    
                 '/'
               | group_prefix IDENT 
                           { raise Racc::ParseError, "\nparse error (&)" unless val[1] =~ /\Aend\Z/i }

  varlist : 
                 vardef    { result = [val[0]] }
               | varlist separator vardef
                           { result = val[0] << val[2] }

  vardef :
                 IDENT equal COMMA
                           { result = ParamDef.new(val[0].downcase.intern, nil, ""); @current_vars << { name: val[0].to_s, lineno: val[0].lineno } }
               | IDENT equal rvalues
                           { result = ParamDef.new(val[0].downcase.intern, nil, val[2]); @current_vars << { name: val[0].to_s, lineno: val[0].lineno } }
               | IDENT '(' array_spec ')' equal rvalues
                           { result = ParamDef.new(val[0].downcase.intern, val[2], val[5]); idx = val[2].map { |v| v.is_a?(Range) ? "#{v.first+1}:#{v.last+1}" : v+1 }; @current_vars << { name: val[0].to_s, lineno: val[0].lineno, index: idx.size == 1 ? idx[0] : idx.join(",") } }

  equal : 
                 '='
               | '=' nls
               | nls '=' 
               | nls '=' nls

  rvalues : 
                rlist
               | ident_list

  rlist : 
                 element
               | NIL       { result = [nil, nil] }
               | rlist element
                           { result = val[0].concat(val[1]) }
               | rlist ',' element
                           { result = val[0].concat(val[2]) }
               | rlist NIL
                           { result = val[0] << nil }

  element :
                 constant  { result = [val[0]] }
               | DIGITS '*' constant
                           { result = repeated_element(val[0], val[2]) }

  constant :
                 STRING
               | LOGICAL
               | real
               | complex

  real :
                 DIGITS
               | FLOAT

  complex :
                '(' real ',' real ')' 
                           { result = Complex(val[1],val[3]) }
  
  ident_list : 
                 IDENT     { result = [val[0].to_s] }
               | STRINGLIKE
                           { result = [val[0].to_s] }
               | ident_list ',' IDENT 
                           { result = val[0] << val[2].to_s }
               | ident_list ',' STRINGLIKE
                           { result = val[0] << val[2].to_s }

  array_spec :
                 DIGITS    { result = [array_index(val[0])] }
               | DIGITS ':' DIGITS     
                           { result = [array_index_range(val[0], val[2])] }
               | DIGITS ',' array_spec 
                           { result = [array_index(val[0])] + val[2] }
               | DIGITS ':' DIGITS ',' array_spec
                           { result = [array_index_range(val[0], val[2])] + val[4] }

end

---- inner

  attr_reader :scan_result

  def parse (str)
    @scan = FortIO::Namelist::Scanner.new(str)
    @root = {}
    @scan_result = []
    @current_vars = []
    @group_start_line = nil
    begin
      @yydebug = true
      do_parse
    rescue Racc::ParseError => err
      message = ""
      message << "namelist " << err.message
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

  #
  #  Array subscripts are 1-based in a namelist and become 0-based positions
  #  in a Ruby Array. Without the lower bound, `v(0)` would turn into the Ruby
  #  index -1 and quietly overwrite the last element of the array.
  #
  def array_index (subscript)
    if subscript < 1
      raise Racc::ParseError, "parse error: array subscript #{subscript} (subscripts start at 1)"
    end
    if subscript > FortIO::Namelist.max_array_size
      raise Racc::ParseError, "parse error: array subscript #{subscript} " \
                              "exceeds FortIO::Namelist.max_array_size " \
                              "(#{FortIO::Namelist.max_array_size})"
    end
    return subscript - 1
  end

  def array_index_range (first, last)
    if last < first
      raise Racc::ParseError, "parse error: array subscript range #{first}:#{last} ends before it starts"
    end
    return array_index(first)..array_index(last)
  end

  #
  #  `n*value` repeats a value n times.
  #
  def repeated_element (count, value)
    if count < 0
      raise Racc::ParseError, "parse error: repeat count #{count} (repeat counts are not negative)"
    end
    if count > FortIO::Namelist.max_array_size
      raise Racc::ParseError, "parse error: repeat count #{count} " \
                              "exceeds FortIO::Namelist.max_array_size " \
                              "(#{FortIO::Namelist.max_array_size})"
    end
    return [value] * count
  end

---- header

require "strscan"
require "stringio"

module FortIO
end

module FortIO::Namelist

  #
  #  An identifier token that remembers the line it was read from.
  #
  #  The line number has to travel with the token: a variable definition is
  #  reduced only after the parser has read its lookahead token, so asking the
  #  scanner for its current position at that point may already report the
  #  next line.
  #
  class Identifier < String

    attr_accessor :lineno

  end

  class Scanner 
  
    def initialize (text)
      @s = StringScanner.new(text)
      @counted_pos = 0
      @counted_lines = 0
      @in_namelist = nil
    end

    attr_accessor :in_namelist

    def identifier_token (name)
      ident = FortIO::Namelist::Identifier.new(name)
      ident.lineno = current_lineno
      return [:IDENT, ident]
    end

    #
    #  Counting the newlines from the start of the text on every call makes
    #  reading a file quadratic in its length. The scanner only ever moves
    #  forward, so only the text passed since the last call has to be counted.
    #
    def current_lineno
      if @s.pos > @counted_pos
        @counted_lines += @s.string[@counted_pos...@s.pos].count("\n")
        @counted_pos = @s.pos
      end
      return @counted_lines + 1
    end

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
          when @s.scan(/\A\.t[\w\d_]*\.?/i)             ### LOGICAL true
            return [                 
              :LOGICAL,
              true,
            ]
          when @s.scan(/\A\.f[\w\d_]*\.?/i)             ### LOGICAL false
            return [
              :LOGICAL,
              false,
            ]
          when @s.match?(/\At[^\w]/i)             ### LOGICAL true
            @s.scan(/\At/i)
            ms = @s[0]
            if @s.match?(/\A[ \t]*=/)
              return identifier_token(ms)
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
              return identifier_token(ms)
            else
              return [
                :LOGICAL,
                false,
              ]
            end
          when @s.scan(/\A[a-z]\w*/i)             ### IDENT or LOGICAL
            return identifier_token(@s[0])
          when @s.scan(/\A\n/)                    ### newline
            return [
              :NL,
              nil
            ]
            next
          when @s.scan(/\A[ \t]+/)                ### blank
            next
          when @s.scan(/\A![^\n]*\n?/)            ### comment
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

