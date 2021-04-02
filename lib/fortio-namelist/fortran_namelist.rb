# ----------------------------------------------------------------------------
#
#  fortran_namelist.rb
#
#  This file is part of simple-fortio library.
#
#  Copyright (C) 2005-2021 Hiroki Motoyoshi
#
# ----------------------------------------------------------------------------

require_relative "fortran_namelist.tab"

module FortIO::Namelist

  class Parser
  
    ParamDef = Struct.new(:ident, :array_spec, :rval)

    class ParamDef

      def set (hash)
        case hash[ident]
        when Array
          case array_spec
          when nil
            if rval.is_a?(Array) and rval.size == 1
              hash[ident][0] = rval.first
            else
              hash[ident].clear
              hash[ident].push(*rval)
            end              
          else
            if array_spec.first.is_a?(Integer) 
              if rval.size == 1
                hash[ident][*array_spec] = rval.first
              else
                idx = array_spec.first
                hash[ident][idx..idx+rval.size-1] = *rval
              end
            else
              hash[ident][*array_spec] = rval
            end
          end        
        else
          if array_spec
            hash[ident] = []
            set(hash)
          else
            if rval.is_a?(Array) and rval.size == 1
              hash[ident] = rval.first
            else
              hash[ident] = rval
            end
          end
        end
      end
      
      def inspect
        if array_spec
          return "#{ident}(#{array_spec.inspect}) = #{rval.inspect}"
        else
          return "#{ident} = #{rval}"
        end
      end
    end

  end

  class Reader
  
    def initialize (text)
      @namelist = FortIO::Namelist::Parser.new.parse(text)
    end
  
    def parse (group, out={})
      group = group.downcase
      raise "no definition of namelist group '#{group}'" \
                                        unless nml = @namelist[group]
      nml.each do |paramdef|
        paramdef.set(out)
      end
      return out
    end

    attr_reader :namelist
  
  end


  #
  # class methods of FortIO::Namelist
  #
  
  #
  # FortIO::Namelist.dump(hash, group: "namelist")
  #
  # hash -> namelist converter
  #
  
  def self.float_to_string (value, d:)
    if value == 0
      return "0.0"
    elsif Math.log10(value).between?(-4,4)
      return "%.16g" % value
    else
      num,exp = ("%.16e" % value).split(/e/)
      return ("%.16g" % num) + d + exp
    end
  end
  
  def self.format_element (value, 
                           logical_format: 'normal', 
                           float_format: 'normal', 
                           uppercase: false)
    case value
    when String
      if value !~ /'/
        return "'" + value + "'"
      else 
        return '"' + value.gsub(/"/, '""') + '"'
      end
    when Float
      d = uppercase ? "D" : "d"
      case float_format
      when 'normal'
        float_to_string(value, d: d)
      when 'd0'
        value = float_to_string(value, d: d)
        return ( value =~ /#{d}/ ) ? value : value + d + "0"
      when 'exp'
        num,exp = ("%.16e" % value).split(/e/)
        return ("%.16g" % num) + d + exp
      else        
        raise "invalid float_format"
      end
    when Complex
      format("(%s,%s)",
             format_element(value.real, float_format: float_format),
             format_element(value.imag, float_format: float_format))
    when TrueClass
      case logical_format
      when 'normal'
        return uppercase ? ".TRUE." : ".true."
      when 'short'
        return uppercase ? "T" : "t"
      else
        raise "invalid logical_format"
      end
    when FalseClass
      case logical_format
      when 'normal'
        return uppercase ? ".FALSE." : ".false."
      when 'short'
        return uppercase ? "F" : "f"
      else
        raise "invalid logical_format"
      end
    else
      return value.to_s
    end      
  end
  
  def self.generate (hash, 
                     group: "group", 
                     array_style: "stream",
                     alignment: "left",
                     uppercase: false,
                     separator: "comma",
                     group_end: "/",
                     indent: '  ',
                     **format_options)
    format_options[:uppercase] = uppercase
    list = []
    hash.each do |ident, value|
      case value
      when Array
        case array_style
        when "index"
          value.each_with_index do |e, i|
            if e
              list << ["#{ident}(#{i+1})", format_element(e, **format_options)]
            end
          end
        when "stream"
          list << [ident, value.map{ |e| format_element(e, **format_options) }.join(", ")]
        else
          raise "invalid keyword argument `array_style` (should be 'index', 'stream')"  
        end
      else
        list << [ident, format_element(value, **format_options)]
      end
    end
    if uppercase 
      list = list.map{|ident,value| [ident.upcase, value]}
      group = group.upcase
    end
    case separator
    when "comma", ","
      nl = ",\n"
    when "nl", "\n"
      nl = "\n"
    else
      raise "invalid keyword argument `separator` (should be 'comma', ',', 'nl', '\n')"
    end
    case alignment
    when /\Astream(:(\d+))?/
      maxlen = $2 ? $2.to_i : 79 - indent.size
      elements = list.map { |ident, value|
        format("%s = %s", ident, value)
      }
      body = ""
      line = []
      len  = indent.size
      elements.each do |e|
        if len + e.length >= maxlen
          body += indent + line.join(", ") + nl
          line = []
          len  = indent.size
        end
        line << e
        len += e.length + 2
      end
      body += indent + line.join(", ") + nl unless line.empty?
      body = body.chomp
    when "none"
      body = list.map { |ident, value|
        format("%s%s = %s", indent, ident, value)
      }.join(nl)      
    when /\Aleft(:(\d+))?/
      if $2
        ident_maxlen = $2.to_i - indent.size
      else
        ident_maxlen = list.map{|ident,value| ident.length}.max
      end
      body = list.map { |ident, value|
        format("%s%-#{ident_maxlen}s = %s", indent, ident, value)
      }.join(nl)
    when /\Aright(:(\d+))?/
      if $2
        ident_maxlen = $2.to_i - indent.size
      else
        ident_maxlen = list.map{|ident,value| ident.length}.max
      end
      body = list.map { |ident, value|
        format("%s%#{ident_maxlen}s = %s", indent, ident, value)
      }.join(nl)
    else
      raise "invalid keyword argument `alignment` (should be 'normal' 'left' 'right' 'stream')"  
    end
    case group_end
    when "slash", "/"
      tail = "/"
    when "end"
      tail = "&end"
    else
      raise "invalid keyword argument `group_end` (should be 'slash', '/', 'end')"
    end
    return ["&#{group}", body, tail, ""].join("\n")
  end

  def self.dump (root, **format_options)
    return root.map { |group, hash| generate(hash, group: group, **format_options) }.join
  end

  #
  #  FortIO::Namelist.read(input, name: nil) 
  #
  def self.parse (input, group: nil)
    case input
    when String
      text = input
    else
      text = input.read
    end
    reader = FortIO::Namelist::Reader.new(text)
    case group
    when Array
      groups = group.map{|s| s.intern }
    when String, Symbol
      groups = [group].map{|s| s.intern }
    when nil
      groups = reader.namelist.keys
    else
      raise "invalid keyword arugment `group` '#{group.inspect}'"
    end
    return groups.each_with_object({}) { |group, root|
      root[group] = {}
      reader.parse(group, root[group])
    }    
  end
  
  def self.read (input, group: nil)
    parse(input, group: group)
  end

  #
  #  FortIO::Namelist.filter(input) { |hash| MODIFYING HASH }
  #
  #  input : namelist string
  #  return value : namelist string
  #
  def self.filter (input, **format_options)
    config = parse(input)
    yield config
    return dump(config, **format_options)
  end

end

