## mhc-make.rb -- Installer for Ruby scripts.
##
## Author:  MIYOSHI Masanori <miyoshi@quickhack.net>
##          Yoshinari Nomura <nom@quickhack.net>
## Created: 2000/7/12
##

require 'ftools'
require 'kconv'
require 'getoptlong'

module MhcMake
  def default()
    if File .exists?('Makefile')
      system("make")
    else
      ## print "Nothing to do in #{Dir .pwd}.\n"
    end
    process_subdirs()
  end

  def clean
    if File .exists?('Makefile')
      system("make", "clean")
    else
      Dir .foreach('.'){|src_file|
	if src_file =~ /\.in$/ or src_file == 'Makefile' or 
	    src_file == 'make.rb'

	  dst_file = src_file .sub(/\.in$/, '')
	  if File .exist?(dst_file)
	    File .delete(dst_file)
	    print "removing: " + dst_file + "\n";
	  end
	end
      }
    end
    process_subdirs()
  end

  def process_subdirs()
    target = ARGV .join(' ')

    Dir .foreach('.'){|entry|
      if entry !~ /^\./ and File .directory?(entry)
	if File .exists?("#{entry}/make.rb")
	  print "Making #{target} in #{File .expand_path(entry)}\n"
	  cd = Dir .pwd()
	  Dir .chdir(File .expand_path(entry))
	  system('ruby', 'make.rb', *ARGV)
	  Dir .chdir(cd)

	elsif File .exists?("#{entry}/Makefile")
	  print "Making #{target} in #{File .expand_path(entry)}\n"
	  cd = Dir .pwd()
	  Dir .chdir(File .expand_path(entry))
	  system('make', *ARGV)
	  Dir .chdir(cd)
        end
      end
    }
  end    

  def install
    if File .exists?('Makefile')
      system("make", "install") 
    else
      INSTALL_FILES .each{|filename_mode_dir|
	filename, mode, dir = filename_mode_dir .split(':')
	File .install(filename, dir, mode .oct, true)
      }
    end
    process_subdirs()
  end

  def print_usage()
    print "Usage: make.rb [target]\ntarget can be none, install or clean.\n"
  end
  
  def doit
    if (ARGV .size == 0)
      default()
    else
      case ARGV[0]
      when "install"
	install()
      when "clean"
	clean()
      else
	print_usage();
	exit(1);
      end
    end
  end
end

class MhcConfigure
  require 'rbconfig'
  require 'mkmf'
  include Config

  PATH_SEP = '/'

  ## option, flag, usage string, default value
  DEFAULT_CONFIG_TABLE = [
    ['--kcode',
      GetoptLong::OPTIONAL_ARGUMENT,
      "--kcode=FILE            kanji code (EUC, JIS, SJIS)",
      (/cygwin|mingw32|os2_emx|sharp-human/ =~ RUBY_PLATFORM) ? 'SJIS' : 'EUC'
    ],

    ['--bindir', 
      GetoptLong::OPTIONAL_ARGUMENT,
      "--bindir=DIR            user executables in DIR",
      CONFIG["bindir"]],

    ['--libdir',
      GetoptLong::OPTIONAL_ARGUMENT,
      "--libdir=DIR            script libraries in DIR",
      File::join(CONFIG["libdir"],
		 "ruby",
		 CONFIG["MAJOR"] + "." + CONFIG["MINOR"])],

    ['--with-ruby',
      GetoptLong::OPTIONAL_ARGUMENT, 
      "--with-ruby=PATH        absolute path of ruby executable",
      CONFIG["bindir"] + "/ruby"],

    ['--help',
      GetoptLong::NO_ARGUMENT,
      "--help                  print this message",
      nil]
  ]

  ### check and set default values.
  def initialize
    @macros = {}
    @local_config_table = []

    CONFIG .each{|key, val|
      @macros[get_macro_name(key)] = val
    }
    @macros[get_macro_name('ruby_version')] = 
      VERSION .split('.') .filter{|i| format("%02d", i)} .join('')
    @macros[get_macro_name('ruby_path')] = which('ruby')

    DEFAULT_CONFIG_TABLE .each{|ary|
      @macros[get_macro_name(ary[0] .sub(/^--/, ''))] = ary[-1] if ary[-1]
    }
    @macros[get_macro_name('topdir')] = Dir .pwd
  end

  def macro(name)
    return @macros[name]
  end

  def set_macro(name, val)
    return @macros[name] = val
  end

  def add_macro(name, val)
    return @macros[name] += val
  end

  def usage
    STDERR .print "usage: ruby configure.rb [options]\n"
    str = (DEFAULT_CONFIG_TABLE + @local_config_table) .collect{|ary|
      ary[-2]
    } .join("\n    ")
    STDERR .print "    " + str, "\n"
  end

  def each_macros()
    @macros .each do |key, val|
      yield(key, val)
    end
  end

  def options(config_table)
    return config_table .collect{|ary| ary[0..-3]}
  end

  def get_macro_name(name)
    return '@@MHC_' + name .sub(/^-*/, '') .tr('a-z-', 'A-Z_') + '@@'
  end

  def set_user_config(argv, local_config_table)
    local_config_table .each{|ary|
      @macros[get_macro_name(ary[0] .sub(/^--/, ''))] = ary[-1] if ary[-1]
    }
    @local_config_table = local_config_table

    parser = GetoptLong .new()
    opt_array = options(DEFAULT_CONFIG_TABLE) + options(local_config_table)
    parser .set_options(*opt_array)

    begin
      parser .each_option do |name, arg|
	if name == '--help'
	  usage() 
	  exit(0)
	else
	  macro_name = get_macro_name(name)
	  @macros[macro_name] = (arg == '' ? '1' : arg)
	end
      end
    rescue
      usage()
      exit(1)
    end
    return self
  end

  def replace_keywords(in_file_list)
    in_file_list .each{|src_file_and_mode|
      src_file, mode = src_file_and_mode .split(':')
      dst_file = src_file .sub(/\.in$/, '')
      print "creating #{dst_file} ..."
      replace_keywords1(src_file, dst_file , @macros, mode .oct)
      print "done.\n"
    }
  end

  def replace_keywords1(src_file_name, dst_file_name, keywords, mode = nil)
    src_file  = File .open(src_file_name, "r") or die "#{$!}\n"
    dst_file  = File .open(dst_file_name, "w") or die "#{$!}\n"
    src_contents = src_file .gets(nil)
    src_file .close
    keywords .each{|key, val|
      src_contents .gsub!(key, val)
    }

    if src_contents =~ /(@@MHC_[a-z\d_]+@@)/pin
      STDERR .print "Warn: keyword #{$1} was remiained in #{dst_file_name}.\n"
    end

    dst_file << src_contents
    dst_file .close 
    File .chmod(mode, dst_file_name) if mode

  end

  def which(command, all = false)
    if File .exist?(macro('@@MHC_BINDIR@@') + PATH_SEP + command)
      print macro('@@MHC_BINDIR@@') + PATH_SEP + command, "\n"
      return macro('@@MHC_BINDIR@@') + PATH_SEP + command
    end
    ENV['PATH'] .split(':') .each{|dir|
      if File .exist?(dir + PATH_SEP + command)
	print dir + PATH_SEP + command, "\n"
	return dir + PATH_SEP + command
      end
    }
    return nil
  end

  def search_include(inc_search_path, header_file, macroname)
    return @macros[macroname] if @macros[macroname]

    cflags = $CFLAGS
    found_inc_path = nil

    inc_search_path .each{|inc_path|
      print "In #{inc_path} .. "
      $CFLAGS  = "-I#{inc_path}"
      if have_header(header_file)
	found_inc_path = inc_path
	# avoiding ruby 1.4.3 bug.
	$defs .push($defs .pop .sub!(/-DHAVE_PI-DLP_H/, '-DHAVE_PI_DLP_H'))
	break
      end
    }
    $CFLAGS  = cflags
    @macros['@@MHC_CFLAGS@@'] += " -I#{found_inc_path} " if found_inc_path
    @macros[macroname] = found_inc_path
    return found_inc_path
  end

  def search_library(lib_search_path, libname, funcname, macroname)
    return @macros[macroname] if @macros[macroname]

    ldflags = $LDFLAGS
    found_lib_path = nil

    lib_search_path .each{|lib_path|
      print "In #{lib_path} .. "
      $LDFLAGS = "-L#{lib_path}"
      if have_library("pisock", "pi_socket")
	found_lib_path = lib_path
	break
      end
    }
    @macros['@@MHC_LDFLAGS@@'] += " -L#{found_lib_path} " if found_lib_path
    @macros[macroname] = found_lib_path
    $LDFLAGS = ldflags
    return found_lib_path
  end

  def ruby_version
    ver   = VERSION .split('.') .filter{|i| format("%02d", i)} .join('')
  end
end

### Copyright Notice:

## Copyright (C) 2000 MHC developing team. All rights reserved.

## Redistribution and use in source and binary forms, with or without
## modification, are permitted provided that the following conditions
## are met:
## 
## 1. Redistributions of source code must retain the above copyright
##    notice, this list of conditions and the following disclaimer.
## 2. Redistributions in binary form must reproduce the above copyright
##    notice, this list of conditions and the following disclaimer in the
##    documentation and/or other materials provided with the distribution.
## 3. Neither the name of the team nor the names of its contributors
##    may be used to endorse or promote products derived from this software
##    without specific prior written permission.
## 
## THIS SOFTWARE IS PROVIDED BY THE TEAM AND CONTRIBUTORS ``AS IS''
## AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
## LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
## FOR A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL
## THE TEAM OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
## INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
## (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
## SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
## HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
## STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
## ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
## OF THE POSSIBILITY OF SUCH DAMAGE.

### mhc-make.rb ends here
