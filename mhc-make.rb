## mhc-make.rb -- Installer for Ruby scripts.
##
## Author:  MIYOSHI Masanori <miyoshi@quickhack.net>
##          Yoshinari Nomura <nom@quickhack.net>
## Created: 2000/7/12
## Revised: $Date: 2000/07/14 05:29:52 $

require 'rbconfig'
require 'mkmf'
require 'ftools'
require 'kconv'
require 'getoptlong'

def File .which(command)
  bindir = CONFIG['bindir']

  if bindir and File .exist?(path = (bindir + '/' + command))
    return path
  end

  ENV['PATH'] .split(':') .each{|dir|
    path = dir + '/' + command
    if File .exist?(path)
      return path
    end
  }
  return nil
end

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

class MhcConfigTable
  include Config
  # ['--kcode', '@@MHC_KCODE@@', GetoptLong::OPTIONAL_ARGUMENT, usage, default]

  DEFAULT_CONFIG_TABLE = [
    ['--kcode', '@@MHC_KCODE@@', GetoptLong::OPTIONAL_ARGUMENT,
      "--kcode=FILE            kanji code (EUC, JIS, SJIS)",
      (/cygwin|mingw32|os2_emx|sharp-human/ =~ RUBY_PLATFORM) ? 'SJIS' : 'EUC'
    ],

    ['--bindir', '@@MHC_BINDIR@@', GetoptLong::OPTIONAL_ARGUMENT,
      "--bindir=DIR            user executables in DIR",
      CONFIG["bindir"]],

    ['--libdir', '@MHC_LIBDIR@@', GetoptLong::OPTIONAL_ARGUMENT,
      "--libdir=DIR            script libraries in DIR",
      File::join(CONFIG["libdir"], "ruby",
		 CONFIG["MAJOR"] + "." + CONFIG["MINOR"])],

    ['--with-ruby', '@@MHC_RUBY_PATH@@', GetoptLong::OPTIONAL_ARGUMENT, 
      "--with-ruby=PATH        absolute path of ruby executable",
      nil],

    ['--with-emacs', '@@MHC_EMACS_PATH@@', GetoptLong::OPTIONAL_ARGUMENT, 
      "--with-emacs=PATH       absolute path of emacs/xemacs executable",
      nil],

    ['--help', '@@MHC_HELP@@', GetoptLong::NO_ARGUMENT,
      "--help                  print this message",
      nil]
  ]

  def initialize(config_table = [])
    @config_table = DEFAULT_CONFIG_TABLE + config_table
  end

  def getopt_table
    return @config_table .collect{|ary| [ary[0], ary[2]]}
  end

  def usage_string
    return @config_table .collect{|ary| ary[-2]} .join("\n    ")
  end

  def macro_hash
    hash = {}
    @config_table .each{|ary| hash[ary[1]] = ary[-1]}
    return hash
  end

  def macro_name(option_string)
    @config_table .each{|ary|
      return ary[1] if ary[0] == option_string
    }
    return nil
  end

  def option_name(macro_name)
    @config_table .each{|ary|
      return ary[0] if ary[1] == macro_name
    }
    return nil
  end
end

class MhcConfigure
  include Config

  def initialize(local_config_table = [])
    @macros = {}
    @config_table = MhcConfigTable .new(local_config_table)

    ## import macros from rbconfig.rb
    CONFIG .each{|key, val| @macros[make_macro_name(key)] = val}

    ## import macros from configure table
    @macros .update(@config_table .macro_hash)

    ## set useful macros.
    @macros['@@MHC_RUBY_VERSION@@'] = 
      VERSION .split('.') .filter{|i| format("%02d", i)} .join('')
    @macros['@@MHC_TOPDIR@@'] = Dir .pwd
  end

  def usage
    STDERR .print "usage: ruby configure.rb [options]\n"
    STDERR .print "    ", @config_table .usage_string, "\n"
  end

  ## parse ARGV and set corresponding macros.
  def parse_argv()
    parser = GetoptLong .new()
    parser .set_options(*@config_table .getopt_table)
    begin
      parser .each_option do |name, arg|
	if name == '--help'
	  usage()
	  exit(0)
	else
	  @macros[@config_table .macro_name(name)] = (arg == '' ? '1' : arg)
	end
      end
    rescue
      usage()
      exit(1)
    end
    return self
  end

  def [](name);         @macros[name];         end
  def []=(name, val);   @macros[name] = val;   end

  def macro(name)
    return @macros[name]
  end

  def set_macro(name, val)
    return @macros[name] = val
  end

  def add_macro(name, val)
    return @macros[name] += val
  end

  def each_macro()
    @macros .each do |key, val|
      yield(key, val)
    end
  end

  ## replace keywords in files. in_file_list:
  ## infile_list is a array of 'filename:mode'
  ## such like ['mhc-sync.in:0755', 'mhc2palm.in:0755' ...]
  def replace_keywords(in_file_list)
    in_file_list .each{|src_file_and_mode|
      src_file, mode = src_file_and_mode .split(':')
      dst_file = src_file .sub(/\.in$/, '')
      print "creating #{dst_file} ..."
      replace_keywords1(src_file, dst_file , @macros, mode .oct)
      print "done.\n"
    }
  end

  ## find header file, add to '@@MHC_CFLAGS@@', set macroname.
  def search_include(search_path, header_file, macroname, force, abort)

    search_path = [@macros[macroname]] if @macros[macroname] and !force

    cflags, found_inc_path = $CFLAGS, nil

    search_path .each{|inc_path|
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

    if found_inc_path
      @macros['@@MHC_CFLAGS@@'] += " -I#{found_inc_path} "
      @macros[macroname] = found_inc_path
    elsif abort
      search_abort(header_file, macroname)
    end
    return found_inc_path
  end

  ## find library file, add to '@@MHC_LDFLAGS@@', set macroname.
  def search_library(search_path, libname, funcname, macroname, force, abort)

    search_path = [@macros[macroname]] if @macros[macroname] and !force

    ldflags, found_lib_path = $LDFLAGS, nil

    search_path .each{|lib_path|
      print "In #{lib_path} .. "
      $LDFLAGS = "-L#{lib_path}"
      if have_library("pisock", "pi_socket")
	found_lib_path = lib_path
	break
      end
    }
    $LDFLAGS = ldflags

    if found_lib_path
      @macros['@@MHC_LDFLAGS@@'] += " -L#{found_lib_path} "
      @macros[macroname] = found_lib_path
    elsif abort
      search_abort(libname, macroname)
    end
    return found_lib_path
  end

  ## find command and set macro name.
  def search_command(command, macroname, force, abort)
    path = @macros[macroname]

    if (!path) or force
      if path = File .which(command)
	@macros[macroname] = path
      end
    end

    if path and File .executable?(path)
      print "#{command} is .. #{path}\n"
    else
      search_abort(command, macroname) if abort
      return nil
    end
  end

  ################################################################a
  private

  def search_abort(missing, macroname)
    if option_name = @config_table .option_name(macroname)
      helping_option = "#{option_name} or --help"
    else
      helping_option = "--help"
    end
    STDERR .print "######################################################\n"
    STDERR .print "Fatal: could not find #{missing} .. aborting.\n"
    STDERR .print "Fatal: option #{helping_option} may help you.\n"
    STDERR .print "######################################################\n"
    exit(1)
  end

  def make_macro_name(name)
    return '@@MHC_' + name .sub(/^-*/, '') .tr('a-z-', 'A-Z_') + '@@'
  end

  def replace_keywords1(src_file_name, dst_file_name, keywords, mode = nil)
    src_file  = File .open(src_file_name, "r") or die "#{$!}\n"
    dst_file  = File .open(dst_file_name, "w") or die "#{$!}\n"

    src_contents = src_file .gets(nil); src_file .close
    keywords .each{|key, val| src_contents .gsub!(key, val)}

    if src_contents =~ /(@@MHC_[a-z\d_]+@@)/pin
      STDERR .print "Warn: keyword #{$1} was remiained in #{dst_file_name}.\n"
    end

    dst_file << src_contents
    dst_file .close 
    File .chmod(mode, dst_file_name) if mode
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
