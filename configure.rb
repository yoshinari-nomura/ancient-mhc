## configure.rb -- Guess values for system-dependent variables.
##
## Author:  MIYOSHI Masanori <miyoshi@quickhack.net>
##
## Created: 2000/7/12
##

require 'ftools'
require 'rbconfig'
require 'getoptlong'
require 'kconv'

include Config

#detect default kanji code
sjis_host_regexp = /cygwin|mingw32|os2_emx|sharp-human/
default_kcode = (sjis_host_regexp =~ RUBY_PLATFORM) ? 'SJIS' : 'EUC'

bindir = CONFIG["bindir"]
libdir = File::join(CONFIG["libdir"], "ruby",
		    CONFIG["MAJOR"] + "." + CONFIG["MINOR"])
rubyexec = bindir + "/ruby"
disable_ext = false

# print usage
def print_usage()
  print <<END
Usage: configure.rb [options]
Options: [defaults in brackets after descriptions]
Configuration:
  --kcode=FILE            kanji code (EUC, JIS, SJIS)
  --bindir=DIR            user executables in DIR
  --libdir=DIR            script libraries in DIR
  --with-ruby=PATH        absolute path of ruby executable
  --disable-ext           do not create ext
  --help                  print this message
END
end

# parse arguments
parser = GetoptLong::new()
parser.set_options(['--kcode', GetoptLong::OPTIONAL_ARGUMENT],
		   ['--bindir', GetoptLong::OPTIONAL_ARGUMENT],
		   ['--libdir', GetoptLong::OPTIONAL_ARGUMENT],
		   ['--with-ruby', GetoptLong::OPTIONAL_ARGUMENT],
		   ['--disable-ext', GetoptLong::NO_ARGUMENT],
		   ['--help', GetoptLong::NO_ARGUMENT])

begin
  parser.each_option do |name, arg|
    case name
    when "--kcode"
      default_kcode = arg
    when "--bindir"
      bindir = arg
    when "--libdir"
      libdir = arg
    when "--with-ruby"
      rubyexec = arg
    when "--disable-ext"
      disable_ext = true
    when "--help"
      print_usage()
      exit(1)
    end
  end
rescue
  print_usage()
  exit(1)
end

# check if kanji code is valid
if not ["EUC", "JIS", "SJIS"].member?(default_kcode)
  print "error: invalid kcode!\n"
  print_usage()
  exit(1)
end

conversion_table = {
  "@@mhc_default_kcode@@" => "Kconv::" + default_kcode,
  "@@mhc_ruby_path@@" => rubyexec,
  "@@mhc_bindir@@" => bindir,
  "@@mhc_libdir@@" => libdir,
  "@@mhc_disable_ext@@" => disable_ext,
}

files = %w(make.rb ruby-ext/make.rb ruby-ext/lib/make.RB) 
while filename = files.shift()
  fin = File::open(filename + ".in")
  fout = File::open(filename, "w")
  print "configuring: " + filename + "\n"
  while line = fin.gets()
    conversion_table.each { |key, value|
      gsub!(key, value)
    }
    fout.puts(line)
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

### configure.rb ends here
