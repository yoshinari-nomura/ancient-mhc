
require 'mkmf'

################################################################
# set search path.
#

lib_search_path = [
  "/usr/local/pilot/lib",
  "/usr/local/lib"
]

inc_search_path = [
  "/usr/local/pilot/include",
  "/usr/local/include"
]

################################################################
# search inclide dir

found_inc_path = nil

inc_search_path .each{|inc_path|
  print "In #{inc_path} .. "
  $CFLAGS  = "-I#{inc_path}"
  if have_header("pi-dlp.h")
    found_inc_path = inc_path

    # avoiding ruby 1.4.3 bug.
    $defs .push($defs .pop .sub!(/-DHAVE_PI-DLP_H/, '-DHAVE_PI_DLP_H'))
    break
  end
}

################################################################
# search lib dir

found_lib_path = nil

lib_search_path .each{|lib_path|
  print "In #{lib_path} .. "
  $LDFLAGS = "-L#{lib_path}"
  if have_library("pisock", "pi_socket")
    found_lib_path = lib_path
    break
  end
}

################################################################
# ruby versoin check

ver   = VERSION .split('.') .filter{|i| format("%02d", i)} .join('')
$CFLAGS += " -DNEW_NAMING"  if ver >= "010300"

if found_lib_path .nil? or found_inc_path .nil?
  create_pilot_lib = false
else
  create_pilot_lib = true
end
  
################################################################
# crate make file.

if create_pilot_lib
  CONFIG["LDSHARED"] = "LD_RUN_PATH=#{found_lib_path} " + CONFIG["LDSHARED"]
  create_makefile("mhc_pilib")
else
  STDERR .print "\n"
  STDERR .print "##################### Warning ##########################\n"
  STDERR .print "## pilot-link was not found in your system.           ##\n"
  STDERR .print "## You are building the MHC without Palm support.     ##\n"
  STDERR .print "##                                                    ##\n"
  STDERR .print "## If you want the Palm support,                      ##\n"
  STDERR .print "## install pilot-link, edit inc_search_path and       ##\n"
  STDERR .print "## lib_search_path in rubyext.conf                    ##\n"
  STDERR .print "##################### Warning ##########################\n"
  mfile = open("Makefile", "w")
  mfile .print <<EOMK

libdir = #{$libdir}
RUBY   = #{CONFIG["ruby_install_name"]}

warn:
\t@echo "Nothing to do. Type \\`\\`make install''."

clean:
\t@rm -f Makefile
  
install:
EOMK
  if ver >= "010404"
    install_rb(mfile, "$(libdir)") 
  else
    install_rb(mfile)
  end
  mfile .print "\n"
  mfile .close
end
