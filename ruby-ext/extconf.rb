
require 'mkmf'

ver = VERSION .split('.') .filter{|i| format("%02d", i)} .join('')

$LDFLAGS    = "-L/usr/local/pilot/lib"
$CFLAGS     = "-I/usr/local/pilot/include"

if ver >= "010300"
  $CFLAGS += " -DNEW_NAMING"
end

if have_library("pisock", "pi_socket")
  if have_header("pi-dlp.h")
    create_makefile("mhc_pilib")
  end
end
