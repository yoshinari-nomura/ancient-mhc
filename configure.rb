## configure.rb -- Guess values for system-dependent variables.
##
## Author:  MIYOSHI Masanori <miyoshi@quickhack.net>
##          Yoshinari Nomura <nom@quickhack.net>
## Created: 2000/7/12
## Revised: $Date: 2000/07/13 17:43:00 $

$LOAD_PATH .unshift('.')
require 'mhc-make'

################################################################a

local_config_table = [
  ['--pilot-link-lib',
    GetoptLong::OPTIONAL_ARGUMENT,
    "--pilot-link-lib=DIR    pilot-link lib in DIR",
    nil],

  ['--pilot-link-inc',
    GetoptLong::OPTIONAL_ARGUMENT,
    "--pilot-link-inc=DIR    pilot-link header in DIR",
    nil],

  ['--disable-palm',
    GetoptLong::OPTIONAL_ARGUMENT,
    "--disable-palm          do not require pilot-link",
    '0']
]

conf = MhcConfigure .new .set_user_config(ARGV, local_config_table)

conf .set_macro('@@MHC_XPM_PATH@@', 
		conf .macro('@@MHC_LIBDIR@@') + '/xpm'
		)

################################################################
## lib check

lib_search_path = ['/usr/local/lib', '/usr/local/pilot/lib']
inc_search_path = ['/usr/local/include', '/usr/local/pilot/include']

if conf .macro('@@MHC_DISABLE_PALM@@') == '0'
  if !(conf .search_library(lib_search_path, 
			    'pisock', 
			    'pi_socket',
			    '@@MHC_PILOT_LINK_LIB@@') and
       conf .search_include(inc_search_path,
			    'pi-dlp.h', 
			    '@@MHC_PILOT_LINK_INC@@'))
    STDERR .print "#######################################################\n"
    STDERR .print "Error: Could not find libpisock. "
    STDERR .print "Error: check path and set\n"
    STDERR .print "Error:   --pilot-link-lib=DIR and --pilot-link-inc=DIR.\n"
    STDERR .print "Error: or\n"
    STDERR .print "Error:  --disable-paml\n"
    STDERR .print "ERror: if you don't need palm support.\n"
    STDERR .print "#######################################################\n"
    exit(1)
  end
end

################################################################
## replace keywords.

infile_list = [
  'mhc-sync.in:0755', 
  'mhc2palm.in:0755', 
  'palm2mhc.in:0755', 
  'adb2mhc.in:0755', 
  'gemcal.in:0755', 
  'make.rb.in:0755', 
  'today.in:0755',
  'ruby-ext/lib/mhc-kconv.rb.in:0644', 
  'ruby-ext/lib/mhc-gtk.rb.in:0644', 
  'ruby-ext/extconf.rb.in:0755'
]

conf .replace_keywords(infile_list)

print "In ruby-ext/\n"
Dir .chdir('ruby-ext')
system('ruby', 'extconf.rb')
