#!/usr/local/bin/ruby
## configure.rb -- Guess values for system-dependent variables.
##
## Author:  MIYOSHI Masanori <miyoshi@quickhack.net>
##          Yoshinari Nomura <nom@quickhack.net>
## Created: 2000/7/12
## Revised: $Date: 2000/07/18 04:33:26 $

$LOAD_PATH .unshift('.')
require 'mhc-make'

################################################################a

local_config_table = [
  ['--pilot-link-lib', '@@MHC_PILOT_LINK_LIB@@',
    GetoptLong::REQUIRED_ARGUMENT,
    "--pilot-link-lib=DIR    pilot-link lib in DIR",
    nil],

  ['--pilot-link-inc', '@@MHC_PILOT_LINK_INC@@',
    GetoptLong::REQUIRED_ARGUMENT,
    "--pilot-link-inc=DIR    pilot-link header in DIR",
    nil],

  ['--disable-palm', '@@MHC_DISABLE_PALM@@',
    GetoptLong::NO_ARGUMENT,
    "--disable-palm          do not require pilot-link",
    '0']
]

conf = MhcConfigure .new(local_config_table) .parse_argv

conf['@@MHC_XPM_PATH@@'] = conf['@@MHC_LIBDIR@@'] + '/xpm'

################################################################
## command check

conf .search_command('ruby', '@@MHC_RUBY_PATH@@',   false, true)
conf .search_command('emacs', '@@MHC_EMACS_PATH@@', false, true)

################################################################
## lib check

lib_search_path = ['/usr/local/lib', '/usr/local/pilot/lib']
inc_search_path = ['/usr/local/include', '/usr/local/pilot/include']

if conf['@@MHC_DISABLE_PALM@@'] == '0'
  conf .search_library(lib_search_path, 
		       'pisock', 
		       'pi_socket',
		       '@@MHC_PILOT_LINK_LIB@@', false, true)
  conf .search_include(inc_search_path,
		       'pi-dlp.h', 
		       '@@MHC_PILOT_LINK_INC@@', false, true)
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

file = File .open('configure.log', 'w')
conf .each_macro{|key, val|
  file .print "#{key} => #{val}\n"
}

conf .replace_keywords(infile_list)

print "In ruby-ext/\n"
Dir .chdir('ruby-ext')
system('ruby', 'extconf.rb')

exit 0
