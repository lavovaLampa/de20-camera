#! /usr/bin/env fish

set BASEDIR (dirname (status -f))
set GHDL_SCRIPT_LOCATION "/usr/lib/ghdl/vendors/compile-osvvm.sh"
set OSVVM_LOCATION (realpath "$BASEDIR/lib/OSVVM")

if test -f "$GHDL_SCRIPT_LOCATION" -a -d "$OSVVM_LOCATION"
    if test -d "$BASEDIR/osvvm"
        echo "library already compiled"
    else
        "$GHDL_SCRIPT_LOCATION" --src "$OSVVM_LOCATION" -c --osvvm
    end
else
    echo "Couldn't find ghdl compile function and/or library directory"
end
