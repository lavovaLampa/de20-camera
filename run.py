#! /usr/bin/env python3

from vunit import VUnit

# Create VUnit instance by parsing command line arguments
vu = VUnit.from_argv()

# Create library 'lib'
lib = vu.add_library("lib")

# Add all files ending in .vhd in current working directory to library
lib.add_source_files("src/*.vhd")
lib.add_source_files("src/common_pkgs/*.vhd")
lib.add_source_files("tb/*.vhd")
lib.add_source_files("tb/model/*.vhd")
lib.add_source_files("tb/common_pkgs/*.vhd")

# Run vunit function
vu.main()
