#!/bin/bash

# Quick and dirty! Should probably try libclang... 

perl -n -e "/^(SP[A-Z][a-z]\S*|void) SP[A-Z][a-z]\S*\(.*\)/sm and print \"\$&\n\";" /Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include/Spatial/*.h | sort | uniq 
