#!/bin/bash

echo "Asterius location : $(which ahc)"
echo "GHC location      : $(which ghc)"

for category in *; do
  if [ -d "${category}" -a "${category}" != "common" ]; then
    # For each category of tests:
    cd ${category} && echo "Entering $PWD ..." # Enter the category folder
    for testfolder in *; do
      if [ -d "${testfolder}" ]; then
        # For each test within this category
        cd ${testfolder} && echo "Entering $PWD ..." # Enter the test folder
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        ghc Main.hs
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        echo "Leaving $PWD ..." && cd ..             # Leave the test folder
      fi
    done
    echo "Leaving $PWD ..." && cd ..           # Leave the category folder
  fi
done

