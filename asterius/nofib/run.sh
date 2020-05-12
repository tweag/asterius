#!/bin/bash

MODE="NORM"  # Default mode is NORM (can also be FAST or SLOW)
COMPILER="$(which ghc)"

# echo "Asterius location : $(which ahc)"
# echo "GHC location      : $(which ghc)"
# echo "Mode              : ${MODE}"

for category in *; do
  if [ -d "${category}" -a "${category}" != "common" ]; then
    # For each category of tests:
    cd ${category} && echo "Entering $PWD ..." # Enter the category folder
    for testfolder in *; do
      if [ -d "${testfolder}" ]; then
        # For each test within this category
        cd ${testfolder} && echo "Entering $PWD ..." # Enter the test folder
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # BUILDING ALL TEST FILES
        for testfile in *.hs; do
          [ -f "$testfile" ] || break # If the file does not exist, move on..
          noext=${testfile%.hs} # Filename without extension

          # Retrieve the compile options for the current mode
          copts_file=${noext}.${MODE}_COMPILE_OPTS
          if [ -f "${copts_file}" ]; then
              copts=$(<${copts_file})
          else
              copts="" # no compiler options, use the empty string
          fi

          # Do the actual building
          # echo "${COMPILER} ${copts} -c $testfile -o ${noext}.o"
          ${COMPILER} ${copts} $testfile # -c $testfile -o ${noext}.o
          # TODO: IT DOES NOT COMPILER THEM IN ORDER :/
        done
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        echo "Leaving $PWD ..." && cd ..             # Leave the test folder
      fi
    done
    echo "Leaving $PWD ..." && cd ..           # Leave the category folder
  fi
done

