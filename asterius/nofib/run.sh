#!/bin/bash

MODE=$1 # FAST / NORM / SLOW
COMPILER=$2 # "$(which ghc)" / "$(which ahc)"

# TODO: Have defaults and conditionally set from arguments

MODE="NORM"  # Default mode is NORM (can also be FAST or SLOW)
COMPILER="$(which ghc)"

echo "-------------------------------------------------------------------------------"
echo "                                PARAMETERS                                     "
echo "-------------------------------------------------------------------------------"
echo "Compiler : ${COMPILER}"
echo "Mode     : ${MODE}"

echo "-------------------------------------------------------------------------------"
echo "                                 BUILDING                                      "
echo "-------------------------------------------------------------------------------"

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
          echo "EXECUTING: ${COMPILER} ${copts} $testfile"
          ${COMPILER} ${copts} $testfile # -c $testfile -o ${noext}.o
          # TODO: IT DOES NOT COMPILER THEM IN ORDER :/
        done
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # RUNNING ALL TEST FILES

        # Retrieve the runtime options for the current mode
        ropts_file=${MODE}_OPTS
        if [ -f "${ropts_file}" ]; then
          ropts=$(<${ropts_file})
        else
          ropts="" # no runtime options options, use the empty string
        fi

        # Retrieve possible stdin contents for the current mode
        input_file_name=${testfolder}.$(echo ${MODE} | tr '[:upper:]' '[:lower:]')stdin # e.g. primetest.faststdin
        if [ -f "${input_file_name}" ]; then
          infile="<${input_file_name}"
        else
          infile="" # no stdin supplied, use the empty string
        fi

        output_file_name=${testfolder}.stdout # e.g. primetest.stdout

        # Do the actual running
        echo "EXECUTING: ./Main ${ropts} ${infile} >${output_file_name}"
        ./Main ${ropts} ${infile} >${output_file_name}
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        echo "Leaving $PWD ..." && cd ..             # Leave the test folder
      fi
    done
    echo "Leaving $PWD ..." && cd ..           # Leave the category folder
  fi
done

