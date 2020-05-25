#!/bin/bash

MODE="NORM"              # Default mode is NORM (can also be FAST or SLOW)
mode="norm"              # Default mode is norm (can also be fast or slow)

COMP="ghc"
COMPILER="$(which ghc)"  # Default is ~/.stack/programs/x86_64-linux/ghc-custom-asterius-8.8.3/bin/ghc
NODEJS="$(which node)"

TESTFILE="UNKNOWN" # Filename (can only be Main.hs or Main.lhs)
COMPILE_OPTS=""    # Compile-time options
RUNTIME_OPTS=""    # Runtime options
STDINFILE=""

function usageMessage {
  # Recommended: ./run.sh FAST ghc 2>&1 | tee log
  echo "Usage: ./run.sh <mode> <compiler>"
  echo "where <mode> is either FAST, or NORM, or SLOW"
  echo "and <compiler> is either ghc or ahc"
}

if [ $# -eq 2 ]; then
  MODE=$1
  mode=$(echo ${MODE} | tr '[:upper:]' '[:lower:]')
  if [ $2 == "ghc" ]; then
    COMP="ghc"
    COMPILER=$(which ghc)
  elif [ $2 == "ahc" ]; then
    COMP="ahc"
    COMPILER=$(which ahc-link)
  else
    usageMessage
    exit 1
  fi
else
  usageMessage
  exit 1
fi

# Testfile name
function setTestFile {
  if [ -f "Main.hs" ]; then
    TESTFILE="Main.hs"
  elif [ -f "Main.lhs" ]; then
    TESTFILE="Main.lhs"
  else
    exit 2
  fi
}

# Compile/link-time options
function setCompileOptions {
  local copts=""
  if [ -f "Main.COMPILE_OPTS" ]; then
    copts=$(<Main.COMPILE_OPTS)
  fi

  if [ ${COMP} == "ghc" ]; then
    COMPILE_OPTS=${copts}
  elif [ ${COMP} == "ahc" ]; then
    COMPILE_OPTS=""
    for word in $copts; do
      COMPILE_OPTS="${COMPILE_OPTS} --ghc-option=${word}"
    done # CONCATENATE THEM
    COMPILE_OPTS="${COMPILE_OPTS} --input-hs" # last
  else
    exit 3
  fi
}

# Runtime options
function setRuntimeOptions {
  RUNTIME_OPTS=""

  if [ ${COMP} == "ghc" ]; then
    RUNTIME_OPTS="${RUNTIME_OPTS} +RTS -V0 -RTS"
    if [ -f "RTS_EXTRA_OPTS" ]; then
      local extra_opts=$(<RTS_EXTRA_OPTS)
      RUNTIME_OPTS="${RUNTIME_OPTS} ${extra_opts}"
    fi
  fi

  if [ -f "${MODE}_OPTS" ]; then
    local ropts=$(<${MODE}_OPTS)
    RUNTIME_OPTS="${RUNTIME_OPTS} ${ropts}"
  fi
}

function compileTestfile {
  echo "EXECUTING: ${COMPILER} ${COMPILE_OPTS} $TESTFILE"
  ${COMPILER} ${COMPILE_OPTS} $TESTFILE
}

function runTest {
  local executable=""
  if [ ${COMP} == "ghc" ]; then
    executable="${PWD}/Main"
  elif [ ${COMP} == "ahc" ]; then
    executable="${NODEJS} ${PWD}/Main.mjs"
  else
    exit 3
  fi

  local input_file_name=$1.${mode}stdin
  if [ -f "${input_file_name}" ]; then
    echo "EXECUTING: ${executable} <${input_file_name} 1>${testname}.${COMP}.stdout 2>${testname}.${COMP}.stderr"
    ${executable} <${input_file_name} 1>${testname}.${COMP}.stdout 2>${testname}.${COMP}.stderr
  else
    echo "EXECUTING: ${executable} 1>${testname}.${COMP}.stdout 2>${testname}.${COMP}.stderr"
    ${executable} 1>${testname}.${COMP}.stdout 2>${testname}.${COMP}.stderr
  fi
}


echo "-------------------------------------------------------------------------------"
echo "                                PARAMETERS                                     "
echo "-------------------------------------------------------------------------------"
echo "Compiler : ${COMPILER}"
echo "Mode     : ${MODE}"
echo "Node     : ${NODEJS}"

echo "-------------------------------------------------------------------------------"
echo "                                    GHC                                        "
echo "-------------------------------------------------------------------------------"
for category in *; do
  if [ -d "${category}" -a "${category}" != "common" ]; then
    # For each category of tests:
    cd ${category} && echo "Entering $PWD ..." # Enter the category folder
    for testname in *; do
      if [ -d "${testname}" ]; then
        # For each test within this category
        cd ${testname} && echo "Entering $PWD ..." # Enter the test folder
        setTestFile         # set the TESTFILE variable to either Main.hs or Main.lhs
        setCompileOptions   # set the COMPILE_OPTIONS variable
        compileTestfile     # compile the file
        setRuntimeOptions   # set the RUNTIME_OPTS variable
        runTest ${testname} # run the test
        echo "Leaving $PWD ..." && cd ..             # Leave the test folder
      fi
    done
    echo "Leaving $PWD ..." && cd ..           # Leave the category folder
  fi
done

