#!/bin/bash

for f in ./*/*/*.ghc.stdout; do
  testname=${f%.ghc.stdout}

  # Stdout work
  ghcout=${testname}.ghc.stdout
  ahcout=${testname}.ahc.stdout

  stdout_ok="missing"
  if [ -f ${ahcout} ]; then
    if diff ${ghcout} ${ahcout} >/dev/null; then
        stdout_ok="true" # echo "  STDOUT OK"
    else
        stdout_ok="false" # echo "  STDOUT FAIL"
    fi
  fi

  # Stderr work
  ghcerr=${testname}.ghc.stderr
  ahcerr=${testname}.ahc.stderr

  stderr_ok="missing"
  if [ -f ${ahcerr} ]; then
    if diff ${ghcerr} ${ahcerr} >/dev/null; then
        stderr_ok="true" # echo "  STDERR OK"
    else
        stderr_ok="false" # echo "  STDERR FAIL"
    fi
  fi

  # Print results
  if [[ "$stdout_ok" == "missing" || "$stderr_ok" == "missing" ]]; then
    echo "${testname} ( MISSING )"
  elif [[ "$stdout_ok" == "true" && "$stderr_ok" == "true" ]]; then
    echo "${testname} ( OK )"
  elif [[ "$stderr_ok" == "false" ]]; then
    echo "${testname} ( STDERR MISMATCH )"
  elif [[ "$stdout_ok" == "false" ]]; then
    echo "${testname} ( STDOUT MISMATCH )"
  fi

done

# for f in ./*/*/*.ghc.stdout; do
#   ghcout=${f}
#   ahcout=${f%.ghc.stdout}.ahc.stdout
#   if [ -f ${ahcout} ]; then
#     if diff ${ghcout} ${ahcout} >/dev/null; then
#         echo "Passed (${ahcout})"
#     else
#         echo "Failed (${ahcout})"
#     fi
#   else
#     echo "Missing (${ahcout})"
#   fi
# done

