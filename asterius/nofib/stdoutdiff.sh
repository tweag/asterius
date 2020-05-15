#!/bin/bash

for f in ./*/*/*.ghc.stdout; do
  ghcout=${f}
  ahcout=${f%.ghc.stdout}.ahc.stdout
  if [ -f ${ahcout} ]; then
    if diff ${ghcout} ${ahcout} >/dev/null; then
        echo "Passed (${ahcout})"
    else
        echo "Failed (${ahcout})"
    fi
  else
    echo "Missing (${ahcout})"
  fi
done

