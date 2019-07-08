#!/usr/bin/env python3
# This script takes an unformatted .csv file produced by the GHC-testsuite
# runner in asterius and produces ascii tables collecting all tests
# that fail at a particular error 
# note: very hacky.
# for usage, use --help
import argparse
import json
import sys
from terminaltables import AsciiTable
from collections import defaultdict
import os
import pandas as pd
import numpy as np
import re

def parse(s):
    p = argparse.ArgumentParser()
    p.add_argument("jsonpath", help="path to input JSON file")
    return p.parse_args(s)

def unescape_ascii(s):
    # We need to unescape the ASCII that is embedded in the string
    # code stolen from:
    # https://stackoverflow.com/questions/4020539/process-escape-sequences-in-a-string-in-python
    # encode the ASCII string into utf-8, and then decode as if it were
    # unicode
    return bytes(s, "utf-8").decode("unicode_escape")



def strip_wasm_line_numbers(s):
    """
    Given input:

    RuntimeError: memory access out of bounds                                                                                                                                               |
        at stg_noDuplicatezh (wasm-function[3056]:20)                                                                                                                                       |
        at scheduleWaitThread (wasm-function[2957]:196)                                                                                                                                     |
        at rts_evalLazyIO (wasm-function[2924]:27)                                                                                                                                          |
        at main (wasm-function[2903]:12)                                                                                                                                                    |
        at evalmachine.<anonymous>:1:43                                                                                                                                                     |
        at Script.runInContext (vm.js:134:20)                                                                                                                                               |
        at Proxy.runInContext (vm.js:297:6)                                                                                                                                                 |
        at Worker.<anonymous> (file:///tmp/asterius/.stack-work/install/x86_64-linux/ghc-8.9/8.9.20190403/share/x86_64-linux-ghc-8.9.20190403/inline-js-core-0.0.1/jsbits/eval.mjs:71:32)   |
        at Worker.emit (events.js:200:13)                                                                                                                                                   |
        at MessagePort.<anonymous> (internal/worker.js:142:55)"}                                                                                                                            |

    Produce output:

    RuntimeError: memory access out of bounds                                                                                                                                               |
        at stg_noDuplicatezh 
        at scheduleWaitThread 
        at rts_evalLazyIO 
        at main 
        at evalmachine.<anonymous>:1:43                                                                                                                                                     |
        at Script.runInContext 
        at Proxy.runInContext 
        at Worker.<anonymous> 
        at Worker.emit 
        at MessagePort.<anonymous> 


    Also delete all messages after expected: RunSuccess
    """
    return re.sub("\(.*\)", "", s)

if __name__ == "__main__":
    p = parse(sys.argv[1:])

    data = pd.read_csv(p.jsonpath, 
            dtype={"trOutcome": object, "trPath": object, "trErrorMessage": str})
    # replace nan with string for the possibly empty error messages.
    data = data.replace(np.nan, '', regex=True)
    data['trErrorMessage'] = data['trErrorMessage'].apply(unescape_ascii)
    message2path = defaultdict(str)

    for (ix, row) in data.iterrows():
        err = row['trErrorMessage']
        # if there is no error message, the script succeeded.
        if not err: continue
        err = err.split("RuntimeError")[-1]
        err = strip_wasm_line_numbers(err)
        # kill extraneous data
        err = err.split("at MessagePort.<anonymous>")[0]
        message2path[err] += "\n" + row['trPath']

    l = []
    for k in message2path:
        l.append([k, message2path[k]])

    # print to table.
    out_ascii = AsciiTable(l).table
    print(out_ascii)
