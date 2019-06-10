#!/usr/bin/env python3
# This script takes an unformatted .csv file produced by the GHC-testsuite
# runner in asterius and produces well-formatted ascii tables.
# For usage, invoke `--help`.
import argparse
import json
import sys
from terminaltables import AsciiTable
import os
import pandas as pd
import numpy as np

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


if __name__ == "__main__":
    p = parse(sys.argv[1:])


    data = pd.read_csv(p.jsonpath, 
            dtype={"trOutcome": object, "trPath": object, "trErrorMessage": str})
    # replace nan with string for the possibly empty error messages.
    data = data.replace(np.nan, '', regex=True)
    data['trErrorMessage'] = data['trErrorMessage'].apply(unescape_ascii)


    # Get ASCII printing working.
    out_ascii = AsciiTable(data.values.tolist()).table
    print(out_ascii)


