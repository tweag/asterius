#!/usr/bin/env python3
import argparse
import json
import sys
import tabulate

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
    with open(p.jsonpath, "r") as f:
        reports = json.load(f)

    rows = []
    for r in reports:
        print(r)
        ix =r['trErrorMessage'].find('γ')
        if ix == -1: continue
        errmsg = unescape_ascii(r['trErrorMessage'][ix+1:])
        rows.append([trPath, )
        print('-----')
        print(errmsg)

