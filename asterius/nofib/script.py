#!/usr/bin/python3

# TODO:
#   1: Set timeouts
#   2: Avoid passing testdir around; it can be recreated using
#      working_directory + category + testname. See here:
#      https://stackoverflow.com/questions/14826888/python-os-path-join-on-a-list

import filecmp
import os
import shutil
import subprocess
import sys
import time

# CONSTANTS
# #############################################################################

valid_compilers = ["ghc", "ahc"]
valid_modes = ["fast", "norm", "slow"]
categories = ["imaginary", "real", "shootout", "spectral"]

blacklist = [
  ("real","eff-S"),              # Seems to run forever
  ("shootout","pidigits"),       # Seems to run forever
  ("real","pic"),                # JS heap out of memory
  ("real","scs"),                # JS heap out of memory
  ("shootout","fannkuch-redux"), # JS heap out of memory
  ("shootout","k-nucleotide")    # JS heap out of memory
  ]

# SETUP
# #############################################################################

working_directory = os.getcwd()

# Relative addresses are used later, so it is imperative that the working
# directory is set to ./asterius/nofib for the script to work. Basically where
# the script is located.
if not (os.path.basename(working_directory) == "nofib"):
  sys.exit("Script can only be executed in the nofib directory. Currently in {0}.".format(working_directory))

def findProgram(progName):
  if shutil.which(progName):
    return shutil.which(progName)
  else:
    sys.exit("Could not find program {0}!".format(progName))

# Find the (absolute addresses of the) needed executables (or die trying..)
ahc  = findProgram("ahc-link")
ghc  = findProgram("ghc")
node = findProgram("node")

# UTILITIES
# #############################################################################

def readEntireFile(filepath):
  handle = open(filepath,mode='r')
  contents = handle.read()
  handle.close()
  return contents

def readOptionsFromFile(filepath):
  if os.path.exists(filepath) and os.path.isfile(filepath):
    return readEntireFile(filepath).split()
  else:
    return []

def getCompilerExe(compiler):
  assert (compiler in valid_compilers)
  return (ghc if compiler == "ghc" else ahc)

# TEST COMPILATION
# #############################################################################

# Locate the (absolute address of the) Main file in a test directory. Sometimes
# this is an *.hs file, and other times this is an *.lhs file; we have to deal
# with both cases.
def findMain(testdir):
  if os.path.exists(os.path.join(testdir, "Main.hs")):
    return os.path.join(testdir, "Main.hs")
  elif os.path.exists(os.path.join(testdir, "Main.lhs")):
    return os.path.join(testdir, "Main.lhs")
  else:
    sys.exit("No Main.hs or Main.lhs found in {0}".format(testdir))

# Read the compile-time options from file Main.COMPILE_OPTS. If we are
# compiling using Asterius, wrap them in --ghc-option=, otherwise they remain
# unchanged. NOTE: The existence of the file is optional.
def readTestCompileOptions(testdir, compiler):
  assert (compiler in valid_compilers)
  # Read the compile-time options from Main.COMPILE_OPTS
  opt_filepath = os.path.join(testdir,"Main.COMPILE_OPTS")
  ghc_opts = readOptionsFromFile(opt_filepath)
  # For Asterius, wrap them in --ghc-option=
  if compiler == "ghc":
    return ghc_opts
  else:
    ahc_opts = []
    for option in ghc_opts:
      ahc_opts.append("--ghc-option={0}".format(option))
    return ahc_opts

# Compile a test
def compileTestFile(testdir, compiler):
  assert (compiler in valid_compilers)

  command = [getCompilerExe(compiler)] \
          + readTestCompileOptions(testdir, compiler) \
          + (["--input-hs"] if compiler == "ahc" else []) \
          + [findMain(testdir)]

  # Execute the command in the right directory
  current_directory = os.getcwd()
  os.chdir(testdir)
  print ("Compile test: {0}".format(command))
  proc = subprocess.Popen(
    command,
    universal_newlines=True
  )
  proc.wait() # TODO: Add timeout
  os.chdir(current_directory)

  if not (proc.returncode == 0):
    sys.exit("Non-zero exit code for test {0}, using compiler {1}!".format(testdir, compiler))

# TEST EXECUTION
# #############################################################################

# Read the runtime, mode-specific options from file <MODE>_OPTS, where <MODE>
# is one of (FAST, NORM, SLOW). NOTE: The existence of the file is optional.
def readModeSpecificOptions(testdir, mode):
  assert (mode in valid_modes)
  opt_filepath = os.path.join(testdir, mode.upper() + "_OPTS")
  return readOptionsFromFile(opt_filepath)

# Gather the runtime options for a specific test. The result of this operation
# varies depending on the compiler used. For GHC, the options include "+RTS -V0
# -RTS", whatever is in the (NOTE: optional) file RTS_EXTRA_OPTS, as well as
# the mode-specific inputs, as computed by readModeSpecificOptions (NOTE: also
# optional). For Asterius, only the last one is passed.
def readTestRuntimeOptions(testdir, mode, compiler):
  assert (compiler in valid_compilers)
  if compiler == "ghc":
    return ["+RTS", "-V0", "-RTS"] \
         + readOptionsFromFile(os.path.join(testdir, "RTS_EXTRA_OPTS")) \
         + readModeSpecificOptions(testdir, mode)
  else:
    return readModeSpecificOptions(testdir, mode)

# Execute a test (given a compiler and a mode). Returns the paths where the
# stdout and stderr were written. Fails if the executio of the test failed.
def runTestFile(testdir, category, testname, compiler, mode):
  assert (compiler in valid_compilers)
  assert (mode in valid_modes)

  # Input file
  stdinfilepath = os.path.join(testdir,"{0}.{1}stdin".format(testname, mode.lower()))
  if not (os.path.exists(stdinfilepath) and os.path.isfile(stdinfilepath)):
    stdinfilehandle = None
  else:
    stdinfilehandle = open(stdinfilepath, mode='r')

  # Output file
  stdoutfilepath = os.path.join(testdir,"{0}.{1}.stdout".format(testname, compiler))
  stdoutfilehandle = open(stdoutfilepath, mode='w')

  # Error file
  stderrfilepath = os.path.join(testdir,"{0}.{1}.stderr".format(testname, compiler))
  stderrfilehandle = open(stderrfilepath, mode='w')

  command = ([node] if compiler == "ahc" else []) \
          + [os.path.join(testdir, "Main.mjs") if compiler == "ahc" else os.path.join(testdir, "Main")] \
          + readTestRuntimeOptions(testdir, mode, compiler)

  print ("Run test: {0}".format(command))

  # Switch to the test directory
  current_directory = os.getcwd()
  os.chdir(testdir)

  start = time.time() # Time before execution

  # Run the test
  proc = subprocess.Popen(
    command,
    stdin=stdinfilehandle,
    stdout=stdoutfilehandle,
    stderr=stderrfilehandle,
    universal_newlines=True
  )
  proc.wait() # TODO: Add timeout
  stdoutfilehandle.flush()
  stderrfilehandle.flush()

  end = time.time() # Time after execution

  # Return to the previous directory
  os.chdir(current_directory)

  # Fail immediately if execution failed
  if not (proc.returncode == 0):
    print("Non-zero exit code for {0}!".format(testdir), file=sys.stderr)
    sys.exit(readEntireFile(stderrfilepath))

  with open(os.path.join(working_directory, "TIMES.csv"), "a+") as timesfile:
    timesfile.write("{0},{1},{2},{3},{4}\n".format(category, testname, compiler, mode, end - start))

  return (stdoutfilepath, stderrfilepath)

# Would be nice..
#   $(which time) --format "%E,%U,%S" --output=asdftest --append du -h ~

# #############################################################################

def main(mode):
  for category in categories:
    category_path = os.path.join(working_directory, category)
    for testname in os.listdir(category_path):
      testdir = os.path.join(category_path, testname)
      if os.path.isdir(testdir):
        # Skip the problematic tests
        if (category, testname) in blacklist:
          print ("Skipping test {0}/{1}.".format(category, testname))
        else:
          # Build and run with GHC
          compileTestFile(testdir, "ghc")
          ghc_stdout, ghc_stderr = runTestFile(testdir, category, testname, "ghc", mode)
          # Build and run with Asterius
          compileTestFile(testdir, "ahc")
          ahc_stdout, ahc_stderr = runTestFile(testdir, category, testname, "ahc", mode)
          # Fail if stdouts or stderrs differ
          if not filecmp.cmp(ghc_stdout, ahc_stdout):
            sys.exit("Stdouts for {0} differ!".format(testdir))
          if not filecmp.cmp(ghc_stderr, ahc_stderr):
            sys.exit("Stderrs for {0} differ!".format(testdir))

# CLEANUP
# #############################################################################

def shoulddelete(filename):
  to_delete_filenames = set([
    "Main",
    "Main.mjs",
    "Main.req.mjs",
    "Main.wasm",
    "Main.wasm.mjs",
    "default.mjs"
    ])
  return (
      (filename.startswith("rts.") and filename.endswith(".mjs"))
      or filename.endswith(".hi")
      or filename.endswith(".o")
      or filename.endswith(".ahc.stderr")
      or filename.endswith(".ahc.stdout")
      or filename.endswith(".ghc.stderr")
      or filename.endswith(".ghc.stdout")
      or filename in to_delete_filenames
    )

def cleanup():
  for category in categories:
    category_path = os.path.join(working_directory, category)
    for testname in os.listdir(category_path):
      testdir = os.path.join(category_path, testname)
      if os.path.isdir(testdir):
        for item in os.listdir(testdir):
          if shoulddelete(item):
            os.remove(os.path.join(testdir,item))

  # Relative paths
  explicit_removals = [
    "./real/fulsom/Bah",
    "./real/compress/Lzw",
    "./real/maillist/runtime_files/fast.tex",
    "./real/maillist/runtime_files/norm.tex",
    "./real/maillist/runtime_files/slow.tex",
    "./TIMES.csv"
    ]

  for filename in explicit_removals:
    if os.path.exists(filename):
      os.remove(filename)

# ENTRY POINT
# #############################################################################

if __name__ == "__main__":
  if len(sys.argv) < 2:
    sys.exit("Too few arguments")
  elif sys.argv[1] == "clean":
    cleanup()
  elif sys.argv[1] in valid_modes:
    cleanup() # cleanup first
    main(sys.argv[1])
  else:
    sys.exit("Invalid arguments.")
