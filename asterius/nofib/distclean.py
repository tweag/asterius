#!/usr/bin/python3

import os
import shutil
import subprocess
import sys

# CONSTANTS
# #############################################################################

valid_compilers = ["ghc", "ahc"]
valid_modes = ["fast", "norm", "slow"]
categories = ["imaginary", "real", "shootout", "spectral"]

# SETUP
# #############################################################################

working_directory = os.getcwd()

# if working_directory
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
time = findProgram("time")
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
  if compiler == "ghc":
    return ghc
  else:
    return ahc

# #############################################################################

def findMain(testdir):
  if os.path.exists(os.path.join(testdir, "Main.hs")):
    return os.path.join(testdir, "Main.hs")
  elif os.path.exists(os.path.join(testdir, "Main.lhs")):
    return os.path.join(testdir, "Main.lhs")
  else:
    sys.exit("No Main.hs or Main.lhs found in {0}".format(testdir))

# READ COMPILE-TIME OPTIONS
# #############################################################################

# Read the compile-time options from Main.COMPILE_OPTS. If we are compiling
# using Asterius, wrap them in --ghc-option=.
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

# READ RUNTIME OPTIONS
# #############################################################################

def readModeSpecificOptions(testdir, mode):
  assert (mode in valid_modes)
  opt_filepath = os.path.join(testdir, mode.upper() + "_OPTS")
  return readOptionsFromFile(opt_filepath)

def readTestRuntimeOptions(testdir, mode, compiler):
  assert (compiler in valid_compilers)
  if compiler == "ghc":
    return ["+RTS", "-V0", "-RTS"] \
         + readOptionsFromFile(os.path.join(testdir, "RTS_EXTRA_OPTS")) \
         + readModeSpecificOptions(testdir, mode)
  else:
    return readModeSpecificOptions(testdir, mode)

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
  proc.wait()
  os.chdir(current_directory)

  print(proc.returncode)
  if not (proc.returncode == 0):
    sys.exit("Non-zero exit code for test {0}, using compiler {1}!".format(testdir, compiler))

# #############################################################################

def runTestFile(testdir, testname, compiler, mode):
  assert (compiler in valid_compilers)
  assert (mode in valid_modes)

  # Input file
  stdinfilepath = os.path.join(testdir,"{0}.{1}stdin".format(testname, mode.lower()))
  if not (os.path.exists(stdinfilepath) and os.path.isfile(stdinfilepath)):
    stdinfilehandle = None
  else:
    stdinfilehandle = open(stdinfilepath, mode='r')

  stdoutfilepath = os.path.join(testdir,"{0}.{1}.stdout".format(testname, compiler))
  stdoutfilehandle = open(stdoutfilepath, mode='w')

  stderrfilepath = os.path.join(testdir,"{0}.{1}.stderr".format(testname, compiler))
  stderrfilehandle = open(stderrfilepath, mode='w')

  command = ([node] if compiler == "ahc" else []) \
          + [os.path.join(testdir, "Main.mjs") if compiler == "ahc" else os.path.join(testdir, "Main")] \
          + readTestRuntimeOptions(testdir, mode, compiler)

  print ("Run test: {0}".format(command))

  # Switch to the test directory
  current_directory = os.getcwd()
  os.chdir(testdir)
  # Run the test
  proc = subprocess.Popen(
    command,
    stdin=stdinfilehandle,
    stdout=stdoutfilehandle,
    stderr=stderrfilehandle,
    universal_newlines=True
  )
  proc.wait()
  stdoutfilehandle.flush()
  stderrfilehandle.flush()
  # Return to the previous directory
  os.chdir(current_directory)

  print(proc.returncode)
  if not (proc.returncode == 0):
    sys.exit("Non-zero exit code for {0}!".format(testdir))

# #############################################################################

def runAndTimeTest(category, testname, compiler, mode):
  time_parameters = [
    time,
    "--format",
    "{0},{1},{2},{3},%E,%U,%S".format(category, testname, compiler, mode),
    "--output=../../aggregate.txt",
    "--append"
    ]

# Would be nice..
#   $(which time) --format "%E,%U,%S" --output=asdftest --append du -h ~

# #############################################################################

def main(mode):
  for category in categories:
    category_path = os.path.join(working_directory, category)
    for testname in os.listdir(category_path):
      testdir = os.path.join(category_path, testname)
      if os.path.isdir(testdir):
        compileTestFile(testdir, "ahc") # PARAMETERIZE
        runTestFile(testdir, testname, "ahc", mode)

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
    "./real/maillist/runtime_files/slow.tex"
    ]

  for filename in explicit_removals:
    if os.path.exists(filename):
      os.remove(filename)

# #############################################################################

if __name__ == "__main__":
  if len(sys.argv) < 2:
    sys.exit("Too few arguments")
  elif sys.argv[1] == "clean":
    cleanup()
  elif sys.argv[1] == "fast":
    cleanup() # cleanup first
    main("fast")



