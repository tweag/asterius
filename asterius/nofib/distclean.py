#!/usr/bin/python3

import os
import shutil
import subprocess

working_directory = os.getcwd()

valid_compilers = ["ghc", "ahc"]
valid_modes = ["fast", "norm", "slow"]
categories = ["imaginary", "real", "shootout", "spectral"]



# Find the needed executables
ahc = shutil.which("ahc-link")
ghc = shutil.which("ghc")
time = shutil.which("time")
node = shutil.which("node")

# if you're interested:
#   if shutil.which("node"):
#     node = shutil.which("node")
#   else:
#     print ("Couldn't find node!")
#     exit (1)

print (working_directory)
print (valid_modes)

print ("ahc: {0}".format(ahc))
print ("ghc: {0}".format(ghc))
print ("time: {0}".format(time))
print ("node: {0}".format(node))

# # Testfile name
# function setTestFile {
#   if [ -f "Main.hs" ]; then
#     TESTFILE="Main.hs"
#   elif [ -f "Main.lhs" ]; then
#     TESTFILE="Main.lhs"
#   else
#     exit 2
#   fi
# }

def findMain(testdir):
  if os.path.exists(os.path.join(testdir, "Main.hs")):
    return "Main.hs" # os.path.join(testdir, "Main.hs")
  elif os.path.exists(os.path.join(testdir, "Main.lhs")):
    return "Main.lhs" # os.path.join(testdir, "Main.lhs")
  else:
    print ("No Main.hs or Main.lhs found in {0}".format(testdir))
    exit (1)

def findExecutable(testdir, compiler):
  assert (compiler in valid_compilers)
  if compiler == "ghc":
    filename = "Main"
  else:
    filename = "Main.mjs"
  filepath = os.path.join(testdir, filename)
  if os.path.exists(filepath):
    return filepath
  else:
    print ("No {0} found in {1}".format(filename, testdir))
    exit (1)


#   local executable=""
#   if [ ${COMP} == "ghc" ]; then
#     executable="${PWD}/Main"
#   elif [ ${COMP} == "ahc" ]; then
#     executable="${NODEJS} ${PWD}/Main.mjs"
#   else
#     exit 3
#   fi


def readEntireFile(filepath):
  handle = open(filepath,mode='r')
  contents = handle.read()
  handle.close()
  return contents

def readOptionsFromFile(filepath):
  if os.path.exists(filepath) and os.path.isfile(filepath):
    return readEntireFile(filepath)
  else:
    return ""

# # Compile/link-time options
# function setCompileOptions {
#   local copts=""
#   if [ -f "Main.COMPILE_OPTS" ]; then
#     copts=$(<Main.COMPILE_OPTS)
#   fi
#
#   if [ ${COMP} == "ghc" ]; then
#     COMPILE_OPTS=${copts}
#   elif [ ${COMP} == "ahc" ]; then
#     COMPILE_OPTS=""
#     for word in $copts; do
#       COMPILE_OPTS="${COMPILE_OPTS} --ghc-option=${word}"
#     done # CONCATENATE THEM
#     COMPILE_OPTS="${COMPILE_OPTS} --input-hs" # last
#   else
#     exit 3
#   fi
# }

def readTestCompileOptionsGHC(testdir):
  opt_filepath = os.path.join(testdir,"Main.COMPILE_OPTS")
  return readOptionsFromFile(opt_filepath)

def readTestCompileOptionsAHC(testdir):
  opt_filepath = os.path.join(testdir,"Main.COMPILE_OPTS")
  ghc_opts = readOptionsFromFile(opt_filepath)
  ahc_opts = ""
  for option in ghc_opts.split():
    ahc_opts += " --ghc-option={0}".format(option)
  return ahc_opts
  # TODO: CAREFUL: --input-hs has not been used

def readTestCompileOptions(testdir, compiler):
  assert (compiler in valid_compilers)
  if compiler == "ghc":
    return readTestCompileOptionsGHC(testdir)
  else:
    return readTestCompileOptionsAHC(testdir)


# # Runtime options
# function setRuntimeOptions {
#   RUNTIME_OPTS=""
#
#   if [ ${COMP} == "ghc" ]; then
#     RUNTIME_OPTS="${RUNTIME_OPTS} +RTS -V0 -RTS"
#     if [ -f "RTS_EXTRA_OPTS" ]; then
#       local extra_opts=$(<RTS_EXTRA_OPTS)
#       RUNTIME_OPTS="${RUNTIME_OPTS} ${extra_opts}"
#     fi
#   fi
#
#   if [ -f "${MODE}_OPTS" ]; then
#     local ropts=$(<${MODE}_OPTS)
#     RUNTIME_OPTS="${RUNTIME_OPTS} ${ropts}"
#   fi
# }

def readModeSpecificOptions(testdir, mode):
  assert (mode in valid_modes)
  opt_filepath = os.path.join(testdir, mode.upper() + "_OPTS")
  return readOptionsFromFile(opt_filepath)

def readTestRuntimeOptionsGHC(testdir, mode):
  assert (mode in valid_modes)
  extra_opts_filepath = os.path.join(testdir, "RTS_EXTRA_OPTS")
  rts_opts = " ".join([
    "+RTS -V0 -RTS",
    readOptionsFromFile(extra_opts_filepath),
    readModeSpecificOptions(testdir, mode)
    ])
  return rts_opts

def readTestRuntimeOptionsAHC(testdir, mode):
  assert (mode in valid_modes)
  rts_opts = readModeSpecificOptions(testdir, mode)
  return rts_opts

def readTestRuntimeOptions(testdir, mode, compiler):
  assert (compiler in valid_compilers)
  if compiler == "ghc":
    return readTestRuntimeOptionsGHC(testdir, mode)
  else:
    return readTestRuntimeOptionsAHC(testdir, mode)

def getCompilerExe(compiler):
  assert (compiler in valid_compilers)
  if compiler == "ghc":
    return shutil.which("ghc")
  else:
    return shutil.which("ahc-link")

def compileTestFile(testdir, compiler):
  assert (compiler in valid_compilers)

  compiler_exe = getCompilerExe(compiler)
  compile_opts = readTestCompileOptions(testdir, compiler)
  testfile = findMain(testdir)

  if compiler == "ahc":
    all_opts = " ".join([compile_opts, "--input-hs", testfile])
  else:
    all_opts = " ".join([compile_opts, testfile])

  full_command = compiler_exe + " " + all_opts
  print ("Compile test: {0}".format(full_command))

  # Switch to the test directory
  current_directory = os.getcwd()
  os.chdir(testdir)
  # Compile the file
  subprocess.call(full_command.split())
  # Return to the previous directory
  os.chdir(current_directory)

# function runTest {
#   local executable=""
#   if [ ${COMP} == "ghc" ]; then
#     executable="${PWD}/Main"
#   elif [ ${COMP} == "ahc" ]; then
#     executable="${NODEJS} ${PWD}/Main.mjs"
#   else
#     exit 3
#   fi
#
#   local input_file_name=$1.${mode}stdin
#   if [ -f "${input_file_name}" ]; then
#     echo "EXECUTING: ${executable} <${input_file_name} 1>${testname}.${COMP}.stdout 2>${testname}.${COMP}.stderr"
#     ${executable} <${input_file_name} 1>${testname}.${COMP}.stdout 2>${testname}.${COMP}.stderr
#   else
#     echo "EXECUTING: ${executable} 1>${testname}.${COMP}.stdout 2>${testname}.${COMP}.stderr"
#     ${executable} 1>${testname}.${COMP}.stdout 2>${testname}.${COMP}.stderr
#   fi
# }

def runTestFile(testdir, testname, compiler, mode):
  assert (compiler in valid_compilers)
  assert (mode in valid_modes)

  main_exe = findExecutable(testdir, compiler)

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

  full_command = main_exe + " " + readTestRuntimeOptions(testdir, mode, compiler)
  print ("Run test: {0}".format(full_command))
  print ("Run test: {0}".format(full_command.split()))

  # Switch to the test directory
  current_directory = os.getcwd()
  os.chdir(testdir)
  # Run the test
  proc = subprocess.Popen(
    full_command.split(),
    # [main_exe], # TODO: Compute and use the runtime options!!
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
    print("Non-zero exit code for {0}!".format(testdir))
    print(readEntireFile(stderrfilepath))
    exit (1)

  # print(readEntireFile(stderrfilepath))

def main():
  for category in categories:
    category_path = os.path.join(working_directory, category)
    for testname in os.listdir(category_path):
      testdir = os.path.join(category_path, testname)
      if os.path.isdir(testdir):
        print ("testname: {0}".format(testname))
        print ("mainfile: {0}".format(findMain(testdir))) # mainfile
        print ("ghc-options: {0}".format(readTestCompileOptionsGHC(testdir))) # ghc_compile_opts
        print ("ahc-options: {0}".format(readTestCompileOptionsAHC(testdir))) # ghc_compile_opts
        print ("ghc-rts-options: {0}".format(readTestRuntimeOptionsGHC(testdir, "fast"))) # ghc_rts_opts
        print ("ahc-rts-options: {0}".format(readTestRuntimeOptionsAHC(testdir, "fast"))) # ghc_rts_opts

        compileTestFile(testdir, "ghc")
        runTestFile(testdir, testname, "ghc", "fast")

        # print (lvl2)
        # for item3 in os.listdir(lvl2):
        #   lvl3 =  os.path.join(lvl2, item3)
        #   if if shoulddelete(item3):
        #     os.remove(lvl3)


# CLEANUP

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

def distclean():
  for category in categories:
    category_path = os.path.join(working_directory, category)
    for testname in os.listdir(category_path):
      testdir = os.path.join(category_path, testname)
      if os.path.isdir(testdir):
        for item in os.listdir(testdir):
          if shoulddelete(item):
            os.remove(os.path.join(testdir,item))

if __name__ == "__main__":
  # distclean()
  main()



