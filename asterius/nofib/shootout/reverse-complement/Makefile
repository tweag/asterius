TOP = ../..
include $(TOP)/mk/boilerplate.mk

# Override default SRCS; the default is all source files, but
# we don't want to include revcomp-c.c
SRCS = Main.hs

CLEAN_FILES += fasta-c revcomp-c reverse-complement *stdout *stdin

# These values are only used in this file. They are ignored by the
# executable itself.
FAST_OPTS =  5000000
NORM_OPTS = 50000000
# Normally, we'd want to have SLOW_OPTS = $((5*NORM_OPTS)) to run this for 5s
# in 2018. But that means
#   - slow generation of huge input and output files
#   - having to diff the generated output files
#   - the input parser in fasta-c.c overflows, so we're generating an incorrect
#     reference output
# So we don't do it.
SLOW_OPTS = 50000000
# This is the official shootout setting ($((NORM_OPTS/2)), i.e. running 0.5s in
# 2018):
# SLOW_OPTS = 25000000

# The benchmark game also uses -fllvm, which we can't since it might
# not be available on the developer's machine.
HC_OPTS += -funfolding-use-threshold=32

#------------------------------------------------------------------
# Create input

fasta-c : ../fasta/fasta-c.c
	$(CC) -std=gnu99 -O3 -fomit-frame-pointer $< -o $@

reverse-complement.faststdin : fasta-c
	./fasta-c $(FAST_OPTS) | tr -d '\r' > $@

reverse-complement.stdin : fasta-c
	./fasta-c $(NORM_OPTS) | tr -d '\r' > $@

reverse-complement.slowstdin : fasta-c
	./fasta-c $(SLOW_OPTS) | tr -d '\r' > $@

# Since we only decide here what the INPUT_FILE is, it's required to first run
# `make boot` and only than `make` (otherwise `make` doesn't "see" the file and
# doesn't call `runstdtest` correctly)
ifeq "$(mode)" "slow"
 INPUT_FILE = reverse-complement.slowstdin
else
 ifeq "$(mode)" "fast"
  INPUT_FILE = reverse-complement.faststdin
 else
  INPUT_FILE = reverse-complement.stdin
 endif
endif

#------------------------------------------------------------------
# Create output to validate against

revcomp-c : revcomp-c.o
	$(CC) $< -o $@ -pthread

reverse-complement.faststdout : revcomp-c $(INPUT_FILE)
	./revcomp-c < $(INPUT_FILE) | tr -d '\r' > $@

reverse-complement.stdout : revcomp-c $(INPUT_FILE)
	./revcomp-c < $(INPUT_FILE) | tr -d '\r' > $@

reverse-complement.slowstdout : revcomp-c $(INPUT_FILE)
	./revcomp-c < $(INPUT_FILE) | tr -d '\r' > $@

# Since we only decide here what the OUTPUT_FILE is, it's required to first run
# `make boot` and only than `make` (otherwise `make` doesn't "see" the file and
# doesn't call `runstdtest` correctly)
ifeq "$(mode)" "slow"
 OUTPUT_FILE = reverse-complement.slowstdout
else
 ifeq "$(mode)" "fast"
  OUTPUT_FILE = reverse-complement.faststdout
 else
  OUTPUT_FILE = reverse-complement.stdout
 endif
endif

boot :: $(INPUT_FILE) $(OUTPUT_FILE)

include $(TOP)/mk/target.mk
