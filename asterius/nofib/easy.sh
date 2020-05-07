#!/bin/sh

echo '\033]0;NOFIB: starting...\007'

# Settings
#######################################################################

mode=norm

# "Library" part
#######################################################################

show_usage () {
cat <<EOF
./easy.sh - easy nofib

Usage: ./easy.sh [ -m mode ] /path/to/baseline/ghc /path/to/new/ghc"

GHC paths can point to the root of the GHC repository,
if it's build with Hadrian.

Available options:
  -m MODE  nofib mode: fast norm slow

This script caches the results using the sha256 of ghc executable.
Remove these files, if you want to rerun the benchmark.
EOF
}

hashoffile () {
	shasum -a 256 $1 | awk '{ print $1 }'
}

# getopt
#######################################################################

while getopts 'm:' flag; do
    case $flag in
        m)
			case $OPTARG in
				slow)
					mode=$OPTARG
					;;
				norm)
					mode=$OPTARG
					;;
				fast)
					mode=$OPTARG
					;;
				*)
					echo "Unknown mode: $OPTARG"
					show_usage
					exit 1
					;;
			esac
			;;

        ?) show_usage
            ;;
	esac
done

shift $((OPTIND - 1))

if [ $# -ne 2 ]; then
	echo "Expected two arguments: ghc executables or roots of source repositories"
	show_usage
	exit 1
fi

OLD_HC=$1
NEW_HC=$2

# Set up
#######################################################################

# Arguments can point to GHC repository roots
if [ -d $OLD_HC -a -f "$OLD_HC/_build/stage1/bin/ghc" ]; then
	OLD_HC="$OLD_HC/_build/stage1/bin/ghc"
fi

if [ -d $NEW_HC -a -f "$NEW_HC/_build/stage1/bin/ghc" ]; then
	NEW_HC="$NEW_HC/_build/stage1/bin/ghc"
fi

# Check we have executables
if [ ! -f $NEW_HC -a -x $OLD_HC ]; then
    echo "$OLD_HC is not an executable"
    exit 1
fi

if [ ! -f $NEW_HC -a -x $NEW_HC ]; then
    echo "$NEW_HC is not an executable"
    exit 1
fi

# Info before we get going
#######################################################################

echo "Running nofib (mode=$mode) with $OLD_HC and $NEW_HC"
echo "Running nofib (mode=$mode) with $OLD_HC and $NEW_HC" | sed 's/./-/g'
sleep 2

# Run nofib
#######################################################################

# Run with old ghc
echo '\033]0;NOFIB: old\007'

OLD_HASH=$(hashoffile $OLD_HC)
OLD_OUTPUT=result-$OLD_HASH-$mode.txt

if [ -f $OLD_OUTPUT ]; then
	echo "$OLD_OUTPUT exists; not re-running."
else
	echo '\033]0;NOFIB: old, cleaning...\007'
	make clean

	echo '\033]0;NOFIB: old, booting...\007'
	make boot mode=$mode HC=$OLD_HC

	echo '\033]0;NOFIB: old, benchmarking...\007'
	make mode=$mode HC=$OLD_HC 2>&1 | tee $OLD_OUTPUT
fi
 
# Run with new ghc
echo '\033]0;NOFIB: new\007'

NEW_HASH=$(hashoffile $NEW_HC)
NEW_OUTPUT=result-$NEW_HASH-$mode.txt

if [ -f $NEW_OUTPUT ]; then
	echo "$NEW_OUTPUT exists; not re-running."
else
	echo '\033]0;NOFIB: new, cleaning...\007'
	make clean

	echo '\033]0;NOFIB: new, booting...\007'
	make boot mode=$mode HC=$NEW_HC

	echo '\033]0;NOFIB: new, benchmarking...\007'
	make mode=$mode HC=$NEW_HC 2>&1 | tee $NEW_OUTPUT
fi

# Done
#######################################################################

echo '\033]0;NOFIB: done\007'

# Analyse
./nofib-analyse/nofib-analyse $OLD_OUTPUT $NEW_OUTPUT > report.txt

# Show report
less report.txt
