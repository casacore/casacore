#!/bin/sh
#-----------------------------------------------------------------------------
# assay: Invoke an AIPS++ test program and verify its output
#-----------------------------------------------------------------------------
#
#   Copyright (C) 1995,1996,1998,1999,2001,2003
#   Associated Universities, Inc. Washington DC, USA.
#
#   This program is free software; you can redistribute it and/or modify
#   it under the terms of the GNU General Public License as published by
#   the Free Software Foundation; either version 2 of the License, or
#   (at your option) any later version.
#
#   This program is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#   GNU General Public License for more details.
#
#   You should have received a copy of the GNU General Public License
#   along with this program; if not, write to the Free Software
#   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
#
#   Correspondence concerning AIPS++ should be addressed as follows:
#          Internet email: aips2-request@nrao.edu.
#          Postal address: AIPS++ Project Office
#                          National Radio Astronomy Observatory
#                          520 Edgemont Road
#                          Charlottesville, VA 22903-2475 USA
#
#-----------------------------------------------------------------------------
# Usage: assay <testexe and arguments>
#-----------------------------------------------------------------------------
# assay invokes a casacore test program. If the test program has an associated
# .run file then it simply invokes it.  Otherwise assay invokes the test
# executable directly, and, if there is a corresponding .out file, compares
# its output with that.
#
# The env.var. CASACORE_CHECK can be defined to do checking using tools
# like valgrind. It defines a script that will execute the command. The
# script can invoke the checking tool. If set to 1, yes, or YES, the script
# casacore_memcheck will be used that uses valgrind's memcheck tool.
#
# If there is a .in file associated with the test program then assay will
# redirect stdin from it.
#
# Options:
#   none
#
# Status returns:
#    0:  success
#    1:  test execution failed
#    2:  test output disagreement
#    3:  untested (usually returned from a .run)
#  130:  interrupt
#
# Original: 1995/11/01 by Mark Calabretta, ATNF
# $Id: assay,v 19.11 2006/10/17 03:41:50 gvandiep Exp $
#=============================================================================

# Find the path used to start the script. It is used for other scripts.
  pgmpath=`dirname $0`
  pgmpath=`cd $pgmpath > /dev/null 2>&1  &&  pwd`

# Also set PYTHONPATH.
  if [ "$PYTHONPATH" = "" ]
  then
     PYTHONPATH=.
  else
     PYTHONPATH=".:$PYTHONPATH"
  fi
  CLEANUP=

# Set testsrcdir to . if undefined.
  if [ "$testsrcdir" = "" ]
  then
     testsrcdir=.
  fi

# Determine if a program checktool (like valgrind) has to be used.
  casa_checktool=$CASACORE_CHECK
# Empty or no means no checktool.
  if test "$casa_checktool" = "0" -o "$casa_checktool" = "no" -o "$casa_checktool" = "NO"; then
     casa_checktool=
  fi
# Default checktool is casacore_memcheck (using valgrind).
  if test "$casa_checktool" = "1" -o "$casa_checktool" = "yes" -o "$casa_checktool" = "YES"; then
     casa_checktool=$pgmpath/casacore_memcheck
  fi
# Export, so it can be used in possible .run files.
  export casa_checktool
# Delete possible already existing checktool output.
  rm -rf $1.checktool*

# Define exit and interrupt handler.
  trap 'rm -rf $CLEANUP core ${1}_tmp* ; \
        trap - 0 ; \
        exit $STATUS' 0 1 2 3 15

# Get the command.
  COMMAND="$casa_checktool $@"
  set $@

# Get the current directory.
  curdir=`pwd`

# If there is a .run file then use it without checktool.
# Start it using sh if not executable.
  if [ -f "$1.run" ]
  then
     COMMAND="sh $1.run"
     if [ -x "$1.run" ]
     then
        COMMAND="$1.run"
     fi
  fi

# If there is a .in file then use it as input.
  if [ -f "$1.in" ]
  then
     COMMAND="$COMMAND < $1.in"
  fi

  # Remove possible old output files.
  rm -rf ${1}_tmp*

  # If running on a Cray cluster, we have to use qsub and yod.
  # If found, the result is a path, thus starts with a slash.
  QSUBP=`which qsub 2>&1 | sed -e 's%^[^/].*%%'`
  YODP=`which yod 2>&1 | sed -e 's%^[^/].*%%'`
  if [ "$QSUBP" = ""  -o  "$YODP" = "" ]
  then
    # No Cray.
    eval "$COMMAND" > ${1}_tmp.out
    STATUS=$?
    if [ $STATUS != 0 ]
    then
      if [ $STATUS = 3 ]
      then
        echo "UNTESTED: $*"
      else
        echo "FAIL (execution failure): $*"
      fi
      exit
    fi
  else
    # .run files are not supported yet on the Cray.
    if [ -f "$1.run" ]
    then
      echo "UNTESTED (no .run support): $*"
      exit 3
    fi
    # Create a batch job which will use a single processor.
    # Copy the AIPSPATH definition.
    # Because the job does not wait, we do that by using a wait file.
    # Note that yod does wait.
    rm -f $1_tmp.wait
    cat > $1_tmp.qsub <<EOF
#PBS -l size=1
#PBS -N $1_tmp_job
#PBS -j oe
#PBS -v AIPSPATH
cd \$PBS_O_WORKDIR
yod -sz 1 ./$1 > $1_tmp.out
touch $1_tmp.wait
EOF
    # Submit the job and wait for it to be finished.
    # Do not wait longer than 10 minutes.
    qsub $1_tmp.qsub
    NRTIMES=0
    while [ ! -f $1_tmp.wait ]
    do
      if test $NRTIMES -gt 600
      then
        # Keep job file.
        mv $1_tmp_job  $1_job
        echo "FAIL (execution killed): $*"
        exit 1
      fi
      sleep 1
      NRTIMES=`expr $NRTIMES + 1`
    done
  fi

  if [ -f $1.out ]
  then
     CAT="$1.out"
  else
     echo "PASS (execution succeeded): $*"
     STATUS=0
     exit
  fi

# Strip out demarked text.
#  First text on a single line enclosed in >>> and <<<.
#  Then lines enclosed in lines starting with >>> and <<<.
# Remove the current directory from the output (to avoid system dependencies).
  sed -e 's/>>>.*<<<//' -e '/^>>>/,/^<<</d' ${1}_tmp.out | sed -e "s%$curdir/%%g" > ${1}_tmp.out2
  mv -f ${1}_tmp.out2 ${1}_tmp.out

# Compare with the expected output.
  cat "$CAT" | sed -e 's/>>>.*<<<//' -e '/^>>>/,/^<<</d' > ${1}_tmpo.out
  if diff ${1}_tmp.out ${1}_tmpo.out
  then
     echo "PASS (output verified): $*"
     STATUS=0
     exit
  fi
  # Not fully equal. Check with floating point accuracy.
  $pgmpath/casacore_floatcheck ${1}_tmp.out ${1}_tmpo.out 1e-5
  stat=$?
  case $stat in
  0)
     echo "PASS (floating point discrepancies <= 1e-5): $1"
     ;;
  1)
     echo "FAIL (output not verified): $*"
     ;;
  *)
     echo "FAIL (floating point discrepancies > 1e-5): $1"
     ;;
  esac
  STATUS=$stat
  exit
