#!/bin/sh
#-----------------------------------------------------------------------------
# testsum.sh: Formats the runtests.report file
#-----------------------------------------------------------------------------
#
#   Copyright (C) 1992-1997,1998,1999,2001,2002
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
# Usage: testsum.sh $AIPSARCH package
#-----------------------------------------------------------------------------
# testsum.sh -- Makes a nicely formated listing out of the runtests.report
#               file found in $AIPSARCH/bintest
#
# Options: None
#
# Notes:
#    Needs gawk or nawk to work
#=============================================================================
AWK=awk
if [ -x /usr/bin/nawk ] ; then
   AWK=nawk
 elif [ -x /usr/local/bin/gawk ] ; then
   AWK=gawk
 elif [ -x /opt/local/gnu/bin/gawk ] ; then
   AWK=gawk
 elif [ -x /usr/bin/gawk ] ; then
   AWK=gawk
fi
PACK=$3
PKG=$3
if [ "$PACK" = "" ]; then
  PACK=ALL
  PKG='.*'
fi
VERSION=`avers | awk '{printf("%.6s",$1)}'`
TPASS=`grep "^${PKG}-.*PASS" $2 | wc -l`
TFAIL=`grep "^${PKG}-.*FAIL" $2 | wc -l`
TWARNING=`grep "^${PKG}-.*WARNING" $2 | wc -l`
TUNKNOWN=`grep "^${PKG}-.*UNKNOWN" $2 | wc -l`
TUNTESTED=`grep "^${PKG}-.*UNTESTED" $2 | wc -l`
echo 
echo "Summary of $AIPSPATH runtests $VERSION"
echo
echo "******************************************************************************"
echo $PACK | awk '{printf "Test results for package %s\n", $1}'
echo $TPASS $TFAIL $TWARNING $TUNKNOWN| awk '{nr=$1+$2+$3+$4; perc=0; if (nr!=0) perc=100*$1/nr; printf "\t%5.1f%% passed (%d of %d)\n", perc, $1, nr}'
echo "          $TFAIL failed"
echo "          $TWARNING with verification warnings (floating point discrepancies)"
echo "          $TUNKNOWN unknown"
echo "          $TUNTESTED skipped"
echo "+*****************************************************************************"

NRPGM=`grep "^${PKG}-.*FAIL.*compile" $2 | wc -l`
if [ $NRPGM != 0 ]; then
  echo 
  echo "  Tests that failed to compile"
  grep "^${PKG}-.*FAIL.*compile" $2
fi
NRPGM=`grep "^${PKG}-.*FAIL.*execute" $2 | wc -l`
if [ $NRPGM != 0 ]; then
  echo
  echo "  Tests that failed to execute"
  grep "^${PKG}-.*FAIL.*execute" $2
fi
NRPGM=`grep "^${PKG}-.*FAIL.*verify" $2 | wc -l`
if [ $NRPGM != 0 ]; then
  echo
  echo "  Tests that failed to verify"
  grep "^${PKG}-.*FAIL.*verify" $2
fi
NRPGM=`grep "^${PKG}-.*UNKNOWN" $2 | wc -l`
if [ $NRPGM != 0 ]; then
  echo 
  echo "  Tests that were unknown"
  grep "^${PKG}-.*UNKNOWN" $2
fi
NRPGM=`grep "^${PKG}-.*UNTESTED" $2 | wc -l`
if [ $NRPGM != 0 ]; then
  echo 
  echo "  Tests that were skipped"
  grep "^${PKG}-.*UNTESTED" $2
fi

grep "^${PKG}-.*FAIL.*execute" $2 > /tmp/aips2tests.noexecute
if [ -s /tmp/aips2tests.noexecute ]
then
   echo 
   echo "*****************************************************************"
   echo "Details tests that had execution failures"
   echo "*****************************************************************"
   
   NOVERIFY=`awk '{print $2}' /tmp/aips2tests.noexecute`
   for testrpt in $NOVERIFY
   do
      echo
      echo "*****************************************************************"
      echo "$testrpt -- report"
      echo "*****************************************************************"
      $AWK -v testrpt=$testrpt '{printf "%s-execute> %s\n", testrpt, $0}' $1/bintest/$testrpt.report
   done
fi
rm /tmp/aips2tests.noexecute
echo 
grep "^${PKG}-.*FAIL.*verify" $2 > /tmp/aips2tests.noverify
if [ -s /tmp/aips2tests.noverify ]
then
   echo 
   echo "*****************************************************************"
   echo "Details tests that had verify failures"
   echo "*****************************************************************"
   
   NOVERIFY=`awk '{print $2}' /tmp/aips2tests.noverify`
   for testrpt in $NOVERIFY
   do
      echo
      echo "*****************************************************************"
      echo "$testrpt -- report"
      echo "*****************************************************************"
      $AWK -v testrpt=$testrpt '{printf "%s-verify> %s\n", testrpt, $0}' $1/bintest/$testrpt.report
   done
fi
rm /tmp/aips2tests.noverify

NRPGM=`grep "^${PKG}-.*WARNING" $2 | wc -l`
if [ $NRPGM != 0 ]; then
  echo
  echo "  Tests that had warnings"
  grep "^${PKG}-.*WARNING" $2
fi
echo
echo "  Tests that passed"
grep "^${PKG}-.*PASS" $2
echo
echo "*****************************************************************"
echo "End tests report for package $PACK"
echo "*****************************************************************"

