#!/bin/sh
#-----------------------------------------------------------------------------
# testsum.sh: Formats the runtests.report file
#-----------------------------------------------------------------------------
#
#   Copyright (C) 1992-1997,1998
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
PACK=$2
VERSION=`avers | awk '{printf("%.6s",$1)}'`
TPASS=`grep "^$2.*PASS" $1/bintest/runtests.report | wc -l`
TFAIL=`grep "^$2.*FAIL" $1/bintest/runtests.report | wc -l`
echo 
echo "Summary of $AIPSPATH runtests $VERSION"
echo
echo "****************************************************************************************"
echo 
echo $PACK | awk '{printf "Test results for %s package\n", $1}'
echo $TPASS $TFAIL | awk '{printf "\t%5.1f%% Passed %d of %d\n", 100*$1/($1+$2), $1, $1+$2}'
echo "****************************************************************************************"
echo 
echo "Tests that failed to compile"
echo 
grep "^$2.*FAIL.*compile" $1/bintest/runtests.report
echo
echo "Tests that failed to execute"
echo 
grep "^$2.*FAIL.*execute" $1/bintest/runtests.report
echo
echo "Tests that failed to verify"
echo 
grep "^$2.*FAIL.*verify" $1/bintest/runtests.report
echo 
grep "^$2.*FAIL.*execute" $1/bintest/runtests.report > /tmp/aips2tests.noexecute
if [ -s /tmp/aips2tests.noexecute ]
then
   echo 
   echo "*****************************************************************"
   echo "Details tests that had execution failures"
   echo "*****************************************************************"
   echo 
   
   NOVERIFY=`awk '{print $2}' /tmp/aips2tests.noexecute`
   for testrpt in $NOVERIFY
   do
      echo
      echo "*****************************************************************"
      echo "$testrpt -- report"
      echo "*****************************************************************"
      echo
      $AWK -v testrpt=$testrpt '{printf "%s-execute> %s\n", testrpt, $0}' $1/bintest/$testrpt.report
   done
fi
rm /tmp/aips2tests.noexecute
echo 
grep "^$2.*FAIL.*verify" $1/bintest/runtests.report > /tmp/aips2tests.noverify
if [ -s /tmp/aips2tests.noverify ]
then
   echo 
   echo "*****************************************************************"
   echo "Details tests that had verify failures"
   echo "*****************************************************************"
   echo 
   
   NOVERIFY=`awk '{print $2}' /tmp/aips2tests.noverify`
   for testrpt in $NOVERIFY
   do
      echo
      echo "*****************************************************************"
      echo "$testrpt -- report"
      echo "*****************************************************************"
      echo
      $AWK -v testrpt=$testrpt '{printf "%s-verify> %s\n", testrpt, $0}' $1/bintest/$testrpt.report
   done
fi
rm /tmp/aips2tests.noverify
echo
echo "Tests that passed"
echo
grep "^$2.*PASS" $1/bintest/runtests.report
echo
echo "*****************************************************************"
echo "End tests report for package $PACK"
echo "*****************************************************************"
