#!/bin/sh
AWK=gawk
PACK=$2
TPASS=`grep "^$2.*PASS" $1/bintest/runtests.report | wc -l`
TFAIL=`grep "^$2.*FAIL" $1/bintest/runtests.report | wc -l`
echo
echo "*****************************************************************"
echo $PACK | awk '{printf "Test results for %s package\n", $1}'
echo $TPASS $TFAIL | awk '{printf "\t%5.1f%% Passed %d of %d\n", 100*$1/($1+$2), $1, $1+$2}'
echo "*****************************************************************"
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
