#! /bin/sh
#
# This script checks if two files containing floating point numbers
# are approximately equal.
# All non-numeric characters are ignored.
# The differences are written to stdout.
#
# run as:    floatcheck.sh file1 file2 [precision]
#                          default precision is 1e-5

  if test $# -lt 2; then
    echo "usage: floatcheck.sh file1 file2 [precision]"
    exit 1
  fi
  PREC=$3
  if test $# -lt 3; then
    PREC=1e-5
  fi
 
  pid=$$

# Check if all numbers are approximately equal.
# First replace all non-numeric characters and single e.+- by a blank.
# Insert a blank if a - or + is preceeded by a digit.
# Replace blanks by a newline.
  sed -e 's/[^-+.e0-9]/ /g' $1 | sed -e 's/\(^\| \)[e.+-]\+\( \|$\)//g' | sed -e 's/^ \+//' -e 's/ \+$//' -e 's/\([0-9.]\)\([+-]\)/\1 \2/g' -e 's/ \+/\n/g' > /tmp/$pid.floatcheck_tmp.1
  sed -e 's/[^-+.e0-9]/ /g' $2 | sed -e 's/\(^\| \)[e.+-]\+\( \|$\)//g' | sed -e 's/^ \+//' -e 's/ \+$//' -e 's/\([0-9.]\)\([+-]\)/\1 \2/g' -e 's/ \+/\n/g' > /tmp/$pid.floatcheck_tmp.2

# Show the differences column-wise.
  diff -y --suppress-common-lines /tmp/$pid.floatcheck_tmp.1 /tmp/$pid.floatcheck_tmp.2 > /tmp/$pid.floatcheck_tmp.diff

# Now loop through all differences and see if almost equal.
  awk '{ a1=$1; if (a1<0) a1=-a1; a2=$3; if (a2<0) a2=-a2; if (2*a1>'"$PREC"' && 2*a2>'"$PREC"') if ((a1>a2 && a1-a2>'"$PREC"'*a1) || (a2>a1 && a2-a1>'"$PREC"'*a2)) print "float diff>'"$PREC"':",$1,$3 }' /tmp/$pid.floatcheck_tmp.diff > /tmp/$pid.floatcheck_tmp.res
  STATUS=0;
  if [ -s /tmp/$pid.floatcheck_tmp.res ]
  then
    cat /tmp/$pid.floatcheck_tmp.res
    STATUS=2
  fi
  \rm -f /tmp/$pid.floatcheck_tmp.*
  exit $STATUS
