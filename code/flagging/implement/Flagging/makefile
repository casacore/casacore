# $Id$
XLIBLIST := WCSLIB
# Use the generic AIPS++ class implementation makefile.
#------------------------------------------------------
include $(word 1, $(AIPSPATH))/code/install/makefile.imp

TEST: all
	(cd test; make EXTRA_C++FLAGS=-O3 OPTLIB=1)
  
APPS: all
	(cd ../../apps/redflagger; make EXTRA_C++FLAGS=-O3 OPTLIB=1)
