# $Id$

# Use the generic AIPS++ class implementation makefile.
#------------------------------------------------------
XLIBLIST := WCSLIB
include $(word 1, $(AIPSPATH))/code/install/makefile.imp

# Set up a parallel make if PARALLEL_MAKE is defined in makedefs
ifdef PARALLEL_MAKE
  MAKE := $(PARALLEL_MAKE)
endif
