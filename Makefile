TOP=..
include $(TOP)/mk/boilerplate.mk

# ---------------------------------------------------------------

ALL_DIRS =	       \
    Control/Arrow

PACKAGE		:= streamproc
RELEASEDAY	:= 2005-02-14
VERSION		:= 0.0-$(RELEASEDAY)
PACKAGE_DEPS	:= base

SRC_HADDOCK_OPTS += -t "Stream Processer Arrow ($(PACKAGE) package)"

# ---------------------------------------------------------------

-include $(TOP)/mk/crypto.mk
include $(TOP)/mk/target.mk
