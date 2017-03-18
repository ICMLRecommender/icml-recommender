CC = g++
CFLAGS_RELEASE = -Wall -DNDEBUG -s -O3
#CFLAGS_RELEASE = -DNDEBUG -s -O3 -ffast-math -DHAVE_INLINE -DGSL_RANGE_CHECK_OFF
CFLAGS_DEBUG = -g -Wall -pedantic -ansi
LDFLAGS = -lgsl -lm -lgslcblas

GSL_INCLUDE_MAC = /usr/local/include/
GSL_LIB_MAC = /usr/local/lib/

#GSL_INCLUDE_LINUX = ~/gsl-2.3/include/
#GSL_LIB_LINUX = ~/gsl-2.3/lib/

LSOURCE = main.cpp utils.cpp corpus.cpp ctr.cpp data.cpp opt.cpp
LHEADER = utils.h corpus.h ctr.h data.h opt.h

UNAME_S := $(shell uname -s)

#ifeq ($(UNAME_S),Linux)
#    CCFLAGS += -I$(GSL_INCLUDE_LINUX) -L$(GSL_LIB_LINUX)
#endif
ifeq ($(UNAME_S),Darwin)
    CCFLAGS += -I$(GSL_INCLUDE_MAC) -L$(GSL_LIB_MAC)
endif

release: $(LSOURCE) $(LHEADER)
	  $(CC) $(CFLAGS_RELEASE) $(CCFLAGS) $(LSOURCE) -o ctr $(LDFLAGS)

debug: $(LSOURCE) $(LHEADER)
	  $(CC) $(CFLAGS_DEBUG)  $(CCFLAGS) $(LSOURCE) -o ctr $(LDFLAGS)

clean:
	-rm -f ctr
