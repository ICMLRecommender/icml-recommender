CC = g++ -Wall
#CC = g++ -ansi -Wall -pedantic
#CFLAGS = -g -Wall -O3 -ffast-math -DHAVE_INLINE -DGSL_RANGE_CHECK_OFF
# CFLAGS = -g -Wall
LDFLAGS = -lgsl -lm -lgslcblas

GSL_INCLUDE_MAC = /usr/local/include/
GSL_LIB_MAC = /usr/local/lib/

LSOURCE = main.cpp utils.cpp corpus.cpp ctr.cpp data.cpp opt.cpp
LHEADER = utils.h corpus.h ctr.h data.h opt.h

UNAME_S := $(shell uname -s)

ifeq ($(UNAME_S),Linux)
endif
ifeq ($(UNAME_S),Darwin)
    CCFLAGS += -I$(GSL_INCLUDE_MAC) -L$(GSL_LIB_MAC)
endif

all: $(LSOURCE) $(LHEADER)
	  $(CC) -O3 -DNDEBUG -s $(CCFLAGS) $(LSOURCE) -o ctr $(LDFLAGS)

debug: $(LSOURCE) $(LHEADER)
	  $(CC) -g  $(CCFLAGS) $(LSOURCE) -o ctr $(LDFLAGS)

clean:
	-rm -f ctr
