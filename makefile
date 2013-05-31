INCLUDE_DIR=include
ERLC=erlc
ERLC_FLAGS=

INCLUDES = $(wildcard *.hrl)
SOURCES = $(wildcard *.erl)

BEAMS = $(SOURCES:%.erl=../ebin/%.beam)
APP = $(wildcard *.app)
TARGET_APP = $(APP:%=../ebin/%)

all: prepare $(BEAMS) $(TARGET_APP)

../ebin/%.beam: %.erl $(INCLUDES)
    $(ERLC) -I ${INCLUDE_DIR} -W $(ERLC_FLAGS) -o ../ebin $<

../ebin/%.app: %.app.src
    cp $< ../ebin

clean:
    rm -f ../ebin/*.beam ../ebin/*.app
