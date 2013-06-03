ERLC=erlc
ERLC_FLAGS=

INCLUDES = $(wildcard ./include/*.hrl)
SOURCES = $(notdir $(wildcard ./src/*.erl))

BEAMS = $(SOURCES:%.erl=../ebin/%.beam)
APP = $(wildcard ./src/*.app.src)
TARGET_APP = $(APP:%=../ebin/%)

all: prepare $(BEAMS) $(TARGET_APP)

prepare: 
	mkdir -p ebin

../ebin/%.beam: src/%.erl $(INCLUDES)
	$(ERLC) -I include -W $(ERLC_FLAGS) -o ../ebin $<

../ebin/%.app: %.app.src
	cp $< ../ebin

clean:
	rm -f ../ebin/*.beam ../ebin/*.app
