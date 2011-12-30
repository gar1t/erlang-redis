compile: deps
	./rebar compile

quick:
	./rebar compile skip_deps=true

deps:
	./rebar get-deps

refresh-deps:
	./rebar delete-deps
	./rebar get-deps

clean:
	./rebar clean

.PHONY: test

DEPS=$(notdir $(wildcard deps/*))
SKIP_APP=$(shell echo $(DEPS) | tr ' ' ',')

TESTS=""
test:
ifeq ($(TESTS), "")
	./rebar -j1 eunit skip_deps=true skip_app=$(SKIP_APP)
else
	./rebar -j1 eunit suite=$(TESTS) skip_deps=true skip_app=$(SKIP_APP)
endif

shell: compile
	erl -pa ebin ../../ebin \
            -pa ebin $(wildcard deps/*/ebin) \
            -config priv/listfever.config \
            -s lf_reloader \
            -s listfever
