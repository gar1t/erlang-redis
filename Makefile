TESTS=""

compile:
	./rebar compile

.PHONY: test
test: compile
ifeq ($(TESTS), "")
	./rebar -j1 eunit
else
	./rebar -j1 eunit suite=$(TESTS)
endif

.PHONY: doc
doc:
	./rebar doc

clean:
	./rebar clean

shell: compile
	 erl -pa ebin -s redis_reloader
