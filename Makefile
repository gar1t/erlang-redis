compile:
	./rebar compile

.PHONY: test

TESTS=""
test:
ifeq ($(TESTS), "")
	./rebar -j1 eunit
else
	./rebar -j1 eunit suite=$(TESTS)
endif

clean:
	./rebar clean
shell:
	 erl -pa ebin -s redis_reloader
