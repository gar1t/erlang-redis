TESTS=""
TESTDB=""

compile:
	./rebar compile

.PHONY: test
test: compile
ifeq ($(TESTDB), "")
	@echo "Usage: make TESTDB=[0-15] test"
	@exit 1
endif
ifeq ($(TESTS), "")
	./rebar -j1 eunit
else
	./rebar -j1 eunit suite=$(TESTS)
endif

.PHONY: doc
doc: compile
	./rebar doc

clean:
	./rebar clean

shell: compile
	 erl -pa ebin .eunit -s redis_reloader -s redis
