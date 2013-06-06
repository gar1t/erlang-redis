compile:
	./rebar compile

clean:
	./rebar clean

shell: compile
	erl -pa ebin ../../ebin ebin -s redis_reloader -s dbservice
