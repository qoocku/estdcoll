all: compile test

compile:
	./rebar compile
	
test:
	./rebar eunit
	