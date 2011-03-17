all: ebin/estdcoll.app compile test

compile:
	./rebar compile
	
test:
	./rebar eunit
	
ebin/stdcoll.app : src/vsn
		