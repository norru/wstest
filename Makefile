.PHONY: rel deps

all: deps
	@./rebar compile

deps:
	@./rebar get-deps

rel: deps
	@./rebar compile generate

relforce: deps
	@./rebar compile generate force=1

clean:
	@./rebar clean

distclean: clean relclean
	@./rebar delete-deps

relclean:
	rm -rf rel/wstest

stage : rel
	cd rel/wstest/lib && \
	rm -rf wstest-* && \
	ln -s ../../../apps/wstest

test:
	./rebar skip_deps=true eunit
