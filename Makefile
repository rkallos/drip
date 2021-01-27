all: compile

clean:
	@rebar clean -a

compile: fmt
	@rebar3 compile

dialyzer:
	@rebar3 dialyzer

eunit:
	@rebar3 as test do eunit -cv, cover -v

fmt:
	@rebar3 fmt --write

gradualizer:
	@rebar3 gradualizer

lint:
	@rebar3 lint

test: lint xref gradualizer dialyzer eunit dialyzer

xref:
	@rebar3 as test do xref

.PHONY: clean compile dialyzer eunit fmt gradualizer lint xref
