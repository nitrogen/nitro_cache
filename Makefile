.PHONY: test

CWD=$(shell pwd)
NAME=$(shell basename ${CWD})
CT_LOG=${APP_DIR}/logs
REBAR?=${CWD}/rebar3
COOKIE?=cookie_${NAME}
ERL?=/usr/bin/env erl
ERLARGS=-pa ebin -smp enable -name ${NODE} \
	-setcookie ${COOKIE} -boot start_sasl

all: clean get-deps compile

# Clean all.
clean:
	@${REBAR} clean

# Gets dependencies.
get-deps:
	@${REBAR} get-deps

# Compiles.
compile:
	@${REBAR} compile

run:
	$(REBAR) shell --apps nitro_cache

test: compile
	$(REBAR) ct

benchmark:
	${ERL} ${ERLARGS} +P 60000000 -eval "application:start(nitro_cache)" -eval "nitro_cache:benchmark(1000)"

# This one runs without a release.
shell: compile
	${ERL} ${ERLARGS}

dialyzer: compile
	$(REBAR) dialyzer

travis: test dialyzer

publish:
	$(REBAR) upgrade
	$(REBAR) hex publish
	$(REBAR) upgrade

