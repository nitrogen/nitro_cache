.PHONY: test

CWD=$(shell pwd)
NAME=$(shell basename ${CWD})
CT_LOG=${APP_DIR}/logs
REBAR?=${CWD}/rebar
COOKIE?=cookie_${NAME}
ERL?=/usr/bin/env erl
ERLARGS=-pa ebin -smp enable -name ${NODE} \
	-setcookie ${COOKIE} -boot start_sasl

all: clean getdeps compile

# Clean all.
clean:
	@${REBAR} clean

# Gets dependencies.
getdeps:
	@${REBAR} get-deps

# Compiles.
compile:
	@${REBAR} compile

test:
	./rebar -v --config "rebar.test.config" skip_deps=true ct

benchmark:
	${ERL} ${ERLARGS} +P 60000000 -eval "application:start(nitro_cache)" -eval "nitro_cache:benchmark(1000)"

# This one runs without a release.
shell: compile
	${ERL} ${ERLARGS}

