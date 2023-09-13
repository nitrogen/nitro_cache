## rebar3.mk
## Version 0.1.1
## Copyright 2023 Jesse Gumm
## MIT License
##
## This is a tool to help you either use the existing rebar3 in the system or
## build a new one just for a specific project.
## 
## How to use:
##  1) include it in your Makefile with:
##
##         include rebar3.mk
##
##  2) Update any rules in your makefile so that rebar3 is required, and then
##     change the rebar3 calls to use "$(REBAR)" instead of "rebar3" or "./rebar3"
##
##     For example, change this:
##
##         compile:
##             rebar3 compile
##     
##     To This:
##
##         compile: rebar3
##             $(REBAR) compile


## TODO Notes:
## I'd like to update this to take a $MINIMUM_REBAR_VERSION variable
## to check the current version of rebar3, and if the version is less than
## the $MINIMUM_REBAR_VERSION, then download/compile the latest version

## Similarly, I'd like to add a $(FORCE_REBAR_VERSION) variable which *forces*
## the project to use that specific verion. This would work similarly by
## comparing versions, and if the version is different, download/compile that
## specific version locally.

## Finally, the logic here should be changed slightly to check for a local (in
## this project) version of rebar3 and use that first. If that doesn't exist,
## then fall back to checking the path, and finally, if that still doesn't
## exist, then (and only then) downloading and compiling rebar3.

## A more advanced version of this checker would also verify that rebar3
## actually runs and doesn't just throw an error.

REBAR_MK_VERSION=0.2.0
REBAR_LATEST_VERSION=$(shell curl -s https://api.github.com/repos/erlang/rebar3/releases/latest | jq -r ".tag_name")
REBAR_VERSION?=$(REBAR_LATEST_VERSION)
#REBAR_VERSION=

REBAR_PATH = $(shell which rebar3)

ifeq ($(REBAR_PATH),)
REBAR = ./rebar3
RANDOM_STRING := rebar3_$(shell openssl rand -hex 16)
rebar3:
	@make update_rebar3
else
REBAR = rebar3
rebar3:
endif

rebar3_mk:
	@curl -O https://raw.githubusercontent.com/choptastic/rebar3.mk/master/rebar3.mk

update_rebar3:
	@echo "Fetching and compiling rebar3 ($(REBAR_VERSION)) for this local project..."
	@(cd /tmp && \
	git clone https://github.com/erlang/rebar3 $(RANDOM_STRING) -q && \
	cd $(RANDOM_STRING) && \
	git fetch --tags -q && \
	git checkout $(REBAR_VERSION) -q && \
	./bootstrap)
	@echo "Installing rebar3 into your project's directory..."
	@(mv /tmp/$(RANDOM_STRING)/rebar3 .)
	@echo "Cleaning up..."
	@(rm -fr /tmp/$(RANDOM_STRING))

install_rebar3: rebar3
	@(./rebar3 local install)
