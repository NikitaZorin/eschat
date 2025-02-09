PROJECT = eschat
PROJECT_DESCRIPTION = New project
PROJECT_VERSION = 0.1.0

DEPS += cowboy
DEPS += jsone
DEPS += epgsql
DEPS += lager
DEPS += sherlock
DEPS += uuid
# DEPS += poolboy

dep_cowboy = git https://github.com/ninenines/cowboy 2.9.0
dep_jsone = git https://github.com/sile/jsone master
dep_epgsql = git https://github.com/epgsql/epgsql master
dep_lager = git https://github.com/erlang-lager/lager master
dep_sherlock = git https://github.com/andranat-b/sherlock master
dep_uuid = git https://github.com/avtobiff/erlang-uuid master
# dep_poolboy = git https://github.com/devinus/poolboy master

REL_DEPS += relx

ERLC_OPTS += '+{parse_transform, lager_transform}'
include erlang.mk
