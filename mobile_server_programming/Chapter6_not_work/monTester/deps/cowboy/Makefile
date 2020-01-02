# See LICENSE for licensing information.

PROJECT = cowboy

# Options.

ERLC_OPTS ?= -Werror +debug_info +warn_export_all +warn_export_vars \
	+warn_shadow_vars +warn_obsolete_guard +warn_missing_spec
COMPILE_FIRST = cowboy_middleware cowboy_sub_protocol
CT_SUITES = eunit http loop_handler spdy ws
CT_OPTS += -pa test -ct_hooks cowboy_ct_hook []
PLT_APPS = crypto public_key ssl

# Dependencies.

DEPS = cowlib ranch
dep_cowlib = pkg://cowlib 0.6.2
dep_ranch = pkg://ranch 0.10.0

TEST_DEPS = ct_helper gun
dep_ct_helper = https://github.com/extend/ct_helper.git master
dep_gun = pkg://gun master

# Standard targets.

include erlang.mk
