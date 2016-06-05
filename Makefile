PROJECT = turnip
PROJECT_DESCRIPTION = Wrapper around RabbitMQ AMQP lib
PROJECT_VERSION = 0.0.1

DEPS = amqp any poolboy
ERL_LIBS = deps

ERLC_OPTS = +debug_info \
            +compressed \
            +report \
            +warn_export_all \
            +warn_export_vars \
            +warn_shadow_vars \
            +warn_unused_function \
            +warn_deprecated_function \
            +warn_obsolete_guard \
            +warn_unused_import \
            +nowarn_export_vars


dep_amqp = git git@github.com:rabbitmq/rabbitmq-erlang-client.git master
dep_any = git git@github.com:shortishly/any.git master

COVER = 1

DIALYZER_DIRS = ebin

include erlang.mk
