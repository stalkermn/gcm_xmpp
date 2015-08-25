PROJECT = gcm_xmpp
DEPS = exmpp jsx lager

ERLC_OPTS += +'{parse_transform, lager_transform}' +'{lager_truncation_size, 1024}'

ERLC_OPTS += +'debug_info'

dep_exmpp = git https://github.com/processone/exmpp.git

include erlang.mk
