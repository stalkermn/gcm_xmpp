%%%-------------------------------------------------------------------
%%% @author protoj
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. Авг. 2015 22:48
%%%-------------------------------------------------------------------
-module(gcm_xmpp).
-author("protoj").


-export([start/0]).
-export([
    send_push/2,
    send_push/3,
    send_push/4
]).
%% API
-export([
    start_link/0,
    start_link/2,
    start_link/4,
    close/1
]).

-include("gcm_xmpp.hrl").
-include_lib("exmpp/include/exmpp.hrl").

%% TODO: Create gen_server with callbacks
start_link() ->
    GcmJid = application:get_env(gcm_xmpp, gcm_jid, <<>>),
    ApiKey = application:get_env(gcm_xmpp, gcm_api_key, <<>>),
    start_link(GcmJid, ApiKey).

start_link(GcmJid, ApiKey) ->
    GcmEndpoint = application:get_env(gcm_xmpp, gcm_xmpp_endpoint, "gcm.googleapis.com"),
    start_link(GcmJid, ApiKey, GcmEndpoint).

start_link(GcmJid, ApiKey, GcmEndpoint) ->
    start_link(GcmJid, ApiKey, GcmEndpoint, application:get_env(gcm_xmpp, gcm_xmpp_port, 5235)).

start_link(GcmJid, ApiKey, GcmEndpoint, GcmEndpointPort) ->
    gcm_xmpp_srv:start_link(GcmJid, ApiKey, GcmEndpoint, GcmEndpointPort).

close(Session) ->
    gcm_xmpp_srv:stop(Session).

send_push(Session, To, Title, Text)->
    send_push(Session, To, #notification{body = Text, title = Title}).

send_push(Session, To, #notification{} = Notification)->
    send_push(Session, #gcm_message{
        to = To,
        payload = Notification
    });
send_push(Session, To, Data)->
    send_push(Session, #gcm_message{to= To, payload = Data}).

send_push(Session, #gcm_message{} = GcmMessage)->
    gen_server:call(Session, GcmMessage, 300000).



start()->
    lager:start(),
    exmpp:start(),
    ssl:start(),
    application:start(gcm_xmpp).