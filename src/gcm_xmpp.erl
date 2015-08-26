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

%% API
-export([
    start_link/0,
    start_link/2,
    start_link/4,
    close/1
]).

-export([
    send_to_sync/2,
    send_push/2,
    send_push/3,
    send_push/5
]).

-include("gcm_xmpp.hrl").
-include_lib("exmpp/include/exmpp.hrl").

-spec start_link()->
    {ok, pid()} | {error, Reason :: term()}.
start_link() ->
    GcmJid = application:get_env(gcm_xmpp, gcm_jid, <<>>),
    ApiKey = application:get_env(gcm_xmpp, gcm_api_key, <<>>),
    start_link(GcmJid, ApiKey).

-spec start_link(binary(), binary())->
    {ok, pid()} | {error, Reason :: term()}.
start_link(GcmJid, APIKey) ->
    GcmEndpoint = application:get_env(gcm_xmpp, gcm_xmpp_endpoint, "gcm.googleapis.com"),
    start_link(GcmJid, APIKey, GcmEndpoint).

-spec start_link(binary(), binary(), binary())->
    {ok, pid()} | {error, Reason :: term()}.
start_link(GcmJid, APIKey, GcmEndpoint) ->
    start_link(GcmJid, APIKey, GcmEndpoint, application:get_env(gcm_xmpp, gcm_xmpp_port, 5235)).

-spec start_link(binary(), binary(), binary(), integer())->
    {ok, pid()} | {error, Reason :: term()}.
start_link(GcmJid, APIKey, GcmEndpoint, GcmEndpointPort) ->
    gcm_xmpp_srv:start_link(GcmJid, APIKey, GcmEndpoint, GcmEndpointPort).

-spec close(pid()) -> ok.
close(Session) ->
    gcm_xmpp_srv:stop(Session).

-spec send_push(pid(), binary(), binary(), binary(), binary()) -> Reply when
    Reply :: gcm_reply().
send_push(Session, To, Title, Text, Icon)->
    send_push(Session, To, #notification{body = Text, title = Title, icon = Icon}).

-spec send_push(pid(), binary(), Payload) -> Reply when
    Payload :: gcm_payload(),
    Reply :: gcm_reply().
send_push(Session, To, #notification{} = Notification)->
    send_push(Session, #gcm_message{
        to = To,
        payload = Notification
    });

send_push(Session, To, Data)->
    send_push(Session, #gcm_message{to= To, payload = Data}).

-spec send_push(pid(), Message) -> Reply when
    Message :: #gcm_message{},
    Reply :: gcm_reply().
send_push(Session, #gcm_message{} = GcmMessage)->
    gen_server:call(Session, GcmMessage, 300000).

-spec send_to_sync(pid(), RegistrationId) -> Reply when
    RegistrationId :: registration_id(),
    Reply :: gcm_reply().
send_to_sync(Session, To)->
    send_push(Session, #gcm_message{to = To}).



start()->
    lager:start(),
    exmpp:start(),
    ssl:start(),
    application:start(gcm_xmpp).