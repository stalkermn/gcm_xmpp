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
    start_link/3,
    start_link/4,
    close/1
]).

-include("gcm_xmpp.hrl").
-include_lib("exmpp/include/exmpp.hrl").

%% TODO: Create gen_server with callbacks
start_link() ->
    GCMSenderId = application:get_env(gcm_xmpp, gcm_sender_id, <<>>),
    ApiKey = application:get_env(gcm_xmpp, gcm_api_key, <<>>),
    start_link(GCMSenderId, ApiKey).

start_link(GCMSenderId, ApiKey) ->
    GcmEndpoint = application:get_env(gcm_xmpp, gcm_xmpp_endpoint, "gcm.googleapis.com"),
    start_link(GCMSenderId, ApiKey, GcmEndpoint).

start_link(GCMSenderId, ApiKey, GcmEndpoint) ->
    start_link(GCMSenderId, ApiKey, GcmEndpoint, application:get_env(gcm_xmpp, gcm_xmpp_port, 5235)).

start_link(GCMSenderId, ApiKey, GcmEndpoint, GcmEndpointPort) when ApiKey =/= <<>> andalso
                                                      GcmEndpoint =/= <<>> andalso
                                                      is_integer(GcmEndpointPort) ->
    gcm_xmpp_srv:start_link(GCMSenderId, ApiKey, GcmEndpoint, GcmEndpointPort).

close(Session) ->
    gcm_xmpp_srv:stop(Session).

send_push(Session, To, Title, Text)->
    send_push(Session, To, #notification{body = Text, title = Title}).

send_push(Session, To, #notification{} = Notification)->
    send_push(Session, #gcm_message{
        to = To,
        notification = Notification
    });
send_push(Session, To, Data)->
    send_push(Session, #gcm_message{to= To, data = Data}).

send_push(Session, #gcm_message{} = GcmMessage)->
    gen_server:call(Session, GcmMessage, 300000).



start()->
    lager:start(),
    exmpp:start(),
    ssl:start(),
    application:start(gcm_xmpp),
    {ok, Pid} = gcm_xmpp:start_link(),
    Token = <<"APA91bEqgEBhhwcT2Pj7s6pYBzGnncjYlYQ9HkqtY0hNSp9nfG_pCKNW2qcXOlC2pHqqn0ZzKkJU8Fl7OHL-O97MKUBR-q8YzSUZPopAofDZkvYO-qdOOwE2jl0okdNy5xwRRizBUnrDCbYjTlF_4ELpjmf8gC6twEWNxM3XRZGH7z57T_ixgKE">>,
    gcm_xmpp:send_push(Pid, Token, <<"hello">>, <<"world">>).