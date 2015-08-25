%%%-------------------------------------------------------------------
%%% @author protoj
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. Авг. 2015 23:26
%%%-------------------------------------------------------------------
-module(gcm_xmpp_authorization).
-author("protoj").

%% API
-export([authorize/5]).

-spec authorize(XmppSession, Jid , Password, Server, Port) ->
    {ok, Session :: pid()} | {error, Reason :: term()} when

    XmppSession :: pid(),
    Jid :: binary() | string(),
    Password :: string(),
    Server :: binary() | string(),
    Port :: 1..65536.

authorize(XmppSession, Jid, Password, Server, Port) ->
    try
        ConnectionTimeout = application:get_env(gcm_xmpp, connection_timeout, 60),
        exmpp_session:auth(XmppSession, Jid, Password, "PLAIN"),
        _StreamId = exmpp_session:connect_SSL(XmppSession, Server, Port, [{timeout, ConnectionTimeout * 1000}]),
        {ok, XmppSession}
    catch
        error:{incorrect_jid, Jid}->
            {error, {wrong_jid_creation, Jid, maybe_api_key_lost}};
        throw:{error, 'not-authorized'} ->
            {error, {authorization_failed, Jid, wrong_api_key}};
        Err:Reason ->
            io:format("[gcm_xmpp_auth] ~p occured with reason ~p~n StackTrace: ~p~n", [Err, Reason, erlang:get_stacktrace()]),
            {error, {error_occured, {Err, Reason}}}
    end.
