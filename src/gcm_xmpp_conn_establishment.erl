%%%-------------------------------------------------------------------
%%% @author protoj
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. Авг. 2015 23:26
%%%-------------------------------------------------------------------
-module(gcm_xmpp_conn_establishment).
-author("protoj").

%% -include_lib("exmpp/include/exmpp.hrl")
%% API
-export([establish/4]).

-spec establish(Jid , Password, Server, Port) ->
    {ok, Session :: pid()} | {error, Reason :: term()} when

    Jid :: binary() | string(),
    Password :: string(),
    Server :: binary() | string(),
    Port :: 1..65536.

establish(GCMJid, Password, Server, Port) ->
    try
        JID = exmpp_jid:parse(GCMJid),
        XmppSession = exmpp_session:start({1,0}),
        exmpp_session:auth_info(XmppSession, JID, Password),
        {ok, _StreamID, Feautures} = exmpp_session:connect_SSL(XmppSession, Server, Port,[{starttls, enabled}]),
        lager:debug("[~p] connection establishing result ~ts", [?MODULE, exmpp_xml:document_to_binary(Feautures)]),
        {ok, session_installing(XmppSession)}
    catch
        error:{incorrect_jid, _Jid}->
            {error, {wrong_jid_creation, GCMJid, maybe_api_key_lost}};
        throw:{error, 'not-authorized'} ->
            {error, {authorization_failed, GCMJid, wrong_api_key}};
        Err:Reason ->
            io:format("[gcm_xmpp_conn_establishment] ~p occured with reason ~p~n StackTrace: ~p~n", [Err, Reason, erlang:get_stacktrace()]),
            {error, {error_occured, {Err, Reason}}}
    end.

session_installing(XmppSession) ->
    {ok, Jid} = exmpp_session:login(XmppSession, "PLAIN"),
    lager:debug("[~p] session_installing GCM - Ready : ~p", [?MODULE, Jid]),
    exmpp_session:send_packet(XmppSession,
        exmpp_presence:set_status(
            exmpp_presence:available(), "Ready")),
    XmppSession.