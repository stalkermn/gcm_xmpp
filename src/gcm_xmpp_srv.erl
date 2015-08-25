%%%-------------------------------------------------------------------
%%% @author protoj
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. Авг. 2015 11:23
%%%-------------------------------------------------------------------
-module(gcm_xmpp_srv).
-author("protoj").

-behaviour(gen_server).

-include("gcm_xmpp.hrl").
-include_lib("exmpp/include/exmpp.hrl").
-include_lib("exmpp/include/exmpp_client.hrl").

%% API
-export([start_link/4]).
-export([stop/1]).
%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).

-record(request, {
    message_id :: list() | binary(),
    from :: {pid(), Tag :: term()}
}).
-record(st, {
    session :: pid(),
    clients_table :: ets:tab(),
    connection_cfg :: list( string() | integer() )
}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link(GcmJid, ApiKey, GcmEndpoint, GcmEndpointPort) ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()} when

    GcmJid :: string(),
    ApiKey :: string(),
    GcmEndpoint :: string(),
    GcmEndpointPort :: integer().
start_link(GcmJid, ApiKey, GcmEndpoint, GcmEndpointPort) ->
    gen_server:start_link(?MODULE, [GcmJid, ApiKey, GcmEndpoint, GcmEndpointPort], []).

stop(Session)->
    gen_server:call(Session, stop).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
    {ok, State :: #st{}} | {ok, State :: #st{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init([GCMJid, ApiKey, GcmEndpoint, GcmEndpointPort] = Config) ->
    case establish_xmpp_connection(GCMJid, ApiKey, GcmEndpoint, GcmEndpointPort) of
        {ok, Session} ->
            Tid = ets:new(gcm_worker_cache, [public, set, {keypos, #request.message_id}]),
            {ok, #st{session = Session, clients_table = Tid, connection_cfg = Config}};
        {error, ErrorReason} ->
            lager:error("[gcm_xmpp_srv] error establishing connection: ~p", [ErrorReason]),
            {stop, {shutdown, {connection_error, ErrorReason}}}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Request, From :: {pid(), Tag :: term()},
    State :: #st{}) ->
    {reply, Reply, NewState :: #st{}} |
    {noreply, NewState :: #st{}} |
    {stop, normal, ok, #st{}} |
    {stop, {shutdown, {unexpected_call, UnexpecterCall :: term()}}, #st{}} when

    Request :: #gcm_message{},
    Reply :: #gcm_ack{} | #gcm_nack{} | #gcm_error{}.

handle_call(#gcm_message{} = GcmMessage, _From, #st{session = undefined} = State) ->
    #st{
        connection_cfg = [GCMJid, ApiKey, GcmEndpoint, GcmEndpointPort]
    } = State,

    case establish_xmpp_connection(GCMJid, ApiKey, GcmEndpoint, GcmEndpointPort) of
        {ok, Session} ->
            handle_call(GcmMessage, _From, State#st{session = Session});
        {error, ErrorReason} ->
            {stop, {shutdown, {connection_error, ErrorReason}}, {error, ErrorReason}, State}
    end;

handle_call(#gcm_message{} = GcmMessage, From, State) ->
    case is_process_alive(State#st.session) of
        true ->
            send_gcm_push(GcmMessage, From, State);
        _ ->
            handle_call(GcmMessage, From, State#st{session = undefined})
    end;

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call(UnexpecterCall, _From, State) ->
    lager:error("[~p] unexpected call in ~p: ~p", [?MODULE, self(), UnexpecterCall]),
    {stop, {shutdown, {unexpected_call, UnexpecterCall}}, State}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #st{}) ->
    {stop, {shutdown, {unexpected_cast, term()}}, #st{}}).
handle_cast(Request, State) ->
    lager:error("[~p] unexpected cast in ~p: ~p", [?MODULE, self(), Request]),
    {stop, {shutdown, {unexpected_cast, Request}} , State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec handle_info(Info, State :: #st{}) ->
    {noreply, #st{}} |
    {stop, {shutdown, {unexpected_info, term()}}, #st{}} when
    Info :: {'DOWN', reference(),process, pid(), Reason :: term()} | #received_packet{}.

handle_info({'DOWN',_,process, Pid, Reason}, #st{session = Pid} = State) ->
    lager:info("[~p] xmpp connection ~p down with reason ~p", [?MODULE, Pid, Reason]),
    PendingClients = ets:tab2list(State#st.clients_table),
    reply2error(State#st.clients_table, PendingClients),
    {noreply, State#st{session = undefined}};

handle_info(#received_packet{raw_packet = RawPacket}, State) ->
    Data = exmpp_xml:get_element(RawPacket, "gcm"),
    {MessageId, DisassembledMessage} = gcm_xmpp_assembler:disassemble_data(Data),
    case DisassembledMessage of
        #gcm_control{control_type = <<"CONNECTION_DRAINING">>} ->
            catch exmpp_session:stop(State#st.session);
        ReplyMessage ->
            Clients2Reply = ets:lookup(State#st.clients_table, MessageId),
            reply2(State#st.clients_table, Clients2Reply, ReplyMessage)
    end,
    {noreply, State};

handle_info(Info, State) ->
    lager:error("[~p] unexpected info in ~p: ~p", [?MODULE, self(), Info]),
    {stop, {shutdown, {unexpected_info, Info}}, State}.



%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #st{}) -> term()).
terminate(_Reason, State) ->
    lager:info("_Reason : ~p", [_Reason]),
    PendingClients = ets:tab2list(State#st.clients_table),
    reply2error(State#st.clients_table, PendingClients),
    catch exmpp_session:stop(State#st.session),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #st{},
    Extra :: term()) ->
    {ok, NewState :: #st{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

establish_xmpp_connection(GCMJid, ApiKey, GcmEndpoint, GcmEndpointPort) ->
    case gcm_xmpp_conn_establishment:establish(GCMJid, ApiKey, GcmEndpoint, GcmEndpointPort) of
        {ok, Session} ->

            erlang:monitor(process, Session),
            {ok, Session};
        {error, Reason} ->
            {error, Reason}
    end.

send_gcm_push(GcmMessage, From, State) ->
    lager:debug("[~p] send_gcm_push raw: ~p", [?MODULE, lager:pr(GcmMessage, ?MODULE)]),
    case send_push(State#st.session, GcmMessage) of
        {ok, Id} ->
            ets:insert(State#st.clients_table, #request{message_id = Id, from = From}),
            {noreply, State};
        {error, Reason} ->
%%             TODO: change to #gcm_error{}
            {reply, {error, Reason}, State}
    end.

send_push(Session, GcmMessage) when is_pid(Session) andalso is_record(GcmMessage, gcm_message)->

    try
        XmppPacket = gcm_xmpp_assembler:assemble_xmpp_packet(GcmMessage),
        lager:debug("[~p] send_push assembled xmpp packet: ~ts", [?MODULE, exmpp_xml:document_to_binary(XmppPacket)]),
        XmppMessageId = send_packet(Session, XmppPacket),
        lager:debug("[~p] xmpp packet sending result: ~p", [?MODULE, XmppMessageId]),
        {ok, XmppMessageId}
    catch
        throw:{package_validation, Reason} ->
            {error, {wrong_gcm_package, Reason}};
        throw:{package_assembling, Reason} ->
            {error, {wrong_gcm_package, Reason}};
        throw:ErrorReason ->
            {error, {packet_sending_error, ErrorReason}};
        Level:ErrorReason ->
            lager:error("unknown error: ~p~nStackTrace: ~p~n", [{Level,ErrorReason}, erlang:get_stacktrace()]),
            {error, {unknown_error, {Level, ErrorReason}}}
    end.


send_packet(Session, Packet) when is_tuple(Packet) ->
    exmpp_session:send_packet(Session, Packet).

reply2error(ClientsTable, Clients2Reply) ->
    [ begin
          gen:reply(Client#request.from, {error, Client}),
          ets:delete_object(ClientsTable, Client)
      end || Client <- Clients2Reply].

reply2(ClientsTable, Clients2Reply, ReplyMessage) ->

    [ begin
          gen:reply(Client#request.from, ReplyMessage),
          ets:delete_object(ClientsTable, Client)
      end || Client <- Clients2Reply].
