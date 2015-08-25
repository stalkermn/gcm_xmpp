%%%-------------------------------------------------------------------
%%% @author protoj
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. Авг. 2015 10:14
%%%-------------------------------------------------------------------
-module(gcm_xmpp_validator).
-author("protoj").

-include("gcm_xmpp.hrl").
%% API
-export([gcm_message_validate/1]).


gcm_message_validate(#gcm_message{to = undefined}) ->
    throw({package_validation, <<"param 'to' requiered">>});

gcm_message_validate(#gcm_message{data = undefined, notification = undefined}) ->
    throw({package_validation, <<"param 'notification' or 'data' not found">>});

gcm_message_validate(#gcm_message{notification = Notification}) when not is_record(Notification, notification) ->
    throw({package_validation, <<"wrong datatype in 'notification' param">>});

gcm_message_validate(#gcm_message{notification = Notification, data = Data})
    when is_record(Notification, notification) andalso Data =/= undefined ->
    throw({package_validation, <<"only one message accepted 'notification' or 'data' ">>});


gcm_message_validate(#gcm_message{notification = #notification{body = Text, title = Title}} = GcmMessage) ->
    if
        Text =/= undefined
            andalso
         is_binary(Text) -> ok;
        true ->
            throw({package_validation, <<" 'notification.text' absent or wrong type ">>})
    end,

    if
        Title =/= undefined andalso is_binary(Title) ->
            other_gcm_message_validate(GcmMessage);
        true ->
            throw({package_validation, <<" 'notification.title' absent or wrong type ">>})
    end;

gcm_message_validate(#gcm_message{data = Data} = GcmMessage) ->
    case jsx:is_term(Data) of
        true ->
            other_gcm_message_validate(GcmMessage);
        false ->
            throw({package_validation, <<"'data' have wrong type. Must be in jsx_term format">>})
    end.

other_gcm_message_validate(#gcm_message{
                            ttl = TTL,
                            delivery_receipt_requested = DelivFlag,
                            delay_while_idle = IdleDelay
                          }) ->
    if
        IdleDelay =/= undefined andalso not is_boolean(IdleDelay) ->
            throw({package_validation, <<"'gcm_message.delay_while_idle' have wrong type">>});

        DelivFlag =/= undefined andalso not is_boolean(DelivFlag) ->
            throw({package_validation, <<"'gcm_message.delivery_receipt_requested' have wrong type">>});

        TTL =/= undefined andalso not is_binary(TTL) ->
            throw({package_validation, <<"'gcm_message.ttl' have wrong type">>});

        true -> ok
    end.
