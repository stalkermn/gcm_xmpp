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

gcm_message_validate(#gcm_message{payload = #notification{body = Text, title = Title, icon = Icon}} = GcmMessage) ->
    if
        Text =/= undefined
            andalso
         is_binary(Text) -> ok;
        true ->
            throw({package_validation, <<" 'notification.text' absent or wrong type ">>})
    end,
    if
        Icon =/= undefined andalso is_binary(Icon) -> ok;
        true ->
            throw({package_validation, <<" 'notification.icon' absent or wrong type ">>})
    end,
    if
        Title =/= undefined andalso is_binary(Title) ->
            other_gcm_message_validate(GcmMessage);
        true ->
            throw({package_validation, <<" 'notification.title' absent or wrong type ">>})
    end;

gcm_message_validate(#gcm_message{payload = Data} = GcmMessage) when is_list(Data) ->
    case jsx:is_term(Data) of
        true ->
            other_gcm_message_validate(GcmMessage);
        false ->
            throw({package_validation, <<"'data' have wrong type. Must be in jsx_term format">>})
    end;

gcm_message_validate(#gcm_message{payload = undefined} = GcmMessage) ->
    other_gcm_message_validate(GcmMessage).



other_gcm_message_validate(#gcm_message{
                            ttl = TTL,
                            delivery_receipt_requested = DelivFlag,
                            delay_while_idle = IdleDelay,
                            collapse_key = CollapseKey,
                            content_available = ContentAvailable,
                            priority = Priority,
                            dry_run = DryRun
                          }) ->
    if
        IdleDelay =/= undefined andalso not is_boolean(IdleDelay) ->
            throw({package_validation, <<"'gcm_message.delay_while_idle' have wrong type">>});

        DelivFlag =/= undefined andalso not is_boolean(DelivFlag) ->
            throw({package_validation, <<"'gcm_message.delivery_receipt_requested' have wrong type">>});

        ContentAvailable =/= undefined andalso not is_boolean(ContentAvailable) ->
            throw({package_validation, <<"'gcm_message.content_available' have wrong type">>});

        DryRun =/= undefined andalso not is_boolean(DryRun) ->
            throw({package_validation, <<"'gcm_message.dry_run' have wrong type">>});

        TTL =/= undefined andalso not is_binary(TTL) ->
            throw({package_validation, <<"'gcm_message.ttl' have wrong type">>});

        Priority =/= undefined andalso not is_binary(Priority) ->
            throw({package_validation, <<"'gcm_message.priority' have wrong type">>});

        CollapseKey =/= undefined andalso not is_binary(CollapseKey) ->
            throw({package_validation, <<"'gcm_message.collapse_key' have wrong type">>});

        true -> ok
    end.
