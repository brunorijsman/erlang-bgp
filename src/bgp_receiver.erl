%%%=====================================================================================================================
%%% Copyright (c) 2009, Bruno Rijsman
%%%
%%% Permission to use, copy, modify, and/or distribute this software for any purpose with or without fee is hereby 
%%% granted, provided that the above copyright notice and this permission notice appear in all copies.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL 
%%% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, 
%%% INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN
%%% AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR 
%%% PERFORMANCE OF THIS SOFTWARE.
%%%=====================================================================================================================

%% @author Bruno Rijsman
%% @copyright 2009 Bruno Rijsman

-module(bgp_receiver).
-author('Bruno Rijsman').

-behavior(gen_server).

-include("bgp_receiver.hrl").
-include("bgp_messages.hrl").
-include("constants.hrl").                  %% TODO: rename this

%% public API

-export([start_link/2,
         stop/1]).

%% gen_server callbacks

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%%----------------------------------------------------------------------------------------------------------------------

start_link(ConnectionFsmPid, Socket) ->
    gen_server:start_link(?MODULE, [ConnectionFsmPid, Socket], []).

%%----------------------------------------------------------------------------------------------------------------------

stop(ReceiverPid) ->
    gen_server:call(ReceiverPid, {stop}).

%%----------------------------------------------------------------------------------------------------------------------

init([ConnectionFsmPid, Socket]) ->
    % TODO: become the controlling process for the socket. There is no race condition because we are the first and the
    % first and only ones to read the socket.
    ReceiveMessageLoopPid = spawn_link(fun () -> receive_message_loop(ConnectionFsmPid, Socket) end),
    State = #bgp_receiver_state{connection_fsm_pid = ConnectionFsmPid, 
                                receiver_pid = ReceiveMessageLoopPid, 
                                socket = Socket},
    {ok, State}.

%%----------------------------------------------------------------------------------------------------------------------

handle_call({stop}, _From, State) ->
    #bgp_receiver_state{receiver_pid = ReceiveMessageLoopPid} = State,
    case ReceiveMessageLoopPid of
        none ->
            ok;
        _ ->
            exit(ReceiveMessageLoopPid, normal)
    end,
    {stop, normal, stopped, State}.

%%----------------------------------------------------------------------------------------------------------------------

handle_cast(_Message, State) ->
    {noreply, State}.

%%----------------------------------------------------------------------------------------------------------------------

handle_info(_Info, State) ->
    {noreply, State}.

%%----------------------------------------------------------------------------------------------------------------------

terminate(_Reason, _State) ->
    %% TODO: do we need to do the same stuff as stop here?
    ok.

%%----------------------------------------------------------------------------------------------------------------------

code_change(_OldVersion, State, _Extra) ->
    {ok, State}.

%%----------------------------------------------------------------------------------------------------------------------

%% TODO: we need to make a distinction between a badly formatted received message and a socket error ("closed").
%% TODO: in the case of a socket error, we need to report and remember errno.

receive_message_loop(ConnectionFsmPid, Socket) ->
    Message = try receive_message(ConnectionFsmPid, Socket)
    catch
        throw:{tcp_error, _Reason} ->
            io:format("TCP error while receiving message~n"),   %% @@@
            error;         %% TODO: handle this; send event to FSM
        throw:{decode_error, Code, SubCode, Data} ->
            io:format("Received message was malformed, Code=~p SubCode=~p Data=~p~n", [Code, SubCode, Data]),  %% @@@
            error;         %% TODO: handle this; send event to FSM
        throw:{notification_decode_error} ->
            io:format("Received notification was malformed~n"),  %% @@@
            error          %% TODO: handle this; send event to FSM
    end,
    case Message of
        error ->
            error;
        _ ->
            io:format("Received message: ~p~n", [Message]),
            receive_message_loop(ConnectionFsmPid, Socket)
    end.

%%----------------------------------------------------------------------------------------------------------------------

receive_message(ConnectionFsmPid, Socket) ->
    MarkerData = case gen_tcp:recv(Socket, 16) of
        {ok, Data1} ->
            Data1;
        {error, Reason1} ->
            throw({tcp_error, Reason1})
    end,
    case MarkerData of 
        <<16#ffffffffffffffffffffffffffffffff:128>> ->
            ok;
        _ ->
            throw({decode_error, 
                   ?BGP_ERROR_CODE_MESSAGE_HEADER_ERROR, 
                   ?BGP_ERROR_SUB_CODE_CONNECTION_NOT_SYNCHRONIZED, 
                   <<>>})
    end,
    MessageLength = case gen_tcp:recv(Socket, 2) of
        {ok, <<MessageLength1:16>>} ->
            MessageLength1;
        {error, Reason2} ->
            throw({tcp_error, Reason2})
    end,
    if 
        (MessageLength < ?BGP_MESSAGE_MIN_LENGTH) or (MessageLength > ?BGP_MESSAGE_MAX_LENGTH) ->
            throw({decode_error, 
                   ?BGP_ERROR_CODE_MESSAGE_HEADER_ERROR, 
                   ?BGP_ERROR_SUB_CODE_BAD_MESSAGE_LENGTH,
                   <<MessageLength:16>>});
        true ->
            ok
    end,
    TypeData = case gen_tcp:recv(Socket, 1) of
        {ok, Data3} ->
            Data3;
        {error, Reason3} ->
            throw({tcp_error, Reason3})
    end,
    <<Type:8>> = TypeData,
    {MinLength, MaxLength, DecodeFunction} = case Type of
        ?BGP_MESSAGE_TYPE_OPEN ->
            {?BGP_OPEN_MESSAGE_MIN_LENGTH, 
             ?BGP_OPEN_MESSAGE_MAX_LENGTH, 
             fun(FsmPid, Data) -> decode_open(FsmPid, Data) end};
        ?BGP_MESSAGE_TYPE_UPDATE ->
            {?BGP_UPDATE_MESSAGE_MIN_LENGTH, 
             ?BGP_UPDATE_MESSAGE_MAX_LENGTH, 
             fun(FsmPid, Data) -> decode_update(FsmPid, Data) end};
        ?BGP_MESSAGE_TYPE_NOTIFICATION ->
            {?BGP_NOTIFICATION_MESSAGE_MIN_LENGTH, 
             ?BGP_NOTIFICATION_MESSAGE_MAX_LENGTH, 
             fun(FsmPid, Data) -> decode_notification(FsmPid, Data) end};
        ?BGP_MESSAGE_TYPE_KEEP_ALIVE ->
            {?BGP_KEEP_ALIVE_MESSAGE_MIN_LENGTH, 
             ?BGP_KEEP_ALIVE_MESSAGE_MAX_LENGTH, 
             fun(FsmPid, Data) -> decode_keep_alive(FsmPid, Data) end};
        ?BGP_MESSAGE_TYPE_ROUTE_REFRESH ->
            {?BGP_ROUTE_REFRESH_MESSAGE_MIN_LENGTH,
             ?BGP_ROUTE_REFRESH_MESSAGE_MIN_LENGTH, 
             fun(FsmPid, Data) -> decode_route_refresh(FsmPid, Data) end};
        _ ->
            throw({decode_error,
                   ?BGP_ERROR_CODE_MESSAGE_HEADER_ERROR, 
                   ?BGP_ERROR_SUB_CODE_BAD_MESSAGE_TYPE,
                   TypeData})
    end,
    if
        (MessageLength < MinLength) or (MessageLength > MaxLength) ->
            throw({decode_error, 
               ?BGP_ERROR_CODE_MESSAGE_HEADER_ERROR, 
               ?BGP_ERROR_SUB_CODE_BAD_MESSAGE_LENGTH,
               <<MessageLength:16>>});
        true ->
            ok
    end,
    MessageData = case gen_tcp:recv(Socket, MessageLength - 19) of
        {ok, Data4} ->
            Data4;
        {error, Reason4} ->
            throw({tcp_error, Reason4})
    end,
    DecodeFunction(ConnectionFsmPid, MessageData).

%%----------------------------------------------------------------------------------------------------------------------

decode_open(ConnectionFsmPid, Data) ->
    <<Version:8, MyAs:16, HoldTime:16, Identifier:32, OptParamsLength:8, OptParamsData/binary>> = Data,
    OptParams = decode_open_optional_paramaters(OptParamsData),
    SupportedVersion = 4,
    if
        Version == SupportedVersion ->
            ok;
        true ->
            %% TODO: special event for FSM?
            throw({decode_error, 
                   ?BGP_ERROR_CODE_OPEN_MESSAGE_ERROR, 
                   ?BGP_ERROR_SUB_CODE_UNSUPPORTED_VERSION_NUMBER, 
                   <<SupportedVersion:16>>})
    end,
    if
        MyAs == 7675 ->             %% TODO: replace by real check
            ok;
        true ->
            throw({decode_error, 
                   ?BGP_ERROR_CODE_OPEN_MESSAGE_ERROR, 
                   ?BGP_ERROR_SUB_CODE_BAD_PEER_AS, 
                   <<>>})
    end,
    if
        (HoldTime == 0) or (HoldTime > 2) ->
            ok;
        true ->
            throw({decode_error, 
                   ?BGP_ERROR_CODE_OPEN_MESSAGE_ERROR, 
                   ?BGP_ERROR_SUB_CODE_UNACCEPTABLE_HOLD_TIME, 
                   <<>>})
    end,
    %% TODO: check syntax of identifier; it must be a "valid unicast IP host address"
    io:format("Receive OPEN~n"),
    io:format("  version = ~p~n", [Version]),
    io:format("  my-as = ~p~n", [MyAs]),
    io:format("  hold-time = ~p~n", [HoldTime]),
    io:format("  identifier = ~p~n", [Identifier]),
    io:format("  opt-params-length = ~p~n", [OptParamsLength]),
    io:format("  opt-params = ~p~n", [OptParams]),
    %% TODO: need to store optional parameters in the record
    %% TODO: need to validate the fixed parts and the optional paramters
    Open = #bgp_open{version=Version, my_as=MyAs, hold_time=HoldTime, identifier=Identifier, options=OptParams},
    bgp_connection_fsm:receive_open(ConnectionFsmPid),
    Open.

%%----------------------------------------------------------------------------------------------------------------------

decode_open_optional_paramaters(OptParamsData) ->
    decode_open_optional_paramaters([], OptParamsData).

%%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

decode_open_optional_paramaters(OptParams, <<>>) ->
    OptParams;

%%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

decode_open_optional_paramaters(OptParams, OptParamsData) ->
    io:format("Decoding optional parameters: ~p~n", [OptParamsData]),
    {Type, Data, Rest} = case OptParamsData of
        <<Type1, Length1, Data1:Length1/binary, Rest1/binary>> ->
            {Type1, Data1, Rest1};
        _ ->
            throw({decode_error, ?BGP_ERROR_CODE_OPEN_MESSAGE_ERROR, ?BGP_ERROR_SUB_CODE_UNSPECIFIC, <<>>})
    end,
    case Type of
        ?BGP_OPEN_PARAMETER_DEPRECATED_AUTHENTICATION ->
            OptParam = {authentication};                     %% TODO: Add authentication data
        ?BGP_OPEN_PARAMETER_CAPABILITIES ->
            OptParam = {capabilities, decode_capabilities(Data)};
        _ ->
            OptParam = {unknown, Type, Data}
    end,
    NewOptParams = OptParams ++ [OptParam],
    decode_open_optional_paramaters(NewOptParams, Rest).

%%----------------------------------------------------------------------------------------------------------------------

decode_capabilities(CapabilitiesData) ->
    decode_capabilities([], CapabilitiesData).

%%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

decode_capabilities(Capabilities, <<>>) ->
    Capabilities;

%%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

decode_capabilities(Capabilities, CapabilitiesData) ->
    io:format("Decoding capabilities: ~p~n", [CapabilitiesData]),
    {Code, Data, Rest} = case CapabilitiesData of
        <<Code1, Length1, Data1:Length1/binary, Rest1/binary>> ->
            {Code1, Data1, Rest1};
        _ ->
            throw({decode_error, ?BGP_ERROR_CODE_OPEN_MESSAGE_ERROR, ?BGP_ERROR_SUB_CODE_UNSPECIFIC, <<>>})
    end,
    Capability = case Code of
        ?BGP_CAPABILITY_CODE_RESERVED ->
            {reserved, Data};
        ?BGP_CAPABILITY_CODE_MULTI_PROTOCOL ->
            decode_capability_multi_protocol(Data);
        ?BGP_CAPABILITY_CODE_ROUTE_REFRESH ->
            decode_capability_route_refresh(Data);
        ?BGP_CAPABILITY_CODE_OUTBOUND_ROUTE_FILTERING ->
            decode_capability_outbound_route_filtering(Data);
        ?BGP_CAPABILITY_CODE_MULTIPLE_ROUTES_TO_DESTINATION ->
            decode_capability_multiple_routes_to_destination(Data);
        ?BGP_CAPABILITY_CODE_GRACEFUL_RESTART ->    
            decode_capability_graceful_restart(Data);
        ?BGP_CAPABILITY_CODE_FOUR_OCTET_AS ->
            decode_capability_four_octet_as(Data);
        ?BGP_CAPABILITY_CODE_DYNAMIC_CAPABILITY ->
            decode_capability_dynamic_capability(Data);
        ?BGP_CAPABILITY_CODE_MULTI_SESSION ->
            decode_capability_multi_session(Data);
        ?BGP_CAPABILITY_CODE_OLD_ROUTE_REFRESH ->
            decode_capability_route_refresh_old(Data);
        ?BGP_CAPABILITY_CODE_OLD_OUTBOUND_ROUTE_FILTERING ->
            decode_capability_outbound_route_filtering_old(Data);
        _ ->
            {unknown, Code, Data}
    end,
    NewCapabilities = Capabilities ++ [Capability],
    decode_capabilities(NewCapabilities, Rest).

%%----------------------------------------------------------------------------------------------------------------------

decode_capability_multi_protocol(Data) ->
    case Data of
        <<Afi:16, _Reserved:8, Safi:8>> ->
            {multi_protocol, Afi, Safi};
        _ ->
            throw({decode_error, ?BGP_ERROR_CODE_OPEN_MESSAGE_ERROR, ?BGP_ERROR_SUB_CODE_UNSPECIFIC, <<>>})
    end.

%%----------------------------------------------------------------------------------------------------------------------

decode_capability_route_refresh(Data) ->
    case Data of
        <<>> ->
            {route_refresh};
        _ ->
            throw({decode_error, ?BGP_ERROR_CODE_OPEN_MESSAGE_ERROR, ?BGP_ERROR_SUB_CODE_UNSPECIFIC, <<>>})
    end.

%%----------------------------------------------------------------------------------------------------------------------

decode_capability_route_refresh_old(Data) ->
    case Data of
        <<>> ->
            {route_refresh_old};
        _ ->
            %% TODO: For every throw like this, log something like "Malformed route-refresh-old capability".
            throw({decode_error, ?BGP_ERROR_CODE_OPEN_MESSAGE_ERROR, ?BGP_ERROR_SUB_CODE_UNSPECIFIC, <<>>})
    end.

%%----------------------------------------------------------------------------------------------------------------------

decode_capability_outbound_route_filtering(_Data) ->
    %% TODO: Decode this.
    {outbound_route_filtering}.

%%----------------------------------------------------------------------------------------------------------------------

decode_capability_outbound_route_filtering_old(_Data) ->
    %% TODO: Decode this.
    {outbound_route_filtering}.

%%----------------------------------------------------------------------------------------------------------------------

decode_capability_multiple_routes_to_destination(Data) ->
    case Data of
        <<>> ->
            {multiple_routes_to_destination};
        _ ->
            throw({decode_error, ?BGP_ERROR_CODE_OPEN_MESSAGE_ERROR, ?BGP_ERROR_SUB_CODE_UNSPECIFIC, <<>>})
    end.

%%----------------------------------------------------------------------------------------------------------------------

decode_capability_graceful_restart(Data) ->
    case Data of
        <<RestartState:1, _Reserved:3, RestartTime:12, _Rest/binary>> ->
            %% TODO: Decode Rest
            {graceful_restart, RestartState, RestartTime};
        _ ->
            throw({decode_error, ?BGP_ERROR_CODE_OPEN_MESSAGE_ERROR, ?BGP_ERROR_SUB_CODE_UNSPECIFIC, <<>>})
    end.

%%----------------------------------------------------------------------------------------------------------------------

decode_capability_four_octet_as(Data) ->
    case Data of
        <<>> ->
            {four_octet_as};
        _ ->
            throw({decode_error, ?BGP_ERROR_CODE_OPEN_MESSAGE_ERROR, ?BGP_ERROR_SUB_CODE_UNSPECIFIC, <<>>})
    end.

%%----------------------------------------------------------------------------------------------------------------------

decode_capability_dynamic_capability(Data) ->
    case Data of
        <<>> ->
            {dynamic_capability};
        _ ->
            throw({decode_error, ?BGP_ERROR_CODE_OPEN_MESSAGE_ERROR, ?BGP_ERROR_SUB_CODE_UNSPECIFIC, <<>>})
    end.

%%----------------------------------------------------------------------------------------------------------------------

decode_capability_multi_session(Data) ->
    case Data of
        <<>> ->
            {multi_session};
        _ ->
            throw({decode_error, ?BGP_ERROR_CODE_OPEN_MESSAGE_ERROR, ?BGP_ERROR_SUB_CODE_UNSPECIFIC, <<>>})
    end.

%%----------------------------------------------------------------------------------------------------------------------

decode_update(ConnectionFsmPid, Data) ->
    {WithdrawnRoutesData, AttributesData, AdvertisedRoutesData} = case Data of
        <<WithdrawnRoutesLength1:16,
          WithdrawnRoutesData1:WithdrawnRoutesLength1/binary,
          AttributesLength1:16,
          AttributesData1:AttributesLength1/binary,
          AdvertisedRoutesData1/binary>> ->
            {WithdrawnRoutesData1, AttributesData1, AdvertisedRoutesData1};
        _ ->
            throw({decode_error, 
                   ?BGP_ERROR_CODE_UPDATE_MESSAGE_ERROR, 
                   ?BGP_ERROR_SUB_CODE_MALFORMED_ATTRIBUTE_LIST, 
                   <<>>})
    end,
    WithdrawnRoutes = decode_nlri_list(WithdrawnRoutesData),
    Attributes = decode_attributes(AttributesData),
    AdvertisedRoutes = decode_nlri_list(AdvertisedRoutesData),
    Update = #bgp_update{withdrawn_routes = WithdrawnRoutes, 
                          attributes = Attributes, 
                          advertised_routes = AdvertisedRoutes},
    bgp_connection_fsm:receive_update(ConnectionFsmPid),      %% TODO: pass in notification
    Update.

%%----------------------------------------------------------------------------------------------------------------------

decode_nlri_list(Data) ->
    decode_nlri_list([], Data).

%%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

decode_nlri_list(Routes, <<>>) ->
    Routes;

%%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

decode_nlri_list(Routes, Data) ->
    <<LengthInBits:8, Rest1/binary>> = Data,
    LengthInBytes = (LengthInBits + 7) div 8,
    {Prefix, Rest3} = case Rest1 of
        <<PrefixData:LengthInBytes/binary, Rest2/binary>> ->
            {{PrefixData, LengthInBits}, Rest2};
        _ ->
            %% TODO: is this the right error?
            throw({decode_error, 
                   ?BGP_ERROR_CODE_UPDATE_MESSAGE_ERROR, 
                   ?BGP_ERROR_SUB_CODE_MALFORMED_ATTRIBUTE_LIST, 
                   <<>>})
    end,
    %% TODO: force bits not covered by prefix length to 0?
    NewRoutes = [Prefix | Routes],  
    decode_nlri_list(NewRoutes, Rest3).

%%----------------------------------------------------------------------------------------------------------------------

decode_attributes(Data) ->
    decode_attributes(#attributes{}, Data).

%%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

decode_attributes(Attributes, <<>>) ->
    Attributes;

%%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

%% TODO: check for the presense of all mandatory attributes in the FSM (here, we don't know the type of the peer).

decode_attributes(Attributes, Data) ->
    {Flags, Type, LengthAndRestData} = case Data of
        <<Type1:8, Flags1:8, LengthAndRestData1/binary>> ->
            {Flags1, Type1, LengthAndRestData1};
        _ ->
            %% TODO: is this the right error?
            throw({decode_error, 
                   ?BGP_ERROR_CODE_UPDATE_MESSAGE_ERROR, 
                   ?BGP_ERROR_SUB_CODE_MALFORMED_ATTRIBUTE_LIST, 
                   <<>>})
    end,
    <<_:3, ExtendedLength:1, _:4>> = <<Flags:8>>,
   case ExtendedLength of
       0 -> LengthLength = 8;
       1 -> LengthLength = 16
    end,
    {Length, Data, Rest} = case LengthAndRestData of
        <<Length1:LengthLength, Data1:Length1/binary, Rest1/binary>> ->
            {Length1, Data1, Rest1};
        _ ->
            %% TODO: is this the right error?
            throw({decode_error, 
                   ?BGP_ERROR_CODE_UPDATE_MESSAGE_ERROR, 
                   ?BGP_ERROR_SUB_CODE_MALFORMED_ATTRIBUTE_LIST, 
                   <<>>})
    end,
    %% TODO: check for same attribute present more than once. is it always illegal (even for communities, etc.)?
    NewAttributes = case Type of
        ?BGP_PATH_ATTRIBUTE_ORIGIN ->
            decode_origin(Flags, Type, Length, Data, Attributes);
        ?BGP_PATH_ATTRIBUTE_AS_PATH ->
            decode_as_path(Flags, Type, Length, Data, Attributes);
        ?BGP_PATH_ATTRIBUTE_NEXT_HOP ->
            decode_next_hop(Flags, Type, Length, Data, Attributes);
        ?BGP_PATH_ATTRIBUTE_MULTI_EXIT_DISC ->
            decode_multi_exit_desc(Flags, Type, Length, Data, Attributes);
        ?BGP_PATH_ATTRIBUTE_LOCAL_PREF ->
            decode_local_pref(Flags, Type, Length, Data, Attributes);
        ?BGP_PATH_ATTRIBUTE_ATOMIC_AGGREGATE ->
            decode_atomic_aggregate(Flags, Type, Length, Data, Attributes);
        ?BGP_PATH_ATTRIBUTE_AGGREGATOR ->
            decode_aggregator(Flags, Type, Length, Data, Attributes);
        ?BGP_PATH_ATTRIBUTE_COMMUNITY ->
            decode_community(Flags, Type, Length, Data, Attributes);
        ?BGP_PATH_ATTRIBUTE_ORIGINATOR_ID ->
            decode_originator_id(Flags, Type, Length, Data, Attributes);
        ?BGP_PATH_ATTRIBUTE_CLUSTER_LIST ->
            decode_cluster_list(Flags, Type, Length, Data, Attributes);
        ?BGP_PATH_ATTRIBUTE_DESTINATION_PREFERENCE ->
            decode_destination_preference(Flags, Type, Length, Data, Attributes);
        ?BGP_PATH_ATTRIBUTE_ADVERTISER ->
            decode_advertiser(Flags, Type, Length, Data, Attributes);
        ?BGP_PATH_ATTRIBUTE_CLUSTER_ID ->
            decode_cluster_id(Flags, Type, Length, Data, Attributes);
        ?BGP_PATH_ATTRIBUTE_MP_REACH_NLRI ->
            decode_reach_nlri(Flags, Type, Length, Data, Attributes);
        ?BGP_PATH_ATTRIBUTE_MP_UNREACH_NLRI ->
            decode_mp_unreach_nlri(Flags, Type, Length, Data, Attributes);
        ?BGP_PATH_ATTRIBUTE_EXTENDED_COMMUNITIES ->
            decode_extended_communities(Flags, Type, Length, Data, Attributes);
        ?BGP_PATH_ATTRIBUTE_AS4_PATH ->
            decode_as4_path(Flags, Type, Length, Data, Attributes);
        ?BGP_PATH_ATTRIBUTE_AS4_AGGREGATOR ->
            decode_as4_aggregator(Flags, Type, Length, Data, Attributes);
        ?BGP_PATH_ATTRIBUTE_SAFI_SPECIFIC ->
            decode_safi_specific(Flags, Type, Length, Data, Attributes);
        ?BGP_PATH_ATTRIBUTE_CONNECTOR ->
            decode_connector(Flags, Type, Length, Data, Attributes);
        _ ->
            decode_unrecognized_attribute(Flags, Type, Length, Data, Attributes)
    end,
    decode_attributes(NewAttributes, Rest).

%%----------------------------------------------------------------------------------------------------------------------

decode_origin(Flags, Type, Length, Data, Attributes) ->
    ok = check_attribute_flags(Flags, Type, Length, Data, well_known_mandatory),
    ok = check_attribute_length(Flags, Type, Length, Data, 1),
    <<Origin:8>> = Data,
    if 
        (Origin == ?BGP_ORIGIN_IGP) or (Origin == ?BGP_ORIGIN_EGP) or (Origin == ?BGP_ORIGIN_INCOMPLETE) ->
            ok;
        true ->
            attribute_decode_error(Flags, Type, Length, Data, ?BGP_ERROR_SUB_CODE_INVALID_ORIGIN_ATTRIBUTE)
    end,
    Attributes#attributes{origin = Origin}.

%%----------------------------------------------------------------------------------------------------------------------

decode_as_path(Flags, Type, Length, Data, Attributes) ->
    ok = check_attribute_flags(Flags, Type, Length, Data, well_known_mandatory),
    %% TODO: implement this
    Attributes#attributes{as_path = present_todo_decoding}.

%%----------------------------------------------------------------------------------------------------------------------

decode_next_hop(Flags, Type, Length, Data, Attributes) ->
    ok = check_attribute_flags(Flags, Type, Length, Data, well_known_mandatory),
    NextHop = case Length of
        4 ->
            Data;           %% TODO: is this acceptable as an IPv6 address or do we need to convert it to a record
        16 ->
            Data;
        _ ->
            %% TODO: do we need to support next-hops of other lengths than 4 or 16?
            check_attribute_length(Flags, Type, Length, Data, force_error)
    end,
    Attributes#attributes{next_hop = NextHop}.

%%----------------------------------------------------------------------------------------------------------------------

decode_multi_exit_desc(Flags, Type, Length, Data, Attributes) ->
    ok = check_attribute_flags(Flags, Type, Length, Data, optional_non_transitive),
    ok = check_attribute_length(Flags, Type, Length, Data, 4),
    <<Med:32>> = Data,
    Attributes#attributes{med = Med}.

%%----------------------------------------------------------------------------------------------------------------------

decode_local_pref(Flags, Type, Length, Data, Attributes) ->
    % TODO: local-pref is actually mandatory for internal peers and confedration peers; but that is handled in the FSM
    ok = check_attribute_flags(Flags, Type, Length, Data, well_known_discretionary),
    ok = check_attribute_length(Flags, Type, Length, Data, 4),
    <<LocalPref:32>> = Data,
    Attributes#attributes{local_pref = LocalPref}.

%%----------------------------------------------------------------------------------------------------------------------

decode_atomic_aggregate(Flags, Type, Length, Data, Attributes) ->
    ok = check_attribute_flags(Flags, Type, Length, Data, well_known_discretionary),
    ok = check_attribute_length(Flags, Type, Length, Data, 0),
    Attributes#attributes{atomic_aggregate = present}.

%%----------------------------------------------------------------------------------------------------------------------

decode_aggregator(Flags, Type, Length, Data, Attributes) ->
    ok = check_attribute_flags(Flags, Type, Length, Data, optional_transitive),
    ok = check_attribute_length(Flags, Type, Length, Data, 6),
    %% TODO: implement this
    %% TODO: decode AS number (2 octets)
    %% TODO: decode IPv4 address (4 octets)
    Attributes.

%%----------------------------------------------------------------------------------------------------------------------

decode_community(Flags, Type, Length, Data, Attributes) ->
    %% TODO: implement this
    Attributes.

%%----------------------------------------------------------------------------------------------------------------------

decode_originator_id(Flags, Type, Length, Data, Attributes) ->
    %% TODO: implement this
    Attributes.

%%----------------------------------------------------------------------------------------------------------------------

decode_cluster_list(Flags, Type, Length, Data, Attributes) ->
    %% TODO: implement this
    Attributes.

%%----------------------------------------------------------------------------------------------------------------------

decode_destination_preference(Flags, Type, Length, Data, Attributes) ->
    %% TODO: implement this
    Attributes.

%%----------------------------------------------------------------------------------------------------------------------

decode_advertiser(Flags, Type, Length, Data, Attributes) ->
    %% TODO: implement this
    Attributes.

%%----------------------------------------------------------------------------------------------------------------------

decode_cluster_id(Flags, Type, Length, Data, Attributes) ->
    %% TODO: implement this
    Attributes.

%%----------------------------------------------------------------------------------------------------------------------

decode_reach_nlri(Flags, Type, Length, Data, Attributes) ->
    %% TODO: implement this
    Attributes.

%%----------------------------------------------------------------------------------------------------------------------

decode_mp_unreach_nlri(Flags, Type, Length, Data, Attributes) ->
    %% TODO: implement this
    Attributes.

%%----------------------------------------------------------------------------------------------------------------------

decode_extended_communities(Flags, Type, Length, Data, Attributes) ->
    %% TODO: implement this
    Attributes.

%%----------------------------------------------------------------------------------------------------------------------

decode_as4_path(Flags, Type, Length, Data, Attributes) ->
    %% TODO: implement this
    Attributes.

%%----------------------------------------------------------------------------------------------------------------------

decode_as4_aggregator(Flags, Type, Length, Data, Attributes) ->
    %% TODO: implement this
    Attributes.

%%----------------------------------------------------------------------------------------------------------------------

decode_safi_specific(Flags, Type, Length, Data, Attributes) ->
    %% TODO: implement this
    Attributes.

%%----------------------------------------------------------------------------------------------------------------------

decode_connector(Flags, Type, Length, Data, Attributes) ->
    %% TODO: implement this
    Attributes.

%%----------------------------------------------------------------------------------------------------------------------

decode_unrecognized_attribute(Flags, Type, Length, Data, Attributes) ->
    %% TODO: implement this
    Attributes.

%%----------------------------------------------------------------------------------------------------------------------

check_attribute_flags(Flags, Type, Length, Data, AttributeType) ->
    <<Optional:1, Transitive:1, Partial:1, _:5>> = <<Flags:8>>,
    FlagsAreCorrect = case AttributeType of
        well_known_mandatory ->
            (Optional == 0) and (Transitive == 1) and (Partial == 0);
        well_known_discretionary ->
            (Optional == 0) and (Transitive == 1) and (Partial == 0);
        optional_transitive ->
            (Optional == 1) and (Transitive == 1);
        optional_non_transitive ->
            (Optional == 1) and (Transitive == 0) and (Partial == 0)
    end,
    case FlagsAreCorrect of
        true ->
            ok;
        false ->
            attribute_decode_error(Flags, Type, Length, Data, ?BGP_ERROR_SUB_CODE_ATTRIBUTE_FLAGS_ERROR)
    end.

%%----------------------------------------------------------------------------------------------------------------------

check_attribute_length(Flags, Type, Length, Data, ExpectedLength) ->
    if
        Length == ExpectedLength ->
            ok;
        true ->
            attribute_decode_error(Flags, Type, Length, Data, ?BGP_ERROR_SUB_CODE_ATTRIBUTE_LENGTH_ERROR)
    end.

%%----------------------------------------------------------------------------------------------------------------------

attribute_decode_error(Flags, Type, Length, Data, ErrorSubCode) ->
    <<_3, ExtendedLength:1, _:4>> = <<Flags:8>>,
    case ExtendedLength of
        0 -> LengthLength = 8;
        1 -> LengthLength = 16
    end,
    AttributeData = <<Flags:8, Type:8, Length:LengthLength, Data/binary>>,
    throw({decode_error, ?BGP_ERROR_CODE_UPDATE_MESSAGE_ERROR, ErrorSubCode, AttributeData}).
    
%%----------------------------------------------------------------------------------------------------------------------

decode_notification(ConnectionFsmPid, Data) ->
    case Data of
        <<Code:8, SubCode:8, NotificationData/binary>> ->
            Notification = #bgp_notification{error_code=Code, error_sub_code=SubCode, data=NotificationData},
            bgp_connection_fsm:receive_notification(ConnectionFsmPid),      %% TODO: pass in notification
            Notification;
        _ ->
            throw({notification_decode_error})
    end.

%%----------------------------------------------------------------------------------------------------------------------

decode_keep_alive(ConnectionFsmPid, _Data) ->
    KeepAlive = #bgp_keep_alive{},
    bgp_connection_fsm:receive_keep_alive(ConnectionFsmPid),
    KeepAlive.

%%----------------------------------------------------------------------------------------------------------------------

decode_route_refresh(_ConnectionFsmPid, _Data) ->
    %% TODO: decode
    %% TODO: dispatch
    #bgp_route_refresh{}.

%%----------------------------------------------------------------------------------------------------------------------
