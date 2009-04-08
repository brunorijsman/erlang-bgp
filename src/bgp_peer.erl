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

-module(bgp_peer).
-author('Bruno Rijsman').

-behavior(gen_server).

-include("bgp_peer.hrl").
-include("rtr_constants.hrl").

%% public API

-export([start_link/1,
         stop/1,
         set_remote_as/2]).

%% gen_server callbacks

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%%----------------------------------------------------------------------------------------------------------------------

start_link(RemoteAddress) ->
    Name = list_to_atom(lists:flatten(io_lib:format("bgp_peer_~p", [RemoteAddress]))),
    gen_server:start_link({local, Name}, ?MODULE, [RemoteAddress], []).

%%----------------------------------------------------------------------------------------------------------------------

stop(PeerPid) ->
    gen_server:call(PeerPid, {stop}).

%%----------------------------------------------------------------------------------------------------------------------

set_remote_as(PeerPid, RemoteAs) 
  when is_number(RemoteAs) 
  or (RemoteAs == none) ->
    gen_server:call(PeerPid, {set_remote_as, RemoteAs}).

%%----------------------------------------------------------------------------------------------------------------------

init([RemoteAddress]) ->
    Ipv4RibPid = rtr_rib_registry:bind(?RTR_ROUTING_INSTANCE_CORE, ?RTR_AFI_IPV4, ?RTR_SAFI_UNICAST),
    State = #bgp_peer_state{remote_address = RemoteAddress, ipv4_rib_pid = Ipv4RibPid}, 
    {ok, State}.

%%----------------------------------------------------------------------------------------------------------------------

handle_call({stop}, _From, State) ->
    rtr_rib_registry:unbind(?RTR_ROUTING_INSTANCE_CORE, ?RTR_AFI_IPV4, ?RTR_SAFI_UNICAST),
    NewState = State#bgp_peer_state{ipv4_rib_pid = none}, 
    %% TODO: stop the connection(s)
    {stop, normal, stopped, NewState};

%%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

handle_call({set_remote_as, RemoteAs}, _From, State) ->
    ChangedState = State#bgp_peer_state{remote_as = RemoteAs},
    FinalState = handle_configuration_change(ChangedState, do_bounce),
    {reply, ok, FinalState}.

%%----------------------------------------------------------------------------------------------------------------------

handle_cast(_Message, State) ->
    {noreply, State}.

%%----------------------------------------------------------------------------------------------------------------------

handle_info(_Info, State) ->
    {noreply, State}.

%%----------------------------------------------------------------------------------------------------------------------

terminate(_Reason, _State) ->
    ok.

%%----------------------------------------------------------------------------------------------------------------------

code_change(_OldVersion, State, _Extra) ->
    {ok, State}.

%%----------------------------------------------------------------------------------------------------------------------

%% TODO: implement forced bouncing of session for "catastrophic" configuration changes.

handle_configuration_change(State, ForceBounceSession)
  when is_record(State, bgp_peer_state)
  and ((ForceBounceSession == do_bounce) or (ForceBounceSession == dont_bounce)) ->
    #bgp_peer_state{admin_state = AdminState, remote_as = RemoteAs} = State,
    {NewOperState, Reason} = case AdminState of
        disabled ->
            {down, "Administratively disabled"};
        enabled ->
            ConfigIsComplete = (RemoteAs /= none),
            case ConfigIsComplete of
                false ->
                    {down, "Incomplete configuration"};
                true ->
                    {up, ""}
            end
    end,
    NewState = case NewOperState of
        up ->
            bring_oper_up(State);
        down ->
            bring_oper_down(State, Reason)
    end,
    NewState.

%%----------------------------------------------------------------------------------------------------------------------

bring_oper_down(State, Reason)
  when is_record(State, bgp_peer_state) ->
    #bgp_peer_state{remote_address = RemoteAddress, 
                    oper_state = OperState, 
                    in_cnx_fsm_pid = InCnxFsmPid, 
                    out_cnx_fsm_pid = OutCnxFsmPid} = State,
    case OperState of
        down ->
            ok;
        up ->
            io:format("Operational state change: down (~p)~n", [Reason]),
            bgp_listener:unregister_acceptable_address(RemoteAddress),
            case InCnxFsmPid of
                none ->
                    ok;
                InPid ->
                    bgp_connection_fsm:stop(InPid)
            end,
            case OutCnxFsmPid of
                none ->
                    ok;
                OutPid ->
                    bgp_connection_fsm:stop(OutPid)
            end
                
    end,
    State#bgp_peer_state{oper_state = down, oper_state_reason = Reason, in_cnx_fsm_pid = none, out_cnx_fsm_pid = none}.
  
%%----------------------------------------------------------------------------------------------------------------------

bring_oper_up(State)
  when is_record(State, bgp_peer_state) ->
    #bgp_peer_state{remote_address = RemoteAddress, oper_state = OperState, out_cnx_fsm_pid = OutCnxFsmPid} = State,
    NewOutCnxFsmPid = case OperState of
        up ->
            OutCnxFsmPid;
        down ->
            io:format("Operational state change: up~n"),
            bgp_listener:register_acceptable_address(RemoteAddress),
            {ok, CnxPid} = bgp_connection_fsm:start_link(outgoing, RemoteAddress),
            ok = bgp_connection_fsm:manual_start(CnxPid),
            CnxPid
    end,
    State#bgp_peer_state{out_cnx_fsm_pid = NewOutCnxFsmPid, oper_state = up, oper_state_reason = ""}.
  
%%----------------------------------------------------------------------------------------------------------------------
