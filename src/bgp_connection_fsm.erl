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

%% TODO: deviations from RFC 4271:
%%
%% New action_initialize_connection_resources

-module(bgp_connection_fsm).
-author('Bruno Rijsman').

-behavior(gen_fsm).

-include("bgp_connection_fsm.hrl").
-include("bgp_messages.hrl").
-include("bgp_constants.hrl").

-export([]).

%% public API

-export([start_link/2,
         stop/1,                %% TODO: Figure out what the proper way is to shut down and FSM
         manual_start/1,
         manual_stop/1,
         automatic_start/1, 
         manual_start_with_passive_tcp_establishment/1, 
         automatic_start_with_passive_tcp_establishment/1, 
         automatic_start_with_damp_peer_oscillations/1, 
         automatic_start_with_damp_peer_oscillations_and_passive_tcp_establishment/1,
         automatic_stop/1,
         connect_retry_timer_expires/1,
         hold_timer_expires/1,
         keep_alive_timer_expires/1,
         delay_open_timer_expires/1,
         idle_hold_timer_expires/1,
         tcp_connection_valid/1,
         tcp_connect_request_invalid/1,
         tcp_connect_request_acked/2,
         tcp_connection_confirmed/2,
         tcp_connection_fails/1,
         receive_open/1,
         receive_open_with_delay_open_timer_running/1,
         receive_invalid_header/1,
         receive_invalid_open/1,
         open_collision_dump/1,
         receive_notification_with_version_error/1,
         receive_notification/1,
         receive_keep_alive/1,
         receive_update/2,
         receive_invalid_update/1]).

%% gen_fsm callbacks

-export([init/1, 
         state_idle/2, 
         state_connect/2,
         state_active/2, 
         state_open_sent/2, 
         state_open_confirm/2,
         state_established/2, 
         handle_event/3,
         handle_sync_event/4, 
         handle_info/3, 
         terminate/3, 
         code_change/4]).

%% TODO: here?
%% TODO: which timers to we need to jitter?

-define(BGP_CONNECT_RETRY_TIME, 10).     %% TODO: short time for testing; change to 120 seconds
-define(BGP_DELAY_OPEN_TIME, 10).        %% TODO: wat is the right value?
-define(BGP_HOLD_TIME_LARGE, 240).

%%----------------------------------------------------------------------------------------------------------------------

%% Public functions. 

%% TODO: add more parameters: the received packet, etc.

start_link(Direction, RemoteAddress) ->
    io:format("API: start_link Direction=~p RemoteAddress=~p~n", [Direction, RemoteAddress]),
    Name = list_to_atom(lists:flatten(io_lib:format("bgp_cnx_fsm_~p_out", [RemoteAddress]))),
    gen_fsm:start_link({local, Name}, ?MODULE, [Direction, RemoteAddress], [{debug, [trace, log, statistics]}]).

stop(Pid) ->
    io:format("API: stop~n"),
    gen_fsm:send_event(Pid, stop).

manual_start(Pid) -> 
    io:format("API: manual_start~n"),
    gen_fsm:send_event(Pid, event_manual_start).

manual_stop(Pid) -> 
    io:format("API: manual_top~n"),
    gen_fsm:send_event(Pid, event_manual_stop).

automatic_start(Pid) -> 
    io:format("API: automatic_start~n"),
    gen_fsm:send_event(Pid, event_automatic_start).

manual_start_with_passive_tcp_establishment(Pid) -> 
    io:format("API: manual_start_with_passive_tcp_establishment~n"),
    gen_fsm:send_event(Pid, event_manual_start_with_passive_tcp_establishment).

automatic_start_with_passive_tcp_establishment(Pid) -> 
    io:format("API: automatic_start_with_passive_tcp_establishment~n"),
    gen_fsm:send_event(Pid, event_automatic_start_with_passive_tcp_establishment).

automatic_start_with_damp_peer_oscillations(Pid) -> 
    io:format("API: automatic_start_with_damp_peer_oscillations~n"),
    gen_fsm:send_event(Pid, event_automatic_start_with_damp_peer_oscillations).

automatic_start_with_damp_peer_oscillations_and_passive_tcp_establishment(Pid) ->
    io:format("API: automatic_start_with_damp_peer_oscillations_and_passive_tcp_establishment~n"),
    gen_fsm:send_event(Pid, event_automatic_start_with_damp_peer_oscillations_and_passive_tcp_establishment).

automatic_stop(Pid) ->
    io:format("API: automatic_stop~n"),
    gen_fsm:send_event(Pid, event_automatic_stop).

connect_retry_timer_expires(Pid) ->
    io:format("API: connect_retry_timer_expires~n"),
    gen_fsm:send_event(Pid, event_connect_retry_timer_expires).

hold_timer_expires(Pid) ->
    io:format("API: hold_timer_expires~n"),
    gen_fsm:send_event(Pid, event_hold_timer_expires).

keep_alive_timer_expires(Pid) ->
    io:format("API: keep_alive_timer_expires~n"),
    gen_fsm:send_event(Pid, event_keep_alive_timer_expires).

delay_open_timer_expires(Pid) ->
    io:format("API: delay_open_timer_expires~n"),
    gen_fsm:send_event(Pid, event_delay_open_timer_expires).

idle_hold_timer_expires(Pid) ->
    io:format("API: idle_hold_timer_expires~n"),
    gen_fsm:send_event(Pid, event_idle_hold_timer_expires).

tcp_connection_valid(Pid) ->
    io:format("API: tcp_connection_valid~n"),
    gen_fsm:send_event(Pid, event_tcp_connection_valid).

tcp_connect_request_invalid(Pid) ->
    io:format("API: tcp_connect_request_invalid~n"),
    gen_fsm:send_event(Pid, event_tcp_connect_request_invalid).

tcp_connect_request_acked(Pid, Socket) ->
    io:format("API: tcp_connect_request_acked~n"),
    gen_fsm:send_event(Pid, {event_tcp_connect_request_acked, Socket}).

tcp_connection_confirmed(Pid, Socket) ->
    io:format("API: tcp_connection_confirmed~n"),
    gen_fsm:send_event(Pid, {event_tcp_connection_confirmed, Socket}).

tcp_connection_fails(Pid) ->
    io:format("API: tcp_connection_fails~n"),
    gen_fsm:send_event(Pid, event_tcp_connection_fails).

receive_open(Pid) ->
    io:format("API: receive_open~n"),
    gen_fsm:send_event(Pid, event_receive_open).

receive_open_with_delay_open_timer_running(Pid) ->
    io:format("API: receive_open_with_delay_open_timer_running~n"),
    gen_fsm:send_event(Pid, event_receive_open_with_delay_open_timer_running).

receive_invalid_header(Pid) ->
    io:format("API: receive_invalid_header~n"),
    gen_fsm:send_event(Pid, event_receive_invalid_header).

receive_invalid_open(Pid) ->
    io:format("API: receive_invalid_open~n"),
    gen_fsm:send_event(Pid, event_receive_invalid_open).

open_collision_dump(Pid) ->
    io:format("API: open_collision_dump~n"),
    gen_fsm:send_event(Pid, event_open_collision_dump).

receive_notification_with_version_error(Pid) ->
    io:format("API: receive_notification_with_version_error~n"),
    gen_fsm:send_event(Pid, event_receive_notification_with_version_error).

receive_notification(Pid) ->
    io:format("API: receive_notification~n"),
    gen_fsm:send_event(Pid, event_receive_notification).

receive_keep_alive(Pid) ->
    io:format("API: receive_keep_alive~n"),
    gen_fsm:send_event(Pid, event_receive_keep_alive).

receive_update(Pid, Update) ->
    io:format("API: receive_update~n"),
    gen_fsm:send_event(Pid, {event_receive_update, Update}).

receive_invalid_update(Pid) ->
    io:format("API: receive_invalid_update~n"),
    gen_fsm:send_event(Pid, event_receive_invalid_update).

%%----------------------------------------------------------------------------------------------------------------------
%% Mapping of event numbers in RFC 4271 to event names used in this code.
%%
%%  1 : manual_start
%%  2 : manual_stop
%%  3 : automatic_start
%%  4 : manual_start_with_passive_tcp_establishment
%%  5 : automatic_start_with_passive_tcp_establishment
%%  6 : automatic_start_with_damp_peer_oscillations
%%  7 : automatic_start_with_damp_peer_oscillations_and_passive_tcp_establishment
%%  8 : automatic_stop
%%  9 : connect_retry_timer_expires
%% 10 : hold_timer_expires
%% 11 : keep_alive_timer_expires
%% 12 : delay_open_timer_expires
%% 13 : idle_hold_timer_expires
%% 14 : tcp_connection_valid
%% 15 : tcp_connect_request_invalid
%% 16 : tcp_connect_request_acked
%% 17 : tcp_connection_confirmed
%% 18 : tcp_connection_fails
%% 19 : receive_open
%% 20 : receive_open_with_delay_open_timer_running    TODO: does this need to be a separate event?
%% 21 : receive_invalid_header
%% 22 : receive_invalid_open
%% 23 : open_collision_dump          TODO: better name for this
%% 24 : receive_notification_with_version_error
%% 25 : receive_notification
%% 26 : receive_keep_alive
%% 27 : receive_update
%% 28 : receive_invalid_update
%%
%% Additional events which are not mentioned in RFC 4271:
%%
%% TODO: add them here.

%% TODO: generate automatic start event, based on a 10 second timer, consider damping

%%----------------------------------------------------------------------------------------------------------------------

init([Direction, RemoteAddress]) ->
    io:format("Callback: init~n"),
    State = #connection_state{direction = Direction, remote_address = RemoteAddress},
    {ok, state_idle, State}.

%%----------------------------------------------------------------------------------------------------------------------

handle_event(Event, StateName, State) ->
    io:format("Callback: handle_event Event=~p StateName=~p State=~p~n", [Event, StateName, State]),
    {next_state, StateName, State}.

%%----------------------------------------------------------------------------------------------------------------------

handle_sync_event(Event, From, StateName, State) ->
    io:format("Callback: handle_sync_event Event=~p From=~p StateName=~p State=~p~n", [Event, From, StateName, State]),
    Reply = ok,
    {reply, Reply, StateName, State}.

%%----------------------------------------------------------------------------------------------------------------------

handle_info(Info, StateName, State) ->
    io:format("Callback: handle_info Info=~p StateName=~p State=~p~n", [Info, StateName, State]),
    {next_state, StateName, State}.

%%----------------------------------------------------------------------------------------------------------------------

terminate(Reason, StateName, State) ->
    io:format("Callback: terminate Reason=~p StateName=~p State=~p~n", [Reason, StateName, State]),
    ok.

%%----------------------------------------------------------------------------------------------------------------------

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%----------------------------------------------------------------------------------------------------------------------

%% TODO: Put these kinds of functions in a utility module.

cancel_timer(Timer) ->
    % Cancel a timer if it is running.
    case Timer of
        not_running ->
            not_running;
        _ ->
            {ok, cancel} = timer:cancel(Timer),
            not_running
    end.

%%----------------------------------------------------------------------------------------------------------------------

%% TODO: Add a flavor with jitter (up and/or down)

restart_timer(Timer, TimeInSeconds, Module, Function, Arguments) ->
    % Start a timer or restart it if it is already running.
    cancel_timer(Timer),
    {ok, NewTimer} = timer:apply_after(timer:seconds(TimeInSeconds), Module, Function, Arguments),
    NewTimer.

%%----------------------------------------------------------------------------------------------------------------------

restart_timer_if_not_zero(Timer, TimeInSeconds, Module, Function, Arguments) ->
    % Start a timer or restart it if it is already running.
    cancel_timer(Timer),
    if
        TimeInSeconds > 0 ->
            {ok, NewTimer} = timer:apply_after(timer:seconds(TimeInSeconds), Module, Function, Arguments);
        true ->
            NewTimer = not_running
    end,
    NewTimer.

%%----------------------------------------------------------------------------------------------------------------------

check_delay_open_attribute(_State) ->
    % TODO: the local system checks the DelayOpen attribute prior to processing.  
    false.

%%----------------------------------------------------------------------------------------------------------------------

check_delay_open_timer_is_running(_State) ->
    % TODO: the local system checks the DelayOpenTimer.  
    % TODO: If the DelayOpenTimer is running, the local system:
    false.

%%----------------------------------------------------------------------------------------------------------------------

action_initialize_resources(State) ->
    io:format("action_initialize_resources (TODO:)~n"),
    % TODO: The RFC says "initializes all BGP resources for the peer connection". I am not sure what it means. Maybe
    % setup the MD5 checksums etc.
    State.

%%----------------------------------------------------------------------------------------------------------------------

action_release_resources(State) ->
    io:format("action_release_resources (TODO:)~n"),
    % TODO: The RFC says "release all BGP resources". I am not sure what that means.
    State.

%%----------------------------------------------------------------------------------------------------------------------

action_initialize_connection_resources(State, Socket) ->
    io:format("action_initialize_connection_resources~n"),
    #connection_state{remote_address = RemoteAddress, direction = Direction} = State,
    Self = self(),
    {ok, SendSchedulerPid} = bgp_send_scheduler:start_link(RemoteAddress, Direction, Socket),
    {ok, ReceiverPid} = bgp_receive_scheduler:start_link(Self, RemoteAddress, Direction, Socket),
    State#connection_state{socket = Socket, 
                           send_scheduler_pid = SendSchedulerPid,
                           receiver_pid = ReceiverPid}.

%%----------------------------------------------------------------------------------------------------------------------

action_set_connect_retry_counter_to_zero(State) ->
    io:format("action_set_connect_retry_counter_to_zero~n"),
    State#connection_state{connect_retry_counter = 0}.

%%----------------------------------------------------------------------------------------------------------------------

action_increment_connect_retry_counter(State) ->
    io:format("action_increment_connect_retry_counter~n"),
    #connection_state{connect_retry_counter = ConnectRetryCounter} = State,
    State#connection_state{connect_retry_counter = ConnectRetryCounter + 1}.

%%----------------------------------------------------------------------------------------------------------------------

action_start_connect_retry_timer(State) ->
    io:format("action_start_connect_retry_timer~n"),
    #connection_state{connect_retry_timer = Timer} = State,
    NewTimer = restart_timer(Timer, ?BGP_CONNECT_RETRY_TIME, ?MODULE, connect_retry_timer_expires, [self()]),
    State#connection_state{connect_retry_timer = NewTimer}.

%%----------------------------------------------------------------------------------------------------------------------

action_stop_connect_retry_timer(State) ->
    io:format("action_stop_connect_retry_timer~n"),
    #connection_state{connect_retry_timer = Timer} = State,
    NewTimer = cancel_timer(Timer),
    State#connection_state{connect_retry_timer = NewTimer}.

%%----------------------------------------------------------------------------------------------------------------------

%% TODO: Do we ever start it with something else that the "initial value"?
%% TODO: Be consistent about calling actions start_... and restart_...

action_start_delay_open_timer_with_initial_value(State) ->
    io:format("action_start_delay_open_timer_with_initial_value~n"),
    #connection_state{delay_open_timer = Timer} = State,
    NewTimer = restart_timer(Timer, ?BGP_DELAY_OPEN_TIME, ?MODULE, delay_open_timer_expires, [self()]),
    State#connection_state{connect_retry_timer = NewTimer}.

%%----------------------------------------------------------------------------------------------------------------------

action_stop_delay_open_timer(State) ->
    io:format("action_stop_delay_open_timer~n"),
    #connection_state{delay_open_timer = Timer} = State,
    NewTimer = cancel_timer(Timer),
    State#connection_state{delay_open_timer = NewTimer}.

%%----------------------------------------------------------------------------------------------------------------------

action_start_keep_alive_timer(State) ->
    io:format("action_start_keep_alive_timer~n"),
    #connection_state{keep_alive_time = Time, keep_alive_timer = Timer} = State,
    NewTimer = restart_timer_if_not_zero(Timer, Time, ?MODULE, keep_alive_timer_expires, [self()]),
    State#connection_state{keep_alive_timer = NewTimer}.

%%----------------------------------------------------------------------------------------------------------------------

%% TODO: This function is not yet use; why is the keep-alive timer not stopped anywhere?

%action_stop_keep_alive_timer(State) ->
%    io:format("action_stop_keep_alive_timer~n"),
%    #connection_state{keep_alive_timer = Timer} = State,
%    NewTimer = cancel_timer(Timer),
%    State#connection_state{keep_alive_timer = NewTimer}.

%%----------------------------------------------------------------------------------------------------------------------

action_start_hold_timer_with_large_value(State) ->
    io:format("action_start_hold_timer_with_large_value (TODO:test)~n"),
    #connection_state{hold_timer = Timer} = State,
    NewTimer = restart_timer(Timer, ?BGP_HOLD_TIME_LARGE, ?MODULE, hold_timer_expires, [self()]),
    State#connection_state{hold_timer = NewTimer}.

%%----------------------------------------------------------------------------------------------------------------------

action_start_hold_timer(State) ->
    io:format("action_start_hold_timer~n"),
    #connection_state{hold_time = Time, hold_timer = Timer} = State,
    NewTimer = restart_timer_if_not_zero(Timer, Time, ?MODULE, hold_timer_expires, [self()]),
    State#connection_state{hold_timer = NewTimer}.

%%----------------------------------------------------------------------------------------------------------------------

%% TODO: This function is not yet used; why is the hold timer not stopped anywhere

%action_stop_hold_timer(State) ->
%    io:format("action_stop_hold_timer~n"),
%    #connection_state{hold_timer = Timer} = State,
%    NewTimer = cancel_timer(Timer),
%    State#connection_state{hold_timer = NewTimer}.

%%----------------------------------------------------------------------------------------------------------------------

action_initiate_tcp_connection(State) ->
    io:format("action_initiate_tcp_connection~n"),
    #connection_state{remote_address = RemoteAddress} = State,
    FsmPid = self(),
    ConnectPid = spawn_link(fun () -> connect(RemoteAddress, FsmPid) end),
    ConnectProcessName = list_to_atom(lists:flatten(io_lib:format("bgp_connect_~p", [RemoteAddress]))),
    register(ConnectProcessName, ConnectPid),
    State#connection_state{connect_pid = ConnectPid}.

%%----------------------------------------------------------------------------------------------------------------------

connect(RemoteAddress, FsmPid) ->
    % TODO: add {send_timeout, 100} ?
    Options = [binary, {packet, raw}, {active, false}],
    case gen_tcp:connect(RemoteAddress, ?BGP_TCP_LISTEN_PORT, Options) of
        {ok, Socket} ->
            gen_tcp:controlling_process(Socket, FsmPid),
            tcp_connect_request_acked(FsmPid, Socket);
        {error, Reason} ->
            %% TODO: log the error with the Reason
            %% TODO: store last error 
            io:format("Error outgoing connection: ~p~n", [Reason]),
            tcp_connection_fails(FsmPid)
    end.
    
%%----------------------------------------------------------------------------------------------------------------------

action_drop_tcp_connection(State) ->
    io:format("action_drop_tcp_connection~n"),
    % Stop the connect process, if any.
    #connection_state{connect_pid = ConnectPid} = State,
    case ConnectPid of
        none ->
            ok;
        _ ->
            exit(ConnectPid, normal)
    end,
    % Close the socket, if any.
    #connection_state{socket = Socket} = State,
    case Socket of
        none ->
            ok;
        _ ->
            gen_tcp:close(Socket)
    end,
    State#connection_state{connect_pid = none, socket = none}.

%%----------------------------------------------------------------------------------------------------------------------

action_start_listening_for_connection(State) ->
    io:format("action_start_listening_for_connection~n"),
    #connection_state{remote_address = RemoteAddress} = State,
    bgp_listener:register_acceptable_address(RemoteAddress),
    State.

%%----------------------------------------------------------------------------------------------------------------------

%% TODO: put in comment at top that we use this instead of the vague "continue to listen" action from the RFC

% TODO: This function is not yet used anywhere.

%action_stop_listening_for_connection(State) ->
%    io:format("action_stop_listening_for_connection"),
%    #connection_state{remote_address = RemoteAddress} = State,
%    bgp_listener:unregister_acceptable_address(RemoteAddress),
%    State.

%%----------------------------------------------------------------------------------------------------------------------

action_send_open(State) ->
    io:format("action_send_open~n"),
    #connection_state{send_scheduler_pid = SendSchedulerPid} = State,
    Open = #bgp_open{my_as=7675, identifier=100},       %%% TODO: get real parameters
    ok = bgp_send_scheduler:send_open(SendSchedulerPid, Open),
    State.

%%----------------------------------------------------------------------------------------------------------------------

action_send_keep_alive(State) ->
    io:format("action_send_keep_alive~n"),
    #connection_state{send_scheduler_pid = SendSchedulerPid} = State,
    ok = bgp_send_scheduler:send_keep_alive(SendSchedulerPid),
    State.

%%----------------------------------------------------------------------------------------------------------------------

action_send_cease_notification(State) ->
    io:format("action_send_cease_notification (TODO:)~n"),
    % TODO: sends a CEASE NOTIFICATION message to its peer,
    State.

%%----------------------------------------------------------------------------------------------------------------------

action_perform_peer_oscillation_damping(State) ->
    io:format("action_perform_peer_oscillation_damping (TODO:)~n"),
    % TODO: - performs peer oscillation damping if the DampPeerOscillations attribute is set to True
    State.

%%----------------------------------------------------------------------------------------------------------------------

action_delete_all_routes_associated_with_connection(State) ->
    io:format("action_delete_all_routes_associated_with_connection (TODO:)~n"),
    % TODO: deletes all routes associated with this connection,
    State.

%%----------------------------------------------------------------------------------------------------------------------

action_complete_bgp_initialization(State) ->
    io:format("action_complete_bgp_initialization (TODO:)~n"),
    % TODO: - completes BGP initialization. Not sure what that means, if anything.
    State.

%%----------------------------------------------------------------------------------------------------------------------

action_process_update(State, Update) ->
    #bgp_update{withdrawn_routes = WithdrawnRoutes, 
                attributes = Attributes, 
                advertised_routes = AdvertisedRoutes} = Update,
    RibPid = 1,     %% ###@@@
    Owner = 2,      %% ###@@@
    rtr_rib:remove_routes(RibPid, WithdrawnRoutes, Owner),
    rtr_rib:add_routes(RibPid, AdvertisedRoutes, Owner, Attributes),
    State.

%%----------------------------------------------------------------------------------------------------------------------

state_idle(Event, State)
  when (Event == event_manual_start) 
  or (Event == event_automatic_start) ->
    State1 = action_initialize_resources(State),
    State2 = action_set_connect_retry_counter_to_zero(State1),
    State3 = action_start_connect_retry_timer(State2),
    State4 = action_initiate_tcp_connection(State3),
    State5 = action_start_listening_for_connection(State4),
    {next_state, state_connect, State5};

%%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

state_idle(Event, State)
  when (Event == event_manual_stop) 
  or (Event == event_automatic_stop) ->
    % Ignore event.
    {next_state, state_idle, State};

%%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

state_idle(Event, State)
  when (Event == event_manual_start_with_passive_tcp_establishment) 
  or (Event == event_automatic_start_with_passive_tcp_establishment) ->
    State1 = action_initialize_resources(State),
    State2 = action_set_connect_retry_counter_to_zero(State1),
    State3 = action_start_connect_retry_timer(State2),
    State4 = action_start_listening_for_connection(State3),
    {next_state, state_active, State4};

%%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

% TODO:      If the DampPeerOscillations attribute is set to TRUE, the
% TODO:      following three additional events may occur within the Idle state:
% TODO:
% TODO:        - AutomaticStart_with_DampPeerOscillations (Event 6),
% TODO:
% TODO:        - AutomaticStart_with_DampPeerOscillations_and_
% TODO:          PassiveTcpEstablishment (Event 7),
% TODO:
% TODO:        - IdleHoldTimer_Expires (Event 13).
% TODO:
% TODO:      Upon receiving these 3 events, the local system will use these
% TODO:      events to prevent peer oscillations.  The method of preventing
% TODO:      persistent peer oscillation is outside the scope of this document.

%%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

state_idle(Event, State) ->
    io:format("Unhandled event ~p in state IDLE~n", [Event]),
    % TODO: unhandled event
    {next_state, state_idle, State}.

%%----------------------------------------------------------------------------------------------------------------------

state_connect(Event, State)
  when (Event == event_manual_stop) ->
    State1 = action_drop_tcp_connection(State),
    State2 = action_release_resources(State1),
    State3 = action_set_connect_retry_counter_to_zero(State2),
    State4 = action_stop_connect_retry_timer(State3),
    {next_state, state_idle, State4};

%%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

state_connect(Event, State) 
  when (Event == event_connect_retry_timer_expires) ->
    State1 = action_drop_tcp_connection(State),
    State2 = action_start_connect_retry_timer(State1),
    State3 = action_stop_delay_open_timer(State2),
    State4 = action_initiate_tcp_connection(State3),
    {next_state, state_connect, State4};

%%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

state_connect(Event, State) 
  when (Event == event_delay_open_timer_expires) ->
    State1 = action_send_open(State),
    State2 = action_start_hold_timer_with_large_value(State1),
    {next_state, state_open_sent, State2};

%%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

state_connect(Event, State) 
  when (Event == event_tcp_connection_valid) ->
    % TODO: the TCP connection is processed
    {next_state, state_open_sent, State};

%%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

state_connect(Event, State) 
  when (Event == event_tcp_connect_request_invalid) ->
    % TODO: the local system rejects the TCP connection
    {next_state, state_connect, State};

%%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

% TODO: can we support these two events as separate events?

state_connect({Event, Socket}, State) 
  when (Event == event_tcp_connect_request_acked)
  or (Event == event_tcp_connection_confirmed) ->
    State1 = action_initialize_connection_resources(State, Socket),
    State2 = action_stop_connect_retry_timer(State1),
    case check_delay_open_attribute(State1) of
        true ->
            State3 = action_start_delay_open_timer_with_initial_value(State2),
            {next_state, state_connect, State3};
        false ->
            State3 = action_complete_bgp_initialization(State2),
            State4 = action_send_open(State3),
            State5 = action_start_hold_timer_with_large_value(State4),
            {next_state, state_open_sent, State5}
    end;

%%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

state_connect(Event, State) 
  when (Event == event_tcp_connection_fails) ->
    case check_delay_open_timer_is_running(State) of
        true ->
            State1 = action_start_connect_retry_timer(State),    %% RFC TODO: says "restarts..."
            State2 = action_stop_delay_open_timer(State1),
            State3 = action_release_resources(State2),
            State4 = action_increment_connect_retry_counter(State3),
            State5 = action_perform_peer_oscillation_damping(State4),
            {next_state, state_idle, State5};
        false ->
            State1 = action_stop_connect_retry_timer(State),
            State2 = action_drop_tcp_connection(State1),
            State3 = action_release_resources(State2),
            {next_state, state_idle, State3}                    %% TODO: need some mechanism to retry autostart
    end;

%%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

state_connect(Event, State) 
  when (Event == event_receive_open_with_delay_open_timer_running) ->
    State1 = action_stop_connect_retry_timer(State),
    State2 = action_stop_delay_open_timer(State1),
    State3 = action_complete_bgp_initialization(State2),
    State4 = action_send_open(State3),
    State5 = action_send_keep_alive(State4),
    State6 = action_start_keep_alive_timer(State5),
    State7 = action_start_hold_timer(State6),
    % TODO: If the value of the autonomous system field is the same as the local Autonomous System number, set the 
    %       connection status to an internal connection; otherwise it will be "external".
    {next_state, state_open_confirm, State7};

%%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

state_connect(Event, State) 
  when (Event == event_bgp_header_error) 
  or (event_bgp_open_message_error) ->
    % TODO: (optionally) If the SendNOTIFICATIONwithoutOPEN attribute is set to TRUE, then the local system first sends
    % TODO: a NOTIFICATION message with the appropriate error code, and then
    State1 = action_stop_connect_retry_timer(State),
    State2 = action_release_resources(State1),
    State3 = action_drop_tcp_connection(State2),
    State4 = action_increment_connect_retry_counter(State3),
    % TODO: (optionally) performs peer oscillation damping if the DampPeerOscillations attribute is set to TRUE, and
    {next_state, state_idle, State4};

%%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

state_connect(Event, State) 
  when (Event == event_receive_notification_version_error) ->
    case check_delay_open_timer_is_running(State) of
        true ->
            State1 = action_stop_connect_retry_timer(State),
            State2 = action_stop_delay_open_timer(State1),
            State3 = action_release_resources(State2),
            State4 = action_drop_tcp_connection(State3),
            {next_state, state_idle, State4};
        false ->
            State1 = action_stop_connect_retry_timer(State),
            State2 = action_release_resources(State1),
            State3 = action_drop_tcp_connection(State2),
            State4 = action_increment_connect_retry_counter(State3),
            State5 = action_perform_peer_oscillation_damping(State4),
            {next_state, state_idle, State5}
    end;

%%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

state_connect(_AnyOtherEvent, State) ->
    State1 = action_stop_connect_retry_timer(State),
    State2 = action_stop_delay_open_timer(State1),
    State3 = action_release_resources(State2),
    State4 = action_drop_tcp_connection(State3),
    State5 = action_increment_connect_retry_counter(State4),
    State6 = action_perform_peer_oscillation_damping(State5),
    {next_state, state_idle, State6}.

%%----------------------------------------------------------------------------------------------------------------------

state_active(Event, State) 
  when (Event == event_manual_start)
  or (Event == event_automatic_start)
  or (Event == event_manual_start_with_passive_tcp_establishment)
  or (Event == event_automatic_start_with_passive_tcp_establishment)
  or (Event == event_automatic_start_with_damp_peer_oscilations)
  or (Event == event_automatic_start_with_damp_peer_oscilations_and_passive_tcp_establishment) ->
    % Ignore event.                                                                                                     
    {next_state, state_active, State};

%%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

state_active(Event, State) 
  when (Event == event_manual_stop) ->
    % TODO: If the DelayOpenTimer is running and the SendNOTIFICATIONwithoutOPEN session attribute is set, the
    State1 = action_send_cease_notification(State),
    State2 = action_release_resources(State1),
    State3 = action_stop_delay_open_timer(State2),
    State4 = action_drop_tcp_connection(State3),
    State5 = action_set_connect_retry_counter_to_zero(State4),
    State6 = action_stop_connect_retry_timer(State5),
    {next_state, state_idle, State6};

%%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

state_active(Event, State) 
  when (Event == event_connect_retry_timer_expires) ->
    State1 = action_start_connect_retry_timer(State),
    State2 = action_initiate_tcp_connection(State1),
    {next_state, state_connect, State2};

%%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

state_active(Event, State) 
  when (Event == event_delay_open_timer_expires) ->
    State1 = action_stop_connect_retry_timer(State),
    State2 = action_stop_delay_open_timer(State1),
    State3 = action_complete_bgp_initialization(State2),
    State4 = action_send_open(State3),
    State5 = action_start_hold_timer_with_large_value(State4),
    {next_state, state_open_sent, State5};

%%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

state_active(Event, State) 
  when (Event == event_tcp_connection_valid) ->
    % TODO: the local system processes the TCP connection flags
    {next_state, state_active, State};

%%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

state_active(Event, State) 
  when (Event == event_tcp_connect_request_invalid) ->
    % TODO: the local system rejects the TCP connection
    {next_state, state_active, State};

%%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

state_active({Event, _Socket}, State) 
  when (Event == event_tcp_connect_request_acked)
  or (Event == event_tcp_connection_confirmed) ->
    case check_delay_open_attribute(State) of
        true ->
            State1 = action_stop_connect_retry_timer(State),
            State2 = action_start_delay_open_timer_with_initial_value(State1),
            {next_state, state_active, State2};
        false ->
            State1 = action_stop_connect_retry_timer(State),
            State2 = action_complete_bgp_initialization(State1),
            State3 = action_send_open(State2),
            State4 = action_start_hold_timer_with_large_value(State3),
            {next_state, state_open_sent, State4}
    end;

%%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

state_active(Event, State) 
  when (Event == event_tcp_connection_fails) ->
    State1 = action_start_connect_retry_timer(State),
    State2 = action_stop_delay_open_timer(State1),
    State3 = action_release_resources(State2),
    State4 = action_increment_connect_retry_counter(State3),
    State5 = action_perform_peer_oscillation_damping(State4),
    {next_state, state_idle, State5};

%%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

state_active(Event, State) 
  when (Event == event_receive_open_with_delay_open_timer_running) ->
    State1 = action_stop_connect_retry_timer(State),
    State2 = action_stop_delay_open_timer(State1),
    State3 = action_complete_bgp_initialization(State2),
    State4 = action_send_open(State3),
    State5 = action_send_keep_alive(State4),
    State6 = action_start_keep_alive_timer(State5),
    State7 = action_start_hold_timer(State6),
    % TODO: If the value of the autonomous system field is the same as the local Autonomous System number, set the 
    %       connection status to an internal connection; otherwise it will be external.
    {next_state, state_open_confirm, State7};

%%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

state_active(Event, State) 
  when (Event == event_bgp_header_error) 
  or (Event == event_bgp_open_message_error) ->
    % TODO: (optionally) sends a NOTIFICATION message with the appropriate error code if the SendNOTIFICATIONwithoutOPEN
    % TODO:   attribute is set to TRUE,
    State1 = action_stop_connect_retry_timer(State),
    State2 = action_release_resources(State1),
    State3 = action_drop_tcp_connection(State2),
    State4 = action_increment_connect_retry_counter(State3),
    State5 = action_perform_peer_oscillation_damping(State4),
    {next_state, state_idle, State5};

%%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

state_active(Event, State) 
  when (Event == event_receive_notification_version_error) ->
    case check_delay_open_timer_is_running(State) of
        true ->
            State1 = action_stop_connect_retry_timer(State),
            State2 = action_stop_delay_open_timer(State1),
            State3 = action_release_resources(State2),
            State4 = action_drop_tcp_connection(State3),
            {next_state, state_idle, State4};
        false ->
            State1 = action_stop_connect_retry_timer(State),
            State2 = action_release_resources(State1),
            State3 = action_drop_tcp_connection(State2),
            State4 = action_increment_connect_retry_counter(State3),
            State5 = action_perform_peer_oscillation_damping(State4),
            {next_state, state_idle, State5}
    end;

%%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

state_active(_AnyOtherEvent, State)  ->
    State1 = action_stop_connect_retry_timer(State),
    State2 = action_release_resources(State1),
    State3 = action_drop_tcp_connection(State2),
    State4 = action_increment_connect_retry_counter(State3),
    State5 = action_perform_peer_oscillation_damping(State4),
    {next_state, state_idle, State5}.

%%----------------------------------------------------------------------------------------------------------------------

state_open_sent(Event, State) 
  when (Event == event_manual_start)
  or (Event == event_automatic_start)
  or (Event == event_manual_start_with_passive_tcp_establishment)
  or (Event == event_automatic_start_with_passive_tcp_establishment)
  or (Event == event_automatic_start_with_damp_peer_oscilations)
  or (Event == event_automatic_start_with_damp_peer_oscilations_and_passive_tcp_establishment) ->
    % Ignore event.                                                                                                     
    {next_state, state_open_sent, State};

%%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

state_open_sent(Event, State) 
  when (Event == event_manual_stop) ->
    State1 = action_send_cease_notification(State),
    State2 = action_stop_connect_retry_timer(State1),
    State3 = action_release_resources(State2),
    State4 = action_drop_tcp_connection(State3),
    State5 = action_increment_connect_retry_counter(State4),
    {next_state, state_idle, State5};

%%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

state_open_sent(Event, State) 
  when (Event == event_automatic_stop) ->
    State1 = action_send_cease_notification(State),
    State2 = action_stop_connect_retry_timer(State1),
    State3 = action_release_resources(State2),
    State4 = action_drop_tcp_connection(State3),
    State5 = action_increment_connect_retry_counter(State4),
    State6 = action_perform_peer_oscillation_damping(State5),
    {next_state, state_idle, State6};

%%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

state_open_sent(Event, State) 
  when (Event == event_hold_timer_expires) ->
    % TODO: sends a NOTIFICATION message with the error code Hold Timer Expired,
    State1 = action_stop_connect_retry_timer(State),
    State2 = action_release_resources(State1),
    State3 = action_drop_tcp_connection(State2),
    State4 = action_increment_connect_retry_counter(State3),
    State5 = action_perform_peer_oscillation_damping(State4),
    {next_state, state_idle, State5};

%%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

%% TODO: Check all event names for consistency

state_open_sent({Event, _Socket}, State) 
  when (Event == event_tcp_conection_valid)
  or (Event == event_tcp_connect_request_acked)
  or (Event == event_tcp_connection_confirmed) ->
    % TODO: a second TCP connection may be in progress.  This second TCP connection is tracked per Connection Collision 
    % TODO: processing (Section 6.8) until an OPEN message is received.
    {next_state, state_open_sent, State};
    
%%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

state_open_sent(Event, State) 
  when (Event == event_tcp_connect_request_invalid) ->
    % Ignore
    {next_state, state_open_sent, State};

%%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

state_open_sent(Event, State) 
  when (Event == event_tcp_connection_fails) ->
    % TODO: closes the BGP connection,
    State1 = action_stop_connect_retry_timer(State),
    {next_state, state_active, State1};

%%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

% TODO: Also do this for event_receive_open_with_delay_open_timer_running?

state_open_sent(Event, State) 
  when (Event == event_receive_open) ->
    State1 = action_stop_delay_open_timer(State),
    State2 = action_stop_connect_retry_timer(State1),
    State3 = action_send_keep_alive(State2),
    %% TODO: determine negotiated hold time and keep-alive time. Any other negotiated values?
    State4 = action_start_keep_alive_timer(State3),
    State5 = action_start_hold_timer(State4),
    {next_state, state_open_confirm, State5};

% TODO: If the value of the Autonomous
%       System field is the same as the local Autonomous System number,
%       then the connection is an "internal" connection; otherwise, it is
%       an "external" connection.  (This will impact UPDATE processing as
%       described below.)

% TODO: Collision detection mechanisms (Section 6.8) need to be applied
%       when a valid BGP OPEN message is received (Event 19 or Event 20).
%       Please refer to Section 6.8 for the details of the comparison.  A
%       CollisionDetectDump event occurs when the BGP implementation
%       determines, by means outside the scope of this document, that a
%       connection collision has occurred.

%%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

state_open_sent(Event, State) 
  when (Event == event_bgp_header_error) 
  or (Event == event_bgp_open_message_error) ->
    % TODO: sends a NOTIFICATION message with the appropriate error code,
    State1 = action_stop_connect_retry_timer(State),
    State2 = action_release_resources(State1),
    State3 = action_drop_tcp_connection(State2),
    State4 = action_increment_connect_retry_counter(State3),
    State5 = action_perform_peer_oscillation_damping(State4),
    {next_state, state_idle, State5};

%%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

state_open_sent(Event, State) 
  when (Event == event_open_collision_dump) -> 
    State1 = action_send_cease_notification(State),
    State2 = action_stop_connect_retry_timer(State1),
    State3 = action_release_resources(State2),
    State4 = action_drop_tcp_connection(State3),
    State5 = action_increment_connect_retry_counter(State4),
    State6 = action_perform_peer_oscillation_damping(State5),
    {next_state, state_idle, State6};

%%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

state_open_sent(Event, State) 
  when (Event == event_receive_notification_version_error) -> 
    State1 = action_stop_connect_retry_timer(State),
    State2 = action_release_resources(State1),
    State3 = action_drop_tcp_connection(State2),
    {next_state, state_idle, State3};

%%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

state_open_sent(_AnyOtherEvent, State) -> 
    % TODO: sends the NOTIFICATION with the Error Code Finite State Machine Error,
    State1 = action_stop_connect_retry_timer(State),
    State2 = action_release_resources(State1),
    State3 = action_drop_tcp_connection(State2),
    State4 = action_increment_connect_retry_counter(State3),
    State5 = action_perform_peer_oscillation_damping(State4),
    {next_state, state_idle, State5}.

%%----------------------------------------------------------------------------------------------------------------------

state_open_confirm(Event, State)
  when (Event == event_manual_start)
  or (Event == event_automatic_start)
  or (Event == event_manual_start_with_passive_tcp_establishment)
  or (Event == event_automatic_start_with_passive_tcp_establishment)
  or (Event == event_automatic_start_with_damp_peer_oscilations)
  or (Event == event_automatic_start_with_damp_peer_oscilations_and_passive_tcp_establishment) ->
    % Ignore event.                                                                                                     
    {next_state, state_open_confirm, State};

%%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

state_open_confirm(Event, State)
  when (Event == event_manual_stop) ->
    State1 = action_send_cease_notification(State),
    State2 = action_release_resources(State1),
    State3 = action_drop_tcp_connection(State2),
    State4 = action_increment_connect_retry_counter(State3),
    State5 = action_stop_connect_retry_timer(State4),
    {next_state, state_idle, State5};

%%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

state_open_confirm(Event, State)
  when (Event == event_automatic_stop) ->
    State1 = action_send_cease_notification(State),
    State2 = action_stop_connect_retry_timer(State1),
    State3 = action_release_resources(State2),
    State4 = action_drop_tcp_connection(State3),
    State5 = action_increment_connect_retry_counter(State4),
    State6 = action_perform_peer_oscillation_damping(State5),
    {next_state, state_idle, State6};

%%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

state_open_confirm(Event, State)
  when (Event == event_hold_timer_expires) ->
    % TODO: If the HoldTimer_Expires event (Event 10) occurs before a KEEPALIVE message is received, the local system:
    % TODO: note - is the above not implied by being in the open_confirm state?
    % TODO: sends the NOTIFICATION message with the Error Code Hold Timer Expired,
    State1 = action_stop_connect_retry_timer(State),
    State2 = action_release_resources(State1),
    State3 = action_drop_tcp_connection(State2),
    State4 = action_increment_connect_retry_counter(State3),
    State5 = action_perform_peer_oscillation_damping(State4),
    {next_state, state_idle, State5};

%%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

state_open_confirm(Event, State)
  when (Event == event_keep_alive_timer_expires) ->
    State1 = action_send_keep_alive(State),
    State2 = action_start_keep_alive_timer(State1),
    {next_state, state_open_confirm, State2};

%%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

% TODO: In the event of a TcpConnection_Valid event (Event 14), or the
% TODO: success of a TCP connection (Event 16 or Event 17) while in
% TODO: OpenConfirm, 

state_open_confirm(Event, State)
  when (Event == event_tcp_connection_valid) ->
    % TODO: the local system needs to track the second connection
    {next_state, state_open_confirm, State};
    
%%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

% TODO: Do we even need the following:
% TODO: If a TCP connection is attempted with an invalid port (Event 15),
% TODO: the local system will ignore the second connection attempt.

%%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

% TODO: Used the official event names from here on down

state_open_confirm(Event, State)
  when (Event == event_tcp_connection_fails) 
  or (Event == event_receive_notification) ->
    State1 = action_stop_connect_retry_timer(State),
    State2 = action_release_resources(State1),
    State3 = action_drop_tcp_connection(State2),
    State4 = action_increment_connect_retry_counter(State3),
    State5 = action_perform_peer_oscillation_damping(State4),
    {next_state, state_idle, State5};

%%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

state_open_confirm(Event, State)
  when (Event == event_receive_notification_with_version_error) -> 
    State1 = action_stop_connect_retry_timer(State),
    State2 = action_release_resources(State1),
    State3 = action_drop_tcp_connection(State2),
    {next_state, state_idle, State3};

%%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

state_open_confirm(Event, State)
  when (Event == event_receive_open) -> 
    % TODO: the collision detect function is processed per Section 6.8.
    % TODO: If this connection is to be dropped due to connection collision, the local system:
    State1 = action_send_cease_notification(State),
    State2 = action_stop_connect_retry_timer(State1),
    State3 = action_release_resources(State2),
    State4 = action_drop_tcp_connection(State3),
    State5 = action_increment_connect_retry_counter(State4),
    State6 = action_perform_peer_oscillation_damping(State5),
    {next_state, state_idle, State6};

%%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

state_open_confirm(Event, State)
  when (Event == event_receive_invalid_header)
  or (Event == event_receive_invalid_open) -> 
    % TODO: sends a NOTIFICATION message with the appropriate error code,
    State1 = action_stop_connect_retry_timer(State),
    State2 = action_release_resources(State1),
    State3 = action_drop_tcp_connection(State2),
    State4 = action_increment_connect_retry_counter(State3),
    State5 = action_perform_peer_oscillation_damping(State4),
    {next_state, state_idle, State5};

%%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

% TODO: If, during the processing of another OPEN message, the BGP
% TODO: implementation determines, by a means outside the scope of this
% TODO: document, that a connection collision has occurred and this
% TODO: connection is to be closed, the local system will issue an
% TODO: OpenCollisionDump event (Event 23).  

%%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

state_open_confirm(Event, State)
  when (Event == event_open_collision_dump) -> 
    State1 = action_send_cease_notification(State),
    State2 = action_stop_connect_retry_timer(State1),
    State3 = action_release_resources(State2),
    State4 = action_drop_tcp_connection(State3),
    State5 = action_increment_connect_retry_counter(State4),
    State6 = action_perform_peer_oscillation_damping(State5),
    {next_state, state_idle, State6};

%%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

state_open_confirm(Event, State)
  when (Event == event_receive_keep_alive) ->
    State1 = action_start_hold_timer(State),
    {next_state, state_established, State1};

%%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

state_open_confirm(_AnyOtherEvent, State) ->
    % TODO: sends a NOTIFICATION with a code of Finite State Machine Error,
    State1 = action_stop_connect_retry_timer(State),
    State2 = action_release_resources(State1),
    State3 = action_drop_tcp_connection(State2),
    State4 = action_increment_connect_retry_counter(State3),
    State5 = action_perform_peer_oscillation_damping(State4),
    {next_state, state_idle, State5}.

%%----------------------------------------------------------------------------------------------------------------------

state_established(Event, State)
  when (Event == event_manual_start)
  or (Event == event_automatic_start)
  or (Event == event_manual_start_with_passive_tcp_establishment)
  or (Event == event_automatic_start_with_passive_tcp_establishment)
  or (Event == event_automatic_start_with_damp_peer_oscilations)
  or (Event == event_automatic_start_with_damp_peer_oscilations_and_passive_tcp_establishment) ->
    % Ignore event.                                                                                                     
    {next_state, state_established, State};

%%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

state_established(Event, State)
  when (Event == event_manual_stop) ->
    State1 = action_send_cease_notification(State),
    State2 = action_stop_connect_retry_timer(State1),
    State3 = action_delete_all_routes_associated_with_connection(State2),
    State4 = action_release_resources(State3),
    State5 = action_drop_tcp_connection(State4),
    % TODO: sets the ConnectRetryCounter to zero, and
    {next_state, state_idle, State5};

%%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

state_established(Event, State)
  when (Event == event_automatic_stop) ->
    State1 = action_send_cease_notification(State),
    State2 = action_stop_connect_retry_timer(State1),
    State3 = action_delete_all_routes_associated_with_connection(State2),
    State4 = action_release_resources(State3),
    State5 = action_drop_tcp_connection(State4),
    State6 = action_increment_connect_retry_counter(State5),
    State7 = action_perform_peer_oscillation_damping(State6),
    {next_state, state_idle, State7};

% TODO: One reason for an AutomaticStop event is: A BGP receives an UPDATE
% TODO: messages with a number of prefixes for a given peer such that the
% TODO: total prefixes received exceeds the maximum number of prefixes
% TODO: configured.  The local system automatically disconnects the peer.

%%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

state_established(Event, State)
  when (Event == event_hold_timer_expires) ->
    % TODO: sends a NOTIFICATION message with the Error Code Hold Timer Expired,
    State1 = action_stop_connect_retry_timer(State),
    State2 = action_release_resources(State1),
    State3 = action_drop_tcp_connection(State2),
    State4 = action_increment_connect_retry_counter(State3),
    State5 = action_perform_peer_oscillation_damping(State4),
    {next_state, state_idle, State5};

%%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

state_established(Event, State)
  when (Event == event_keep_alive_timer_expires) ->
    State1 = action_send_keep_alive(State),
    State2 = action_start_keep_alive_timer(State1),
    {next_state, state_established, State2};

%%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

state_established(Event, State)
  when (Event == event_tcp_connection_valid) ->
    % TODO: cause the second connection to be tracked.
    {next_state, state_established, State};

%%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

state_established(Event, State)
  when (Event == event_tcp_connect_request_invalid) ->
    % Ignore.
    {next_state, state_established, State};

%%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

state_established({Event, _Socket}, State)
  when (Event == event_tcp_connect_request_acked)
  or (Event == event_tcp_connection_confirmed) ->
    % the second connection SHALL be tracked until it sends an OPEN message.
    {next_state, state_established, State};
      
%%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

% TODO: This does not seem right: if we receive an open when we are already in state established that should
% TODO: be a FSM error.
% TODO:
% TODO:      If a valid OPEN message (BGPOpen (Event 19)) is received, and if
% TODO:      the CollisionDetectEstablishedState optional attribute is TRUE,
% TODO:      the OPEN message will be checked to see if it collides (Section
% TODO:      6.8) with any other connection.  If the BGP implementation
% TODO:      determines that this connection needs to be terminated, it will
% TODO:      process an OpenCollisionDump event (Event 23).  If this connection
% TODO:      needs to be terminated, the local system:
% TODO:        - sends a NOTIFICATION with a Cease,
% TODO:        - sets the ConnectRetryTimer to zero,
% TODO:        - deletes all routes associated with this connection,
% TODO:        - releases all BGP resources,
% TODO:        - drops the TCP connection,
% TODO:        - increments the ConnectRetryCounter by 1,
% TODO:        - (optionally) performs peer oscillation damping if the
% TODO:          DampPeerOscillations is set to TRUE, and
% TODO:        - changes its state to Idle.

%%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

state_established(Event, State)
  when (Event == event_receive_notification_with_version_error)
  or (Event == event_receive_notification) 
  or (Event == event_tcp_connection_fails) ->
    State1 = action_stop_connect_retry_timer(State),
    State2 = action_delete_all_routes_associated_with_connection(State1),
    State3 = action_release_resources(State2),
    State4 = action_drop_tcp_connection(State3),
    State5 = action_increment_connect_retry_counter(State4),
    {next_state, state_idle, State5};

%%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

state_established(Event, State)
  when (Event == event_receive_keep_alive) ->
    State1 = action_start_hold_timer(State),
    {next_state, state_established, State1};

%%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

state_established({Event, Update}, State)
  when (Event == event_receive_update) ->
    State1 = action_process_update(State, Update),
    State2 = action_start_hold_timer(State1),
    {next_state, state_established, State2};

%%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

state_established(Event, State)
  when (Event == event_receive_invalid_update) ->
    % TODO: sends a NOTIFICATION message with an Update error,
    State1 = action_stop_connect_retry_timer(State),
    State2 = action_delete_all_routes_associated_with_connection(State1),
    State3 = action_release_resources(State2),
    State4 = action_drop_tcp_connection(State3),
    State5 = action_increment_connect_retry_counter(State4),
    State6 = action_perform_peer_oscillation_damping(State5),
    {next_state, state_idle, State6};

%%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

state_established(_AnyOtherEvent, State) ->
    % TODO: sends a NOTIFICATION message with the Error Code Finite State Machine Error,
    State1 = action_delete_all_routes_associated_with_connection(State),
    State2 = action_stop_connect_retry_timer(State1),
    State3 = action_release_resources(State2),
    State4 = action_drop_tcp_connection(State3),
    State5 = action_increment_connect_retry_counter(State4),
    State6 = action_perform_peer_oscillation_damping(State5),
    {next_state, state_idle, State6}.

%%----------------------------------------------------------------------------------------------------------------------
