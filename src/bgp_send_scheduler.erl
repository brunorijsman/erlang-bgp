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

-module(bgp_send_scheduler).
-author('Bruno Rijsman').

-behavior(gen_server).

-include("bgp_send_scheduler.hrl").
-include("bgp_messages.hrl").

%% public API

-export([start_link/3,
         stop/1,
         send_open/2,
         send_update/2,
         send_notification/2,
         send_keep_alive/1]).

%% gen_server callbacks

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%%----------------------------------------------------------------------------------------------------------------------

start_link(RemoteAddress, Direction, Socket) ->
    NameString = case Direction of
        incoming -> io_lib:format("bgp_cnx_txs_~p_in", [RemoteAddress]);
        outgoing -> io_lib:format("bgp_cnx_txs_~p_out", [RemoteAddress])
    end,
    Name = list_to_atom(lists:flatten(NameString)),
    gen_server:start_link({local, Name}, ?MODULE, [RemoteAddress, Direction, Socket], []).

%%----------------------------------------------------------------------------------------------------------------------

stop(SendSchedulerPid) ->
    gen_server:call(SendSchedulerPid, {stop}).

%%----------------------------------------------------------------------------------------------------------------------

send_open(SendSchedulerPid, Open) 
  when is_record(Open, bgp_open) ->
    gen_server:call(SendSchedulerPid, {send_open, Open}).

%%----------------------------------------------------------------------------------------------------------------------

send_update(SendSchedulerPid, Update) 
  when is_record(Update, bgp_update) ->
    gen_server:call(SendSchedulerPid, {send_update, Update}).

%%----------------------------------------------------------------------------------------------------------------------

send_notification(SendSchedulerPid, Notification) 
  when is_record(Notification, bgp_notification) ->
    gen_server:call(SendSchedulerPid, {send_notification, Notification}).

%%----------------------------------------------------------------------------------------------------------------------

send_keep_alive(SendSchedulerPid) -> 
    gen_server:call(SendSchedulerPid, send_keep_alive).

%%----------------------------------------------------------------------------------------------------------------------

send_completed(SendSchedulerPid) ->
    gen_server:call(SendSchedulerPid, send_completed).

%%----------------------------------------------------------------------------------------------------------------------

send_failed(SendSchedulerPid) ->
    gen_server:call(SendSchedulerPid, send_failed).

%%----------------------------------------------------------------------------------------------------------------------

init([RemoteAddress, Direction, Socket]) ->
    State = #bgp_send_scheduler_state{sender_pid = none, 
                                      remote_address = RemoteAddress, 
                                      direction = Direction, 
                                      socket = Socket},
    {ok, State}.

%%----------------------------------------------------------------------------------------------------------------------

handle_call({stop}, _From, State) ->
    #bgp_send_scheduler_state{sender_pid = SenderPid} = State,
    case SenderPid of
        none ->
            ok;
        _ ->
            exit(SenderPid, normal)
    end,
    {stop, normal, stopped, State};

%%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

handle_call({send_open, Open}, _From, State) ->
    EncodedMessage = bgp_messages:encode_open(Open),
    {ok, NewState} = schedule_send_message(State, EncodedMessage),
    {reply, ok, NewState};

%%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

handle_call({send_update, _Update}, _From, State) ->
    %% TODO:
    {reply, ok, State};

%%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

handle_call({send_notification, _Notification}, _From, State) ->
    %% TODO:
    {reply, ok, State};

%%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

handle_call(send_keep_alive, _From, State) ->
    EncodedMessage = bgp_messages:encode_keep_alive(),
    {ok, NewState} = schedule_send_message(State, EncodedMessage),
    {reply, ok, NewState};

%%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

handle_call(send_completed, _From, State) ->
    %% TODO: take the next message off the queue (if any, and send it)
    %% TODO: some way to avoid spawning lots of processes? have a call get_more_data?
    {reply, ok, State};

%%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

handle_call(send_failed, _From, State) ->
    %% TODO:
    {reply, ok, State}.

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

schedule_send_message(State, EncodedMessage) ->
    %% TODO: queue the message
    %% TODO: spawn a sender process if it is not already running
    %% TODO: but for now... just always spawn a new sender process, relying on the fact that the previous one is done
    #bgp_send_scheduler_state{socket = Socket} = State,
    SendSchedulerPid = self(),
    SenderPid = spawn_link(fun () -> sender(SendSchedulerPid, Socket, EncodedMessage) end),
     %% TODO: Include remote address and direction in process name
    register(bgp_cnx_tx, SenderPid),
    io:format("Sender PID is ~p~n", [SenderPid]),
    {ok, State}.

%%----------------------------------------------------------------------------------------------------------------------

%% rename this to send_loop after all?   

%% TODO: manage delay option; normally want it on, but not for notifications
%% TODO: manage time-out option; normally don't want it, but want a short one for notifications
%% TODO: it's starting to sound like we want a special send_final function?
%% TODO: have to take into account that we have to finish sending whatever message we are in the middle of first.

sender(SendSchedulerPid, Socket, Data) ->
    case gen_tcp:send(Socket, Data) of
        ok ->
            send_completed(SendSchedulerPid);     %% make this return {more_data, Data} or no_more_data. Loop in the first case.
        {error, Reason} ->
            %% TODO: log the error with the Reason
            io:format("Error sending on socket: ~p~n", [Reason]),
            send_failed(SendSchedulerPid)
    end.

%%----------------------------------------------------------------------------------------------------------------------
