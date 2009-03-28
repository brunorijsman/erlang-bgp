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

-module(bgp).
-author('Bruno Rijsman').

-behavior(gen_server).

-include("bgp.hrl").
-include("bgp_constants.hrl").

%% public API

-export([start_link/0,
         stop/0,
         add_peer/1,
         remove_peer/1]).

%% TODO: for testing
-export([test/0]).

%% gen_server callbacks

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%%----------------------------------------------------------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, bgp}, ?MODULE, [], []).

%%----------------------------------------------------------------------------------------------------------------------

stop() ->
    gen_server:call(bgp, {stop}).

%%----------------------------------------------------------------------------------------------------------------------

add_peer(RemoteAddress) ->
    gen_server:call(bgp, {add_peer, RemoteAddress}).

%%----------------------------------------------------------------------------------------------------------------------

remove_peer(RemoteAddress) ->
    gen_server:call(bgp, {remove_peer, RemoteAddress}).

%%----------------------------------------------------------------------------------------------------------------------

test() ->
    add_peer({192, 168, 1, 105}).

%%----------------------------------------------------------------------------------------------------------------------

init([]) ->
    PeerTable = ets:new(peers, []),
    bgp_listener:start_link(),
    State = #bgp_state{peer_table = PeerTable},
    {ok, State}.

%%----------------------------------------------------------------------------------------------------------------------

handle_call({stop}, _From, State) ->
    #bgp_state{peer_table = PeerTable} = State,
    ets:delete(PeerTable),
    NewState = #bgp_state{peer_table = none},
    {stop, normal, stopped, NewState};

%%----------------------------------------------------------------------------------------------------------------------

handle_call({add_peer, RemoteAddress}, _From, State) ->
    #bgp_state{peer_table = PeerTable} = State,
    %% TODO: Hide the guts of a peer (e.g. connection handling) in a separate bgp_peer module.
    {ok, ConnectionFsmPid} = bgp_connection_fsm:start_link(outgoing, {192, 168, 1, 105}),
    ok = bgp_connection_fsm:manual_start(ConnectionFsmPid),
    true = ets:insert(PeerTable, {RemoteAddress, void}),
    {reply, ok, State};

%%----------------------------------------------------------------------------------------------------------------------

handle_call({remove_peer, RemoteAddress}, _From, State) ->
    #bgp_state{peer_table = PeerTable} = State,
    %% TODO: stop the peer
    true = ets:delete(PeerTable, RemoteAddress),
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
