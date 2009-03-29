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

%% Controlling process issues: the process that accepted the socket should become the reader?

%% @author Bruno Rijsman
%% @copyright 2009 Bruno Rijsman

-module(bgp_listener).
-author('Bruno Rijsman').

-behavior(gen_server).

-include("bgp_listener.hrl").
-include("bgp_constants.hrl").

%% public API

-export([start_link/0,
         stop/0,
         register_acceptable_address/1,
         unregister_acceptable_address/1,
         show_acceptable_addresss/0,
         incoming_connection/1]).

%% gen_server callbacks

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%%----------------------------------------------------------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, bgp_listener}, ?MODULE, [], []).

%%----------------------------------------------------------------------------------------------------------------------

stop() ->
    gen_server:call(bgp_listener, {stop}).

%%----------------------------------------------------------------------------------------------------------------------

register_acceptable_address(RemoteAddress) ->
    gen_server:call(bgp_listener, {register_acceptable_address, RemoteAddress}).

%%----------------------------------------------------------------------------------------------------------------------

unregister_acceptable_address(RemoteAddress) ->
    gen_server:call(bgp_listener, {unregister_acceptable_address, RemoteAddress}).

%%----------------------------------------------------------------------------------------------------------------------

show_acceptable_addresss() ->
    gen_server:call(bgp_listener, {show_acceptable_addresss}).

%%----------------------------------------------------------------------------------------------------------------------

incoming_connection(Socket) ->
    gen_server:call(bgp_listener, {incoming_connection, Socket}).

%%----------------------------------------------------------------------------------------------------------------------

init([]) ->
    AcceptableAddressTable = ets:new(?MODULE, []),
    Options = [binary, {packet, raw}, {active, false}, {reuseaddr, true}],
    {ok, ListenSocket} = gen_tcp:listen(?BGP_TCP_LISTEN_PORT, Options),       %% TODO: handle errors (e.g. eaccess => must run as root)
    AcceptLoopPid = spawn_link(fun () -> accept_loop(ListenSocket) end),
    register(bgp_acceptor, AcceptLoopPid),
    State = #bgp_listener_state{accept_loop_pid = AcceptLoopPid,
                                listen_socket = ListenSocket, 
                                acceptable_address_table = AcceptableAddressTable},
    {ok, State}.

%%----------------------------------------------------------------------------------------------------------------------

handle_call({stop}, _From, State) ->
    #bgp_listener_state{accept_loop_pid = AcceptLoopPid, listen_socket = ListenSocket} = State,
    exit(AcceptLoopPid, normal),
    gen_tcp:close(ListenSocket),
    {stop, normal, stopped, State};

%%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

handle_call({register_acceptable_address, RemoteAddress}, _From, State) ->
    io:format("register_acceptable_address: ~p~n", [RemoteAddress]),
    #bgp_listener_state{acceptable_address_table = AcceptableAddressTable} = State,
    %% TODO: replace void
    ets:insert(AcceptableAddressTable, {RemoteAddress, void}),
    {reply, ok, State};

%%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

handle_call({unregister_acceptable_address, RemoteAddress}, _From, State) ->
    io:format("unregister_acceptable_address: ~p~n", [RemoteAddress]),
    #bgp_listener_state{acceptable_address_table = AcceptableAddressTable} = State,
    ets:delete(AcceptableAddressTable, RemoteAddress),
    {reply, ok, State};

%%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

handle_call({show_acceptable_addresss}, _From, State) ->
    io:format("show_acceptable_addresss~n"),
    #bgp_listener_state{acceptable_address_table = AcceptableAddressTable} = State,
    ets:foldl(fun(Address, Acc) -> io:format("~p~n", [Address]), Acc end, 0, AcceptableAddressTable),
    {reply, ok, State};

%%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

handle_call({incoming_connection, Socket}, _From, State) ->
    {ok, {Address, Port}} = inet:peername(Socket), 
    io:format("incoming_connection address=~p port=~p~n", [Address, Port]),
    #bgp_listener_state{acceptable_address_table = AcceptableAddressTable} = State,
    case ets:lookup(AcceptableAddressTable, Address) of
        [] ->
            io:format("ignored - address not registered~n"),
            gen_tcp:close(Socket);
        [{Address, void}] ->
            io:format("got connection from registered address ~p (TODO:)~n", [Address]),
            gen_tcp:close(Socket)
    end,
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

%%---------------------------------------------------------------------------------------------------`-------------------

accept_loop(ListenSocket) ->
    case gen_tcp:accept(ListenSocket) of
        {ok, Socket} ->
            incoming_connection(Socket),
            accept_loop(ListenSocket);
        {error, closed} ->
            io:format("Listening socket was closed~n");
        {error, Reason} ->
            %% TODO: log the error with the Reason
            io:format("Error accepting socket: ~p~n", [Reason]),
            accept_loop(ListenSocket)
    end.

%%----------------------------------------------------------------------------------------------------------------------
