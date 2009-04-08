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

-module(rtr_rib).
-author('Bruno Rijsman').

-behavior(gen_server).

-include("rtr_rib.hrl").

%% TODO: Desired features of RIB (Routing Information Base) module:
%% - Support multiple tables (e.g. different AFI/SAFIs, tunnels, etc.)
%% - Provide APIs to query table (find-exact-match, find-best-match, find-next, etc.)
%% - Add support for next-hop interface and next-hop interface
%% - Allow clients to bind and unbind to a RIB.
%% - Create RIB on first bind; destroy on last unbind.
%% - Remove all routes from client on unbind.
%% - If client crashes, remove all its routes or mark them as stale (if client supports graceful restart)
%% - Best route selection for each prefix
%% - Allow clients to register for callback when best route for prefix changes
%% - Create a FIB module which tracks best routes and populates forwarding fast path
%% - Add support for ECMP (equal cost multi-path).
%% - Add support for NECMP (non-equal cost multi-path).
%% - Add support for optional next-hop resolution (separate module?)

%% public API

-export([start_link/3,
         stop/1,
         add_route/4,
         add_routes/4,
         remove_route/3,
         remove_routes/3]).

%% gen_server callbacks

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%%----------------------------------------------------------------------------------------------------------------------

start_link(RoutingInstance, Afi, Safi) ->
    %% TODO: Encode AFI/SAFI in registered name
    %% Name = list_to_atom(lists:flatten(io_lib:format("bgp_cnx_fsm_~p_out", [RemoteAddress]))),
    %% gen_fsm:start_link({local, Name}, ?MODULE, [Direction, RemoteAddress], [{debug, [trace, log, statistics]}]).
    gen_server:start_link({local, rtr_rib}, ?MODULE, [], [RoutingInstance, Afi, Safi]).

%%----------------------------------------------------------------------------------------------------------------------

stop(RibPid) ->
    gen_server:call(RibPid, {stop}).

%%----------------------------------------------------------------------------------------------------------------------

add_route(RibPid, Prefix, Owner, Attributes) ->
    gen_server:call(RibPid, {add_route, Prefix, Owner, Attributes}).

%%----------------------------------------------------------------------------------------------------------------------

add_routes(RibPid, PrefixList, Owner, Attributes) ->
    gen_server:call(RibPid, {add_routes, PrefixList, Owner, Attributes}).

%%----------------------------------------------------------------------------------------------------------------------

remove_route(RibPid, Prefix, Owner) ->
    gen_server:call(RibPid, {add_route, Prefix, Owner}).

%%----------------------------------------------------------------------------------------------------------------------

remove_routes(RibPid, PrefixList, Owner) ->
    gen_server:call(RibPid, {add_routes, PrefixList, Owner}).

%%----------------------------------------------------------------------------------------------------------------------

init([]) ->
    %% TODO: implement this
    State = #rtr_rib_state{},
    {ok, State}.

%%----------------------------------------------------------------------------------------------------------------------

handle_call({stop}, _From, State) ->
    %% TODO: implement this
    {stop, normal, stopped, State};

%%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

handle_call({add_route, _Prefix, _Owner, _Attributes}, _From, State) ->
    %% TODO: implement this
    io:format("ADD ROUTE~n"),       %% ###@@@
    {reply, ok, State};

%%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

handle_call({add_routes, _PrefixList, _Owner, _Attributes}, _From, State) ->
    %% TODO: implement this
    io:format("ADD ROUTES~n"),       %% ###@@@
    {reply, ok, State};

%%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

handle_call({remove_route, _Prefix, _Owner}, _From, State) ->
    %% TODO: implement this
    io:format("REMOVE ROUTE~n"),       %% ###@@@
    {reply, ok, State};

%%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

handle_call({remove_routes, _PrefixList, _Owner}, _From, State) ->
    %% TODO: implement this
    io:format("REMOVE ROUTES~n"),       %% ###@@@
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
