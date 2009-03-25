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

%%% TODO: Register all spawned processes using an easy to read name so that the process list is more meaningful.

%% @author Bruno Rijsman
%% @copyright 2009 Bruno Rijsman

-module(bgp).
-author('Bruno Rijsman').

-export([start/0,
         stop/0]).

-include("constants.hrl").

%%----------------------------------------------------------------------------------------------------------------------

start() ->
    io:format("[test] calling bgp_listener:start_link~n"),
    bgp_listener:start_link(),
    io:format("[test] calling bgp_connection_fsm:create~n"),
    {ok, Pid} = bgp_connection_fsm:create(outgoing, {192, 168, 1, 105}),
    io:format("[test] connection FSM Pid=~p~n", [Pid]),
    io:format("[test] calling bgp_connection_fsm:manual_start~n"),
    Result = bgp_connection_fsm:manual_start(Pid),
    io:format("[test] called manual_start, Result=~p~n", [Result]),
    ok.

%%----------------------------------------------------------------------------------------------------------------------

% TODO: need to stop connection FSMs.
    
stop() ->
    bgp_listener:stop(),
    ok.

%%----------------------------------------------------------------------------------------------------------------------
