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
%% @doc The as_path module provides functions for the manipulation of AS paths.
%%
-module(as_path).
-author('Bruno Rijsman').

-export([new_empty/0, 
         is_empty/1, 
         is_looped/1, 
         length/1, 
         encode/1, 
         decode/1, 
         to_string/1]).

-include_lib("eunit/include/eunit.hrl").

%% TODO: prepending
%% TODO: 4-byte as number manipulations
%% TODO: confederation manipulations
%% TODO: aggregation

%%----------------------------------------------------------------------------------------------------------------------
%% TODO: Make this a real type: list of AS path segments.
%% % @type asPath = integer().

%%----------------------------------------------------------------------------------------------------------------------
%% @doc Creates an empty AS path.
%% @spec new_empty() -> asPath

new_empty() ->
    [].

new_empty_test() ->
    ?assertEqual([], new_empty()).

%%----------------------------------------------------------------------------------------------------------------------
%% @doc Checks whether an AS path is empty.
%% @spec is_empty(asPath) -> bool()

is_empty([]) ->
    true;

is_empty(_) ->
    false.

is_empty_test() ->
    ?assert(is_empty(new_empty())).

%% TODO: test case for non-empty as path.

%%----------------------------------------------------------------------------------------------------------------------
%% @doc Checks whether an AS path contains a loop.
%% @spec is_looped(asPath) -> bool()

%% TODO: implement this
%% TODO: test case

is_looped(_) ->
    true.

%%----------------------------------------------------------------------------------------------------------------------
%% @doc Returns the length of an AS path.
%% @spec length(asPath) -> integer()

%% TODO: implement this
%% TODO: test case

length(_) ->
    1.

%%----------------------------------------------------------------------------------------------------------------------
%% @doc Encodes an AS path into a binary.
%% @spec encode(asPath) -> binary() 

%% TODO: implement this
%% TODO: test case

encode(_) ->
    <<1>>.

%%----------------------------------------------------------------------------------------------------------------------
%% @doc Decodes an AS path from a binary.
%% @spec decode(binary()) -> {ok, asPath} |
%%                           {error, ErrorCode, ErrorSubCode}

%% TODO: implement this
%% TODO: think more about how to return errors
%% TODO: test case

decode(_) ->
    {ok, []}.

%%----------------------------------------------------------------------------------------------------------------------
%% @doc Convert an AS path to a human readable string.
%% @spec to_string(asPath) -> string()

%% TODO: also have a from_string?

to_string([]) ->
    "";

to_string([Segment | Tail]) ->
    as_path_segment:to_string(Segment) ++ to_string(Tail).

%% TODO: test cases

%%----------------------------------------------------------------------------------------------------------------------
