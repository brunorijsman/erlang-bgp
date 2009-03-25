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
%% @doc The as_path_segment module provides functions for the manipulation of AS path segments.
%%
-module(as_path_segment).
-author('Bruno Rijsman').

-export([new_as_sequence/1, 
         new_as_set/1, 
         new_as_confed_sequence/1, 
         new_as_confed_set/1,
         encode/3,
         to_string/1]).

-include("constants.hrl").
-include_lib("eunit/include/eunit.hrl").

%%----------------------------------------------------------------------------------------------------------------------
%% TODO: Make this a real type: list of AS path segments.
%% % @type asPathSegment = integer().

%%----------------------------------------------------------------------------------------------------------------------
%% @doc Creates an "AS sequence" AS path segment.
%% @spec new_as_sequence([AsNumber::integer()]) -> asPathSegment

new_as_sequence(AsNumberList) when is_list(AsNumberList) ->
    {as_sequence, AsNumberList}.

new_as_sequence_test() ->
    ?assertEqual({as_sequence, []}, new_as_sequence([])),
    ?assertEqual({as_sequence, [1]}, new_as_sequence([1])),
    ?assertEqual({as_sequence, [1, 2]}, new_as_sequence([1, 2])),
    ?assertError(function_clause, new_as_sequence(10)).

%%----------------------------------------------------------------------------------------------------------------------
%% @doc Creates an "AS set" AS path segment.
%% @spec new_as_set([AsNumber::integer()]) -> asPathSegment

new_as_set(AsNumberList) when is_list(AsNumberList) ->
    {as_set, AsNumberList}.

new_as_set_test() ->
    ?assertEqual({as_set, []}, new_as_set([])),
    ?assertEqual({as_set, [1]}, new_as_set([1])),
    ?assertEqual({as_set, [1, 2]}, new_as_set([1, 2])),
    ?assertError(function_clause, new_as_set(10)).

%%----------------------------------------------------------------------------------------------------------------------
%% @doc Creates an "AS confed sequence" AS path segment.
%% @spec new_as_confed_sequence([AsNumber::integer()]) -> asPathSegment

new_as_confed_sequence(AsNumberList) when is_list(AsNumberList) ->
    {as_confed_sequence, AsNumberList}.

new_as_confed_sequence_test() ->
    ?assertEqual({as_confed_sequence, []}, new_as_confed_sequence([])),
    ?assertEqual({as_confed_sequence, [1]}, new_as_confed_sequence([1])),
    ?assertEqual({as_confed_sequence, [1, 2]}, new_as_confed_sequence([1, 2])),
    ?assertError(function_clause, new_as_confed_sequence(10)).

%%----------------------------------------------------------------------------------------------------------------------
%% @doc Creates an "AS confed set" AS path segment.
%% @spec new_as_confed_set([AsNumber::integer()]) -> asPathSegment

new_as_confed_set(AsNumberList) when is_list(AsNumberList) ->
    {as_confed_set, AsNumberList}.

new_as_confed_set_test() ->
    ?assertEqual({as_confed_set, []}, new_as_confed_set([])),
    ?assertEqual({as_confed_set, [1]}, new_as_confed_set([1])),
    ?assertEqual({as_confed_set, [1, 2]}, new_as_confed_set([1, 2])),
    ?assertError(function_clause, new_as_confed_set(10)).

%%----------------------------------------------------------------------------------------------------------------------
%% @doc Encode an AS path segment.
%% @spec encode(AsPathSegment:asPathSegment(), AsNumberSize:integer(), Encoding:encoding()) -> encoding()

encode({Type, AsNumberList}, AsNumberSize, Encoding) when ((AsNumberSize == 2) or (AsNumberSize == 4)) ->
    case Type of
        as_sequence ->
            TypeByte = ?BGP_AS_PATH_SEGMENT_TYPE_AS_SEQUENCE;
        as_set ->
            TypeByte = ?BGP_AS_PATH_SEGMENT_TYPE_AS_SET;
        as_confed_sequence ->
            TypeByte = ?BGP_AS_PATH_SEGMENT_TYPE_AS_CONFED_SEQUENCE;
        as_confed_set ->
            TypeByte = ?BGP_AS_PATH_SEGMENT_TYPE_AS_CONFED_SET
    end,
    NewEncoding = encoding:append_byte_list(Encoding, [TypeByte, AsNumberSize * size(AsNumberList)]),
    case AsNumberSize of
        2 ->
            encoding:append_word_list(NewEncoding, AsNumberList);
        4 ->
            encoding:append_long_list(NewEncoding, AsNumberList)
    end.

%% TODO: test case; include error cases (wrong type, AS number too big)

%%----------------------------------------------------------------------------------------------------------------------
%% @doc Convert an AS path segment to a human readable string.
%% @spec to_string(asPathSegment) -> string()

to_string({as_sequence, AsNumberList}) ->
    as_number_list_to_string(AsNumberList);

to_string({as_set, AsNumberList}) ->
    "{" ++ as_number_list_to_string(AsNumberList) ++ "}";

to_string({as_confed_sequence, AsNumberList}) ->
    "(" ++ as_number_list_to_string(AsNumberList) ++ ")";

to_string({as_confed_set, AsNumberList}) ->
    "<" ++ as_number_list_to_string(AsNumberList) ++ ">".

to_string_test() ->
    ?assertEqual("", to_string(new_as_sequence([]))),
    ?assertEqual("10", to_string(new_as_sequence([10]))),
    ?assertEqual("10 20", to_string(new_as_sequence([10, 20]))),
    ?assertEqual("{}", to_string(new_as_set([]))),
    ?assertEqual("{10}", to_string(new_as_set([10]))),
    ?assertEqual("{10 20}", to_string(new_as_set([10, 20]))),
    ?assertEqual("()", to_string(new_as_confed_sequence([]))),
    ?assertEqual("(10)", to_string(new_as_confed_sequence([10]))),
    ?assertEqual("(10 20)", to_string(new_as_confed_sequence([10, 20]))),
    ?assertEqual("<>", to_string(new_as_confed_set([]))),
    ?assertEqual("<10>", to_string(new_as_confed_set([10]))),
    ?assertEqual("<10 20>", to_string(new_as_confed_set([10, 20]))).

%%----------------------------------------------------------------------------------------------------------------------
%% @doc Convert a list of AS numbers to a human readable string. The format is a list of space separated numbers.
%% @spec as_number_list_to_string([AsNumber::integer()]) -> string()
%% @private

as_number_list_to_string([]) ->
    "";

as_number_list_to_string([AsNumber]) ->
    integer_to_list(AsNumber);

as_number_list_to_string([AsNumber | Tail]) ->
    integer_to_list(AsNumber) ++ " " ++ as_number_list_to_string(Tail).

as_number_list_to_string_test() ->
    ?assertEqual("", as_number_list_to_string([])),
    ?assertEqual("10", as_number_list_to_string([10])),
    ?assertEqual("10 20", as_number_list_to_string([10, 20])).

