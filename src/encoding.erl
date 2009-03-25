%% TODO: This code is not used at all anymore. I'm just keeping it around as an example for unit testing.

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
%% @doc The encoding module provides functions to manipulate encoded BGP messages or parts thereof. An encoding is a
%% list of bytes in reverse order: the first sent byte is at the end of the list so that appending bytes to the encoding
%% is efficient.
%%
%% TODO: also has a length
%% TODO: list is deep list of reversed lists of integers 
%% TODO: also allow binaries in deep list
%%
-module(encoding).
-author('Bruno Rijsman').
%


% TODO: to_binary
% TODO: from_binary
% TODO: to_string
% TODO: flatten
% TODO: compare two encodings for equality (need to flatten first?)
% TODO: concat_encodings (takes list of encodings) 

-export([empty/0,
         from_byte/1, 
         from_word/1,
         from_long/1,
         from_byte_list/1,
         from_word_list/1,
         from_long_list/1,
         append_byte/2,
         append_word/2,
         append_long/2,
         append_byte_list/2,
         append_word_list/2,
         append_long_list/2,
         append_encoding/2,
         size/1]). 

-include_lib("eunit/include/eunit.hrl").

%%----------------------------------------------------------------------------------------------------------------------
%% TODO: Make this a real type:
%% % @type encoding {length::integer(), [integer()]}

% TODO: Add a check for the Encoding parameter(s) to all of the when clauses below. Needs an is_encoding(). 

%%----------------------------------------------------------------------------------------------------------------------
%% @doc Creates an empty encoding.
%% @spec empty() -> encoding

empty() ->
    {0, []}.

empty_test() ->
    ?assertEqual({0, []}, empty()).

%%----------------------------------------------------------------------------------------------------------------------
%% @doc Creates an encoding from a byte.
%% @spec from_byte() -> encoding

from_byte(Byte) when is_integer(Byte) and (Byte >= 0) and (Byte =< 255)  ->
    {1, [Byte]}.

from_byte_test() ->
    ?assertEqual({1, [11]}, from_byte(11)).

%%----------------------------------------------------------------------------------------------------------------------
%% @doc Creates an encoding from a word (2 bytes).
%% @spec from_word() -> encoding

from_word(Word) when is_integer(Word) and (Word >= 0) and (Word =< 16#ffff)  ->
    <<Byte1:8, Byte2:8>> = <<Word:16>>,
    {2, [Byte2, Byte1]}.

from_word_test() ->
    ?assertEqual({2, [16#23, 16#01]}, from_word(16#0123)).

%%----------------------------------------------------------------------------------------------------------------------
%% @doc Creates an encoding from a long (4 bytes).
%% @spec from_long() -> encoding

from_long(Long) when is_integer(Long) and (Long >= 0) and (Long =< 16#ffffffff)  ->
    <<Byte1:8, Byte2:8, Byte3:8, Byte4:8>> = <<Long:32>>,
    {4, [Byte4, Byte3, Byte2, Byte1]}.

from_long_test() ->
    ?assertEqual({4, [16#67, 16#45, 16#23, 16#01]}, from_long(16#01234567)).

%%----------------------------------------------------------------------------------------------------------------------
%% @doc Creates an encoding from a list of bytes.
%% @spec from_byte_list() -> encoding

from_byte_list(ByteList) when is_list(ByteList) ->
    Size = length(ByteList),
    ReversedByteList = lists:reverse(ByteList),
    {Size, ReversedByteList}.

from_byte_list_test() ->
    ?assertEqual({3, [33, 22, 11]}, from_byte_list([11, 22, 33])).

%%----------------------------------------------------------------------------------------------------------------------
%% @doc Creates an encoding from a list of words (each word is 2 bytes).
%% TODO: spec

from_word_list(WordList) when is_list(WordList) ->
    append_word_list({0, []}, WordList).

%% TODO: test

%%----------------------------------------------------------------------------------------------------------------------
%% @doc Creates an encoding from a list of longs (each long is 4 bytes).
%% TODO: spec

from_long_list(LongList) when is_list(LongList) ->
    append_long_list({0, []}, LongList).

%% TODO: test

%%----------------------------------------------------------------------------------------------------------------------
%% @doc Append a byte to the encoding.
%% @spec append_byte(Encodig::encoding(), Byte::integer()) -> encoding

append_byte(Encoding, Byte) when (Byte >= 0) and (Byte =< 255) ->
    {Size, ReversedByteList} = Encoding,
    {Size + 1, [Byte | ReversedByteList]}.

append_byte_test() ->
    EmptyEncoding = empty(),
    OneByteEncoding = append_byte(EmptyEncoding, 11),
    ?assertEqual({1, [11]}, OneByteEncoding),
    TwoBytesEncoding = append_byte(OneByteEncoding, 22),
    ?assertEqual({2, [22, 11]}, TwoBytesEncoding).

% TODO: more test cases

%%----------------------------------------------------------------------------------------------------------------------
%% @doc Append a word (2 bytes) to the encoding.
%% @spec append_word(Encodig::encoding(), Word::integer()) -> encoding

% TODO: Add is_ clauses, but can we write an is_encoding which is allowed in a when clause?

append_word(Encoding, Word) when (Word >= 0) and (Word =< 16#ffff) ->
    {Size, ReversedByteList} = Encoding,
    <<Byte1:8, Byte2:8>> = <<Word:16>>,
    {Size + 2, [Byte2, Byte1 | ReversedByteList]}.

append_word_test() ->
    EmptyEncoding = empty(),
    OneWordEncoding = append_word(EmptyEncoding, 16#0123),
    ?assertEqual({2, [16#23, 16#01]}, OneWordEncoding),
    TwoWordsEncoding = append_word(OneWordEncoding, 16#4567),
    ?assertEqual({4, [16#67, 16#45, 16#23, 16#01]}, TwoWordsEncoding).

% TODO: more test cases

%%----------------------------------------------------------------------------------------------------------------------
%% @doc Append a long (4 bytes) to the encoding.
%% @spec append_long(Encodig::encoding(), Long::integer()) -> encoding

append_long(Encoding, Long) when (Long >= 0) and (Long =< 16#ffffffff) ->
    {Size, ReversedByteList} = Encoding,
    <<Byte1:8, Byte2:8, Byte3:8, Byte4:8>> = <<Long:32>>,
    {Size + 4, [Byte4, Byte3, Byte2, Byte1 | ReversedByteList]}.

append_long_test() ->
    EmptyEncoding = empty(),
    OneLongEncoding = append_long(EmptyEncoding, 16#01234567),
    ?assertEqual({4, [16#67, 16#45, 16#23, 16#01]}, OneLongEncoding),
    TwoLongsEncoding = append_long(OneLongEncoding, 16#89abcdef),
    ?assertEqual({8, [16#ef, 16#cd, 16#ab, 16#89, 16#67, 16#45, 16#23, 16#01]}, TwoLongsEncoding).

% TODO: more test cases

%%----------------------------------------------------------------------------------------------------------------------
%% @doc Append a byte list to the encoding.
%% TODO: spec

append_byte_list(Encoding, ByteList) when is_list(ByteList) ->
    append_encoding(Encoding, from_byte_list(ByteList)).

% TODO: test case

%%----------------------------------------------------------------------------------------------------------------------
%% @doc Append a word (2 byte) list to the encoding.
%% TODO: spec

append_word_list(Encoding, []) ->
    Encoding;

append_word_list(Encoding, [Word | Tail]) when (Word >= 0) and (Word =< 16#ffff) ->
    append_word_list(append_word(Encoding, Word), Tail).

% TODO: test case

%%----------------------------------------------------------------------------------------------------------------------
%% @doc Append a long (4 byte) list to the encoding.
%% TODO: spec

append_long_list(Encoding, []) ->
    Encoding;

append_long_list(Encoding, [Long | Tail]) when (Long >= 0) and (Long =< 16#ffffffff) ->
    append_long_list(append_long(Encoding, Long), Tail).

% TODO: test case

%%----------------------------------------------------------------------------------------------------------------------
%% @doc Append one encoding to another encoding.
%% @spec append_long(Encodig::encoding(), AppendedEncoding::encoding()) -> encoding()

append_encoding(Encoding, AppendedEncoding) ->
    {Size1, ReversedByteList1} = Encoding,
    {Size2, ReversedByteList2} = AppendedEncoding,
    % Postpone concatenating the lists; instead create a deep list in reverse order.
    {Size1 + Size2, [ReversedByteList2, ReversedByteList1]}.

append_encoding_test() ->
    Encoding1 = from_word(16#0123),
    Encoding2 = from_word(16#4567),
    CombinedEncoding1 = append_encoding(Encoding1, Encoding2),
    ?assertEqual({4, [[16#67, 16#45], [16#23, 16#01]]}, CombinedEncoding1),
    Encoding3 = from_word(16#89ab),
    CombinedEncoding2 = append_encoding(CombinedEncoding1, Encoding3),
    ?assertEqual({6, [[16#ab, 16#89], [[16#67, 16#45], [16#23, 16#01]]]}, CombinedEncoding2).

% TODO: more test cases

%%----------------------------------------------------------------------------------------------------------------------
%% @doc Determine the size in bytes of an encoding.
%% @spec size(Encodig::encoding()) -> integer()

size(Encoding) ->
    {Size, _} = Encoding,
    Size.

size_test() ->
    Encoding1 = empty(),
    ?assertEqual(0, encoding:size(Encoding1)),
    Encoding2 = from_byte(8#01),
    ?assertEqual(1, encoding:size(Encoding2)),
    Encoding3 = from_word(16#2345),
    ?assertEqual(2, encoding:size(Encoding3)),
    Encoding4 = append_encoding(Encoding1, Encoding2),
    ?assertEqual(1, encoding:size(Encoding4)),
    Encoding5 = append_encoding(Encoding4, Encoding3),
    ?assertEqual(3, encoding:size(Encoding5)).
