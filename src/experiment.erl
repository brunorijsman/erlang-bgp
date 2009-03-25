-module(experiment).
-author('Bruno Rijsman').

-export([test/0]).

test() ->
    A = {<<3, 0>>, 9},
    B = {<<3, 128>>, 10},
    io:format("A < B = ~p~n", [A < B]),
    io:format("A == B = ~p~n", [A == B]),
    io:format("A > B = ~p~n", [A > B]).
       