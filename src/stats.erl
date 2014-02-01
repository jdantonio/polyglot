%% @author Jerry D'Antonio
%% @doc Statistics functions.
%% @version 0.1

-module(stats).

-export([
  minimum/1
  ]).

%-define(switch(Condition, True, False),
        %case Condition of true -> True; false -> False end).

-spec(minimum(list()) -> number()).

minimum(List) when is_list(List) ->
  [Head | Tail] = List,
  minimum(Head, Tail).

-spec(minimum(number(), list()) -> number()).

minimum(Minimum, []) when is_number(Minimum) ->
  Minimum;
minimum(Minimum, List) when is_number(Minimum), is_list(List) ->
  [Head | Tail] = List,
  %minimum(erlang:min(Head, Minimum), Tail).
  %NewMinimum = ?switch(Head < Minimum, Head, Minimum),
  NewMinimum = case Head < Minimum of true -> Head; false -> Minimum end,
  minimum(NewMinimum, Tail).
