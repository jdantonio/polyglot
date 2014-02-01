%% @author Jerry D'Antonio
%% @doc Statistics functions.
%% @version 0.1

-module(stats).

-export([
  minimum/1,
  maximum/1,
  range/1
  ]).

-define(min(X, Y), case X < Y of true -> X; false -> Y end).
-define(max(X, Y), case X > Y of true -> X; false -> Y end).

-spec(minimum(list()) -> number()).

minimum(List) when is_list(List) ->
  [Head | Tail] = List,
  minimum(Head, Tail).

-spec(minimum(number(), list()) -> number()).

minimum(Minimum, []) when is_number(Minimum) ->
  Minimum;
minimum(Minimum, List) when is_number(Minimum), is_list(List) ->
  [Head | Tail] = List,
  minimum(?min(Head, Minimum), Tail).

-spec(maximum(list()) -> number()).

maximum(List) when is_list(List) ->
  [Head | Tail] = List,
  maximum(Head, Tail).

-spec(maximum(number(), list()) -> number()).

maximum(Maximum, []) when is_number(Maximum) ->
  Maximum;
maximum(Maximum, List) when is_number(Maximum), is_list(List) ->
  [Head | Tail] = List,
  maximum(?max(Head, Maximum), Tail).

-spec(range(list()) -> list()).

range(List) when is_list(List) ->
  [Head | Tail] = List,
  range(Head, Head, Tail).

-spec(range(number(), number(), list()) -> list()).

range(Minimum, Maximum, []) when is_number(Minimum), is_number(Maximum) ->
  [Minimum, Maximum];
range(Minimum, Maximum, List) when is_number(Minimum), is_number(Maximum), is_list(List) ->
  [Head | Tail] = List,
  range(?min(Head, Minimum), ?max(Head, Maximum), Tail).
