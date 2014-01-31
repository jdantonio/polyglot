%% @author Jerry D'Antonio
%% @doc Recursive functions
%% @version 0.1

-module(powers).

-include("katas.hrl").

-export([
  raise/2,
  nth_root/2
  ]).

-spec(raise(number(),number()) -> number()).

raise(_, N) when N == 0 ->
  1;
raise(X, 1) ->
  X;
raise(X, N) when N > 0 ->
  X * raise(X, N - 1);
raise(X, N) when N < 0 ->
  1.0 / raise(X, -1 * N).

-spec(nth_root(number(),number()) -> number()).

nth_root(X, N) ->
  nth_root(X, N, X / 2.0).

nth_root(X, N, A) ->
  io:format("Current guess is ~p~n", [A]),
  F = (raise(A, N) - X),
  Fprime = N * raise(A, N - 1),
  Next = A - F / Fprime,
  Change = abs(Next - A),
  if
    Change < 1.0e-8 ->
      Next;
    true ->
      nth_root(X, N, Next)
  end.
