%% @author Jerry D'Antonio
%% @doc Module
%% @version 0.1

-module(calculus).

-export([
  derivative/2
  ]).

-spec(derivative(fun(), integer()) -> float()).

derivative(F, X) ->
  Delta = 1.0e-10,
  (F(X + Delta) - F(X)) / Delta.
