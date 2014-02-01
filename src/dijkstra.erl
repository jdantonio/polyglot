%% @author Jerry D'Antonio
%% @doc Edsger W. Dijkstra's greatest common divisor (GCD).
%% @version 0.1

-module(dijkstra).

-export([
  gcd/2
  ]).

%% @doc Edsger W. Dijkstra's greatest common divisor (GCD).
%% Returns the gcd.

-spec(gcd(number(),number()) -> number()).

gcd(M, N) when M == N ->
  M;
gcd(M, N) when M > N ->
  gcd(M - N, N);
gcd(M, N) ->
  gcd(M, N - M).
