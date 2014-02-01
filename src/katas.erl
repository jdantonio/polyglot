%% @author Jerry D'Antonio
%% @doc Erlang Katas.
%% @version 0.1

-module(katas).

-include("katas.hrl").

-export([
  delta/2,
  type_of/1
  ]).

-spec(delta(number(), number()) -> number()).

delta(unknown, unknown) -> 0;
delta(A, unknown) -> abs(A);
delta(unknown, B) -> abs(B);
delta(A, B) -> abs(A - B).

-spec(type_of(any()) -> atom()).

% http://fullof.bs/type-of-variable-type-detection-in-erlang/
% http://erlang.org/doc/man/erlang.html#is_atom-1
type_of(X) when is_integer(X)   -> integer;
type_of(X) when is_float(X)     -> float;
type_of(X) when is_list(X)      -> list;
type_of(X) when is_tuple(X)     -> tuple;
type_of(X) when is_binary(X)    -> binary;
type_of(X) when is_bitstring(X) -> bitstring;
type_of(X) when is_boolean(X)   -> boolean;
type_of(X) when is_function(X)  -> function;
type_of(X) when is_pid(X)       -> pid;
type_of(X) when is_port(X)      -> port;
type_of(X) when is_reference(X) -> reference;
type_of(X) when is_atom(X)      -> atom;
type_of(_)                      -> unknown.
