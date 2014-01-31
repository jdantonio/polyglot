%% @author Jerry D'Antonio
%% @doc Basic date parsing.
%% @version 0.1

-module(dates).

-export([
  date_parts/1
  ]).

-spec(date_parts(string()) -> list()).

date_parts(Date) ->
  Parts = re:split(Date,"-",[{return,list}]),
  lists:map(fun(Part) -> element(1, string:to_integer(Part)) end, Parts).
