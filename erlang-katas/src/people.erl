%% @author Jerry D'Antonio
%% @doc Module
%% @version 0.1

-module(people).

-export([
  male_and_over_forty/1,
  male_or_over_forty/1
  ]).

-spec(male_and_over_forty(list()) -> list()).

male_and_over_forty(People) when is_list(People) ->
  [Name || {Name, Sex, Age} <- People, Sex =:= $M, Age > 40].

-spec(male_or_over_forty(list()) -> list()).

male_or_over_forty(People) when is_list(People) ->
  [Name || {Name, Sex, Age} <- People, (Sex =:= $M) orelse (Age > 40)].
