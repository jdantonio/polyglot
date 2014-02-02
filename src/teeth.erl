%% @author Jerry D'Antonio
%% @doc Dentist helper functions.
%% @version 0.1

-module(teeth).

-export([
  alert/1
  ]).

-spec(alert(list()) -> list()).

alert(Teeth) when is_list(Teeth), length(Teeth) =:= 32 ->
  check_teeth(Teeth, 1, []).

-spec(check_teeth(list(), integer(), list()) -> list()).

check_teeth([], _Index, Results) when is_list(Results), is_integer(_Index) ->
  lists:reverse(Results);
check_teeth(Teeth, Index, Results) when is_list(Teeth), is_integer(Index), is_list(Results) ->
  [Tooth | Remaining] = Teeth,
  case bad_tooth(Tooth) of
    true  -> check_teeth(Remaining, Index + 1, [Index] ++ Results);
    false -> check_teeth(Remaining, Index + 1, Results)
  end.

-spec(bad_tooth(list()) -> boolean()).

bad_tooth([0]) ->
  false;
bad_tooth(Depths) when is_list(Depths), length(Depths) =:= 6 ->
  lists:any(fun(X) -> if X >= 4 -> true; true -> false end end, Depths).
