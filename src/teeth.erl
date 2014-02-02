%% @author Jerry D'Antonio
%% @doc Dentist helper functions.
%% @version 0.1

-module(teeth).

-export([
  alert/1,
  generate/2
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

-spec(generate(float(), float()) -> list()).

generate(Present, Bad) when is_float(Present), is_float(Bad) ->
  random:seed(now()),
  lists:map(fun(_) ->
        [P, B] = [random:uniform(), random:uniform()],
        if
          P =< Present -> [0];
          B =< Bad -> bad_tooth();
          true -> good_tooth()
        end
    end, lists:seq(1, 32)).

-spec(good_tooth() -> list()).
-spec(bad_tooth() -> list()).

good_tooth() -> lists:map(fun(_) -> random:uniform(3) end, lists:seq(1, 6)).
bad_tooth() ->
  Bad = random:uniform(6),
  lists:map(fun(Index) ->
        erlang:display([Bad, Index]),
        if
          Index =:= Bad -> 4;
          true -> random:uniform(3)
        end
    end, lists:seq(1, 6)).
