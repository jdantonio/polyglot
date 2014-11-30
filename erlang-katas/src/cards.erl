%% @author Jerry D'Antonio
%% @doc Playing card functions.
%% @version 0.1

-module(cards).

-export([
  deck/0,
  shuffle/0
  ]).

-spec(deck() -> list()).

deck() ->
  Suits = ["Clubs", "Diamonds", "Hearts", "Spades"],
  Values = ["A"] ++ lists:seq(2, 10) ++ ["J", "Q", "K"],
  [{Value, Suit} || Value <- Values, Suit <- Suits].

-spec(shuffle() -> list()).
-spec(shuffle(list(), list()) -> list()).

shuffle() -> shuffle(deck(), []).
shuffle([], Acc) -> Acc;
shuffle(List, Acc) ->
  {Leading, [H | T]} = lists:split(random:uniform(length(List)) - 1, List),
  shuffle(Leading ++ T, [H | Acc]).
