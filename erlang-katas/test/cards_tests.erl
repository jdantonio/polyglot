-module(cards_tests).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

function_0_function_test() ->
  Deck = cards:deck(),
  {"deck/0 tests", [
      ?assertEqual(lists:nth(1, Deck),  {"A","Clubs"}),
      ?assertEqual(lists:nth(2, Deck),  {"A","Diamonds"}),
      ?assertEqual(lists:nth(3, Deck),  {"A","Hearts"}),
      ?assertEqual(lists:nth(4, Deck),  {"A","Spades"}),
      ?assertEqual(lists:nth(5, Deck),  {2,"Clubs"}),
      ?assertEqual(lists:nth(9, Deck),  {3,"Clubs"}),
      ?assertEqual(lists:nth(13, Deck), {4,"Clubs"}),
      ?assertEqual(lists:nth(17, Deck), {5,"Clubs"}),
      ?assertEqual(lists:nth(49, Deck), {"K","Clubs"}),
      ?assertEqual(lists:nth(50, Deck), {"K","Diamonds"}),
      ?assertEqual(lists:nth(51, Deck), {"K","Hearts"}),
      ?assertEqual(lists:nth(52, Deck), {"K","Spades"})
      ]}.

-endif.
