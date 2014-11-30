%% @author Jerry D'Antonio
%% @doc Interactive gemometry function.
%% @version 0.1

-module(ask_area).

-export([
  area/0
  ]).

-spec(area() -> number()).

area() ->
  Shape = char_to_shape(get_shape()),
  {Dimension1, Dimension2} = case Shape of
    rectangle -> get_dimensions("width", "height");
    triangle -> get_dimensions("base", "height");
    ellipse -> get_dimensions("major axis", "minor axis");
    _Else -> {0, 0}
  end,
  if
    Shape == unknown ->
      io:format("Unknown shape~n"),
      ok;
    true ->
      geom:area(Shape, Dimension1, Dimension2)
  end.

-spec(get_shape() -> char()).

get_shape() ->
  hd(io:get_line("R)ectangle, T)riangle, or E)llipse > ")).

-spec(char_to_shape(char()) -> atom()).

char_to_shape(Shape) ->
  case Shape of
    $R -> rectangle;
    $r -> rectangle;
    $T -> triangle;
    $t -> triangle;
    $E -> ellipse;
    $e -> ellipse;
    _Else -> unknown
  end.

-spec(get_number(string()) -> float()).

get_number(Prompt) ->
  Input = io:get_line("Enter " ++ Prompt ++ " > "),
  Value = string:strip(Input, right, $\n),
  case string:to_float(Value) of
    {error, no_float} ->
      {Number, _} = string:to_integer(Value);
    {Number, _} -> true
    end,
  float(Number).

-spec(get_dimensions(string(), string()) -> {number(), number()}).

get_dimensions(Prompt1, Prompt2) ->
  { get_number(Prompt1), get_number(Prompt2) }.
