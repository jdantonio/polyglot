%% @author Jerry D'Antonio
%% @doc Basic date parsing.
%% @version 0.1

-module(dates).

-export([
  date_parts/1,
  is_leap_year/1,
  julian/1,
  julian/3
  ]).

-define(DAYS_IN_MONTH, [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]).

-spec(date_parts(string()) -> list()).

date_parts(Date) ->
  Parts = re:split(Date,"-",[{return,list}]),
  lists:map(fun(Part) -> element(1, string:to_integer(Part)) end, Parts).

-spec(is_leap_year(integer()) -> boolean()).

is_leap_year(Year) when is_integer(Year) ->
  (Year rem 4 == 0 andalso Year rem 100 /= 0)
  orelse (Year rem 400 == 0).

-spec(julian(string()) -> integer()).

julian(Date) when is_list(Date) ->
  [Year, Month, Day] = date_parts(Date),
  julian(Year, Month, Day).

-spec(julian(integer(), integer(), integer()) -> integer()).

julian(Year, Month, Day) when is_integer(Year), is_integer(Month), is_integer(Day) ->
  Accum = Day + case is_leap_year(Year) andalso Month > 2 of true -> 1; false -> 0 end,
  Months = element(1, lists:split(Month - 1, ?DAYS_IN_MONTH)),
  lists:foldl(fun(X, Sum) -> X + Sum end, Accum, Months).

%julian(Year, Month, Day) when is_integer(Year), is_integer(Month), is_integer(Day) ->
  %Accum = case is_leap_year(Year) andalso Month > 2 of true -> 1; false -> 0 end,
  %julian(Year, Month, Day, ?DAYS_IN_MONTH, Accum).

%-spec(julian(integer(), integer(), integer(), list(), integer()) -> integer()).

%julian(Year, 1, Day, DaysInMonth, Accum) when is_integer(Year), is_integer(Day), is_list(DaysInMonth), is_integer(Accum) ->
  %Day + Accum;
%julian(Year, Month, Day, DaysInMonth, Accum) when is_integer(Year), is_integer(Month), is_integer(Day), is_list(DaysInMonth), is_integer(Accum) ->
  %[Current | Remaining] = DaysInMonth,
  %julian(Year, Month - 1, Day, Remaining, Accum + Current).
