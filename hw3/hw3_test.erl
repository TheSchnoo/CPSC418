% hw3_test: run some simple tests
%   WARNING: the test-code for Q2 executes the command:
%     gcc -std=c99 -O3 -o hw3
%   in your current directly.  In particular, it will overwrite the file hw3
%   if it already exists.

-module(hw3_test).
-include_lib("eunit/include/eunit.hrl").

% import the functions to test from the homework solution
-import(hw3, [primes/3, sum_inv_twin_primes/2]).

-export([check_text/2, check_text2/2]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                          %
% Test cases for primes.                                                   %
%   This uses eunit test fixtures.  See hw2_test.erl for more explanation. %
%                                                                          %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

primes_init() ->
  % I'll create 8 worker processes, that's a nice number for functional tests.
  % For speed-up measurements, we'll need other tests that adjust the number of
  % cores.
  wtree:create(8).

primes_test_cases(W) ->
  PrimesPar = fun(N) ->
    primes(W, N, primes),
    lists:append(workers:retrieve(W, primes))
  end,
  [    ?_assertEqual(primes(N), PrimesPar(N))
    || N <- [100, 5, 15, 1000000]
  ].

primes_test_() ->
  {setup, fun()  -> primes_init() end,        % set up
          fun(W) -> wtree:reap(W) end,        % clean up
          fun(W) -> primes_test_cases(W) end  % the test cases
  }.


% sequential implementation of primes -- for a reference
primes(Lo, Hi) when is_integer(Lo) and is_integer(Hi) and (Lo > Hi) -> [];
primes(Lo, Hi) when is_integer(Lo) and is_integer(Hi) and (Hi < 5) ->
  lists:filter(fun(E) -> (Lo =< E) and (E =< Hi) end, [2,3]);
primes(Lo, Hi) when is_integer(Lo) and is_integer(Hi) and (Lo =< Hi)  ->
  M = trunc(math:sqrt(Hi)),
  SmallPrimes = primes(2, M),
  BigPrimes = do_primes(SmallPrimes, max(Lo, M+1), Hi),
  if
    (Lo =< 2) -> SmallPrimes ++ BigPrimes;
    (Lo =< M) -> lists:filter(fun(E) -> E >= Lo end, SmallPrimes) ++ BigPrimes;
    true -> BigPrimes
  end.
primes(N) -> primes(1,N). % a simple default

% do_primes(SmallPrimes, Lo, Hi) ->  the elements of [Lo, ..., Hi]
%   that are not divisible} by any element of SmallPrimes.
do_primes(SmallPrimes, Lo, Hi) ->
  lists:foldl(fun(P, L) -> lists:filter(fun(E) -> (E rem P) /= 0 end, L) end,
	      lists:seq(Lo, Hi),
	      SmallPrimes).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                           %
% Test cases for sum_inv_twin_primes.                                       %
%   The definition in the homework statement only includes '5' once in the  %
%     list of twin primes.  It was pointed out on piazza that the summation %
%     for calculating Brun's constant should have the 1/5 term twice --     %
%     once for the twin-prime pair {3, 5} and again for {5, 7} we accept    %
%     either version.                                                       %
%  In the function names below, sitp is my shorthand for "sum of the        %
%     inverses of the twin primes.                                          %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(errTol, 1.0e-12).
close(X, Y) -> % true if X and Y are "close"
  abs(X-Y) =< ?errTol*lists:max([X, Y, 1]).
close(X1, X2, Y) -> % true if Y is "close" to either X1 or X2
  close(X1, Y) orelse close(X2, Y).
  
sitp_init() -> wtree:create(8).

sitp_test_cases(W) ->
  Check = fun(N) ->
    % These tests do not rely primes(W, N, Key).
    %   That way, even if primes(W, N, Key) fails its tests, we can still
    %   check sum_inv_twin_primes(W, Key).
    PN = primes(N),
    workers:update(W, primes, misc:cut(PN, W)),
    TestVal = sum_inv_twin_primes(W, primes),
    RefVal  = sum_inv_twin_primes(twin_primes(PN)),
    if N <  7  -> close(RefVal, TestVal);
       N >= 7  -> close(RefVal, RefVal+0.2, TestVal)
    end
  end,
  [?_assert(Check(N)) || N <- [100, 5, 15, 1000000]].

sitp_test_() ->
  {setup, fun() ->  sitp_init() end,        % set up
          fun(W) -> wtree:reap(W) end,      % clean up
          fun(W) -> sitp_test_cases(W) end  % the test cases
  }.

sum_inv_twin_primes([]) -> 0;
sum_inv_twin_primes(TP = [3, 5 | _]) -> % plausibly a list of twin primes
  lists:sum([1/X || X <- TP]);
sum_inv_twin_primes(N) when is_integer(N), 0 =< N ->
  sum_inv_twin_primes(twin_primes(N)).


% twin_primes(N) return all twin primes where the larger twin is at most N.
twin_primes(N) when is_integer(N) -> twin_primes(primes(N));
twin_primes([]) -> [];
twin_primes(Primes=[_ | P_Tail]) ->
  %   It's easiest just to find all such pairs and concatenate them, but this
  %   duplicates 5 because {3,5} and {5,7} are both twin prime pairs.  This
  %   is the only such pair because for any P, one of P, P+2, or P+4 must be
  %   divisible by 3.  So, I wrote a case that fixes the [3,5,5,7 | _] sequence.
  TP_pairs  = [[P1, P2] || {P1, P2} <- lists:zip(Primes, P_Tail++[0]), P2 == P1+2],
  case lists:append(TP_pairs) of
    [3, 5, 5 | TP_Tail] -> [3, 5 | TP_Tail];
    TwinPrimes ->  TwinPrimes
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                           %
% Test cases for Q2							    %
%                                                                           %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% execute CommandString in the unix shell
cmd(CommandString) -> % {Status, OutputText}
  io:format("cmd(\"~s\")~n", [CommandString]),
  UnixProc = erlang:open_port({spawn, CommandString}, [exit_status]),
  fun GrabOutput(Buf) ->
    receive
      {UnixProc, {data, SomeText}} -> GrabOutput([Buf , SomeText]);
      {UnixProc, {exit_status, Status}} -> {Status, lists:flatten(Buf)}
    end
  end([]).

check_text2([], []) -> true;
check_text2([C | TextTail], Pattern) when C =:= $\s; C =:= $\t; C =:= $\n ->
  check_text2(TextTail, Pattern);
check_text2([H | TextTail], [digits | PatternTail])
    when ($0 =< H) andalso (H =< $9) ->
  check_text2(
    lists:dropwhile(fun(C) -> ($0 =< C) andalso (C =< $9) end, TextTail),
    PatternTail);
check_text2([H | TextTail], [sign | PatternTail])
    when (H =:= $+) orelse (H =:= $-) ->
  check_text2(TextTail, PatternTail);
check_text2(Text, [String | PatternTail]) when is_list(String) ->
  lists:prefix(String, Text) andalso
  check_text2(lists:nthtail(length(String), Text), PatternTail);
check_text2(_, _) -> false.


check_text(Text, Pattern) ->
  case check_text2(Text, Pattern) of
    true -> true;
    false -> {format_error, Text}
  end.

q2_init() ->
  case cmd("gcc -std=c99 -O3 hw3.c -o hw3") of
    {0, []} -> ok;
    X -> X
  end.

q2_test_cases(ok) ->
  Check = fun(Args1, Args2) ->
    case cmd("./hw3 " ++ Args1 ++ " " ++ Args2) of
      {0, OutputText} ->
        check_text(OutputText,
	  [ Args1,
	    "n=",       digits,
	    "n_trial=", digits,
	    "t_avg=",   digits, ".", digits, "e", sign, digits
	  ]);
      X -> X
    end
  end,
  [ ?_assert(Check("random array", "")),
    ?_assert(Check("ascending list", "100")),
    ?_assert(Check("random array", "123 42"))
  ];
q2_test_cases({Status, OutputText}) ->
  {compile_errors_or_warnings, {exit_status, Status}, OutputText}.

q2_test_() ->
  {setup, fun() ->  q2_init() end,        % set up
          fun(_) -> ok end,      	  % nothing to clean-up
          fun(X) -> q2_test_cases(X) end  % the test cases
  }.
