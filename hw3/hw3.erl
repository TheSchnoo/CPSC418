-module(hw3).

% the exports required for the homework problems.
-export([primes/3, sum_inv_twin_primes/2, speedup/3, speedup_sum/3, time_sum/3, time_sum_par/3]).

% export some functions that I found handy to access while developing my solution.
-export([primes/1, primes/2, sum_inv_twin_primes/1, twin_primes/1]).

% primes(W, N, DstKey) -> ok
%   Use the workers of W to compute the primes in [2,N] and store them
%   in  list distributed over the workers for W.  DstKey is the key for
%   this distributed list.
primes(W, N, DstKey) ->

  % Parallel
  Args = misc:intervals(2, N, W),
  workers:update(W, DstKey,
    fun(_ProcState, {Low, High}) ->
      if (High == N) -> primes(Low, High);
        true -> primes(Low, High-1)
      end
    end,
    Args
  ).

primes_seq(W, N, DstKey) ->
  % Sequential
  workers:update(W, DstKey, misc:cut(primes(N), W)).

% sum_inv_twin_primes(W, SrcKey) -> Sum
%   Compute the sum of the reciprocals of the twin primes stored as a
%   distributed list for the workers of W.  SrcKey is the key associated
%   with this list of twin primes.
sum_inv_twin_primes(W, SrcKey) ->
  wtree:reduce(W, 
    fun(ProcState) -> leaf(ProcState, SrcKey) end,
    fun(Left, Right) -> combine(Left, Right) end,
    fun({_, _, Twins}) -> sum_inv_twin_primes(Twins) end)
  .

leaf(ProcState, SrcKey) ->
  Primes = workers:get(ProcState, SrcKey),
  if (Primes == []) -> ok;
    true ->
      {hd(Primes), lists:nth(length(Primes), Primes), twin_primes(Primes)} end
  .

combine({LHead, LLast, LPrimes}, ok) when is_list(LPrimes) -> {LHead, LLast, LPrimes};
combine(ok, {RHead, RLast, RPrimes}) when is_list(RPrimes) -> {RHead, RLast, RPrimes};
combine({LHead, LLast, LPrimes}, {RHead, RLast, RPrimes}) when is_list(LPrimes), is_list(RPrimes) ->
  if (LLast == RHead - 2) ->
    {LHead, RLast, lists:append([LPrimes, RPrimes, [LLast, RHead]])};
    true -> {LHead, RLast, lists:append(LPrimes, RPrimes)}
  end.

sum_inv_twin_primes_seq(W, SrcKey) ->
% Sequential
lists:sum([1/X || X <- twin_primes(lists:append(workers:retrieve(W, SrcKey)))]).
% 
time_sum_par(NWorkers, NData, NTrial) ->
  W = wtree:create(NWorkers),
  primes(W, NData, primes),
  [{mean, MeanPar}, {std, _}] = time_it:t( fun() -> sum_inv_twin_primes(W, primes) end, NTrial), % Timing the parallel version
  wtree:reap(W),
  MeanPar
  .

time_sum(NWorkers, NData, NTrial) ->
  W = wtree:create(NWorkers),
  primes(W, NData, primes2),
  [{mean, MeanSeq}, {std, _}] = time_it:t( fun() -> sum_inv_twin_primes_seq(W, primes2) end, NTrial), % Timing the sequential version
  wtree:reap(W),
  MeanSeq
  .

speedup_sum(NWorkers, NData, NTrial) -> 
  [{seq, MeanSeq, _StdSeq}, {par, MeanPar, _StdPar}] = time_sum(NWorkers, NData, NTrial),
  [{seq, MeanSeq}, {par, MeanPar}, {speedup, MeanSeq/MeanPar}].

% PLEASE NOTE: time/3 and speedup/3 functions based heavily on Tutorial 5 discussions
time(NWorkers, NData, NTrial) ->
  W = wtree:create(NWorkers),
  % workers:update(W, data, misc:cut(Data, W)),
  % workers:update(W, data2, misc:cut(Data, W)), % Split data
  % io:format("~w~n", [time_it:t(fun() -> primes_seq(W, NData, data) end, 10)]),
  [{mean, MeanSeq}, {std, StdSeq}] = time_it:t( fun() -> primes_seq(W, NData, data), wtree:barrier(W) end, NTrial), % Timing the sequential version
  [{mean, MeanPar}, {std, StdPar}] = time_it:t( fun() -> primes(W, NData, data2), wtree:barrier(W) end, NTrial), % Timing the parallel version
  wtree:reap(W),
  [{seq, MeanSeq, StdSeq}, {par, MeanPar, StdPar}]
  .

speedup(NWorkers, NData, NTrial) -> 
  [{seq, MeanSeq, _StdSeq}, {par, MeanPar, _StdPar}] = time(NWorkers, NData, NTrial),
  [{seq, MeanSeq}, {par, MeanPar}, {speedup, MeanSeq/MeanPar}].

sum_inv_twin_primes([]) -> 0;
sum_inv_twin_primes(N) when is_list(N) ->
  lists:sum([1/X || X <- N]);

sum_inv_twin_primes(N) when is_integer(N), 0 =< N ->
  lists:sum([1/TP || TP <- twin_primes(N)]).

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
