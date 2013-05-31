-module(urchin_util).

%% API
-export([for0/2,
        forN/3,
        for1_increment/3,
        forN_increment/4,
        for1_decrement/3,
        forN_decrement/4]).

%% ------------------------------------
%% API
%% ------------------------------------

for0(N, Fun) when
    is_integer(N),
    N >=0,
    is_function(Fun, 0) ->
    for0(N, Fun, []).

forN(N, Fun, Args) when
    is_integer(N),
    N >=0,
    is_function(Fun, 1) ->
    forN(N, Fun, Args, []).

for1_increment(I, J, Fun) when 
    is_integer(I),
    is_integer(J),
    J >= I,
    is_function(Fun, 1) ->
    for1_increment(I, J, Fun, []).

forN_increment(I, J, Fun, Args) when 
    is_integer(I),
    is_integer(J),
    J >= I,
    is_function(Fun, 2) ->
    forN_increment(I, J, Fun, Args, []).

for1_decrement(I, J, Fun) when
    is_integer(I), 
    is_integer(J), 
    I >= J, 
    is_function(Fun, 1) ->
    for1_decrement(I, J, Fun, []).

forN_decrement(I, J, Fun, Args) when
    is_integer(I), 
    is_integer(J), 
    I >= J, 
    is_function(Fun, 2) ->
    forN_decrement(I, J, Fun, Args, []).

%% ------------------------------------
%% Private
%% ------------------------------------

for0(N, Fun, ResList) when N > 0 ->
    for0(N - 1, Fun, [Fun()|ResList]);
for0(N, _, ResList) when N == 0 ->
    lists:reverse(ResList).

forN(N, Fun, Args, ResList) when N > 0 ->
    forN(N - 1, Fun, Args, [Fun(Args)|ResList]);
forN(N, _, _, ResList) when N == 0 ->
    lists:reverse(ResList).

for1_increment(I, J, Fun, ResList) when J >= I ->
    for1_increment(I + 1, J, Fun, [Fun(I)|ResList]);
for1_increment(I, J, _, ResList) when J < I->
    lists:reverse(ResList).

forN_increment(I, J, Fun, Args, ResList) when J >= I ->
    forN_increment(I + 1, J, Fun, Args, [Fun(I, Args)|ResList]);
forN_increment(I, J, _, _, ResList) when J < I->
    lists:reverse(ResList).

for1_decrement(I, J, Fun, ResList) when I >= J -> 
    for1_decrement(I - 1, J, Fun, [Fun(I)|ResList]);
for1_decrement(I, J, _, ResList) when I < J ->
    lists:reverse(ResList).

forN_decrement(I, J, Fun, Args, ResList) when I >= J ->
    forN_decrement(I - 1, J, Fun, Args, [Fun(I, Args)|ResList]);
forN_decrement(I, J, _, _, ResList) when I < J ->
    lists:reverse(ResList).
