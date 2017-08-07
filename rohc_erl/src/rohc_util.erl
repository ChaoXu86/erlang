-module(rohc_util).

-export([crc8/1,
         gen_rand_sn/1]).

crc8(_Binary) ->
    <<0>>.


%%% -------------------------------------------------------------
%%% gen_rand_sn(Seed) -> SN
%%%
%%% Seed        - integer()
%%% SN          - integer()
%%%
%%% generate initial sequence number
%%% -------------------------------------------------------------
%% gen_rand_sn() ->
%%      gen_rand_sn(erlang:system_time(milli_seconds)).
gen_rand_sn(_Seed) ->
    %% TODO seed ?
    rand:uniform(16#ffff).