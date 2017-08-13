-module(rohc_util).

-export([crc8/1,
         gen_rand_sn/1,
         
         sn_window_create/3,
         sn_window_add_ref_value/2,
         sn_window_get_min_bits/2]).

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

%%% -------------------------------------------------------------
%%% sn_window_create(Bits, WindowWidth, ShiftP) -> #wlsb{}
%%%
%%% create wlsb context
%%% -------------------------------------------------------------
sn_window_create(Bits, WindowWidth, ShiftP) ->
    rohc_util_wlsb:create(Bits, WindowWidth, ShiftP).

%%% -------------------------------------------------------------
%%% sn_window_add_ref_value(WLSBCxt, RefV) -> #wlsb{}
%%%
%%% add reference value into wlsb context
%%% -------------------------------------------------------------
sn_window_add_ref_value(WLSBCxt, RefV) ->
    rohc_util_wlsb:add_ref_value(WLSBCxt, RefV).

%%% -------------------------------------------------------------
%%% sn_window_get_min_bits(WLSBCxt, Value) -> integer()
%%%
%%% get num of LSBs of given value need to be sent
%%% -------------------------------------------------------------
sn_window_get_min_bits(WLSBCxt, Value) ->
    rohc_util_wlsb:get_num_of_bits(WLSBCxt, Value).
