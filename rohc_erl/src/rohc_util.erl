-module(rohc_util).

-define(CRC_8_Table, { %% The CRC-8 table       
                       16#00, 16#91, 16#e3, 16#72, 16#07, 16#96, 16#e4, 16#75,
                       16#0e, 16#9f, 16#ed, 16#7c, 16#09, 16#98, 16#ea, 16#7b,
                       16#1c, 16#8d, 16#ff, 16#6e, 16#1b, 16#8a, 16#f8, 16#69,
                       16#12, 16#83, 16#f1, 16#60, 16#15, 16#84, 16#f6, 16#67,
                       16#38, 16#a9, 16#db, 16#4a, 16#3f, 16#ae, 16#dc, 16#4d,
                       16#36, 16#a7, 16#d5, 16#44, 16#31, 16#a0, 16#d2, 16#43,
                       16#24, 16#b5, 16#c7, 16#56, 16#23, 16#b2, 16#c0, 16#51,
                       16#2a, 16#bb, 16#c9, 16#58, 16#2d, 16#bc, 16#ce, 16#5f,
                       16#70, 16#e1, 16#93, 16#02, 16#77, 16#e6, 16#94, 16#05,
                       16#7e, 16#ef, 16#9d, 16#0c, 16#79, 16#e8, 16#9a, 16#0b,
                       16#6c, 16#fd, 16#8f, 16#1e, 16#6b, 16#fa, 16#88, 16#19,
                       16#62, 16#f3, 16#81, 16#10, 16#65, 16#f4, 16#86, 16#17,
                       16#48, 16#d9, 16#ab, 16#3a, 16#4f, 16#de, 16#ac, 16#3d,
                       16#46, 16#d7, 16#a5, 16#34, 16#41, 16#d0, 16#a2, 16#33,
                       16#54, 16#c5, 16#b7, 16#26, 16#53, 16#c2, 16#b0, 16#21,
                       16#5a, 16#cb, 16#b9, 16#28, 16#5d, 16#cc, 16#be, 16#2f,
                       16#e0, 16#71, 16#03, 16#92, 16#e7, 16#76, 16#04, 16#95,
                       16#ee, 16#7f, 16#0d, 16#9c, 16#e9, 16#78, 16#0a, 16#9b,
                       16#fc, 16#6d, 16#1f, 16#8e, 16#fb, 16#6a, 16#18, 16#89,
                       16#f2, 16#63, 16#11, 16#80, 16#f5, 16#64, 16#16, 16#87,
                       16#d8, 16#49, 16#3b, 16#aa, 16#df, 16#4e, 16#3c, 16#ad,
                       16#d6, 16#47, 16#35, 16#a4, 16#d1, 16#40, 16#32, 16#a3,
                       16#c4, 16#55, 16#27, 16#b6, 16#c3, 16#52, 16#20, 16#b1,
                       16#ca, 16#5b, 16#29, 16#b8, 16#cd, 16#5c, 16#2e, 16#bf,
                       16#90, 16#01, 16#73, 16#e2, 16#97, 16#06, 16#74, 16#e5,
                       16#9e, 16#0f, 16#7d, 16#ec, 16#99, 16#08, 16#7a, 16#eb,
                       16#8c, 16#1d, 16#6f, 16#fe, 16#8b, 16#1a, 16#68, 16#f9,
                       16#82, 16#13, 16#61, 16#f0, 16#85, 16#14, 16#66, 16#f7,
                       16#a8, 16#39, 16#4b, 16#da, 16#af, 16#3e, 16#4c, 16#dd,
                       16#a6, 16#37, 16#45, 16#d4, 16#a1, 16#30, 16#42, 16#d3,
                       16#b4, 16#25, 16#57, 16#c6, 16#b3, 16#22, 16#50, 16#c1,
                       16#ba, 16#2b, 16#59, 16#c8, 16#bd, 16#2c, 16#5e, 16#cf}).

-export([crc8/1,
         gen_rand_sn/1,
         
         sn_window_create/3,
         sn_window_add_ref_value/2,
         sn_window_get_min_bits/2]).

crc8(Binary) ->
    crc_calc_8(Binary, byte_size(Binary), _Init_val = 2#11111111, ?CRC_8_Table).

crc_calc_8(_Header, 0, CRC, _CRCTable) ->
    <<CRC>>;
crc_calc_8(<<First,Next/binary>>, Length, CRC, CRCTable) ->
    N = First bxor CRC,                                     
    NewCRC = element(N+1,CRCTable),
    crc_calc_8(Next, Length-1, NewCRC, CRCTable).

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
