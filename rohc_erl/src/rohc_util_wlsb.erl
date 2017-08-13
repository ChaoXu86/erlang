-module(rohc_util_wlsb).

-export([create/3,
         add_ref_value/2,
         get_num_of_bits/2
        ]).

-record(wlsb,
        {   
         bits,               %% maximum bits of repeseting value 
         window       = [],  %% refs window, queue in list form,
                             %% new refv => [... refv3, refv2, refv1]
         window_width,       %% number of entries in window
         lsb_shift_p         %% RFC 3095 4.5.2
        
        }
       ).

%%% -------------------------------------------------------------
%%% create(Bits, WindowWidth, ShiftP) -> #wlsb{}
%%%
%%% create wlsb context
%%% -------------------------------------------------------------
create(Bits, WindowWidth, ShiftP) ->
    #wlsb{bits         = Bits,
          window_width = WindowWidth,
          lsb_shift_p  = ShiftP}.

%%% -------------------------------------------------------------
%%% add_ref_value(WLSBCxt, RefV) -> #wlsb{}
%%%
%%% add reference value into wlsb context
%%% -------------------------------------------------------------
add_ref_value(WLSBCxt, RefV) ->
    #wlsb{window       = Window,
          window_width = WindowWidth} = WLSBCxt,
    
    NewWindow = 
        case length(Window) of
            WindowWidth ->
                %% Window is full, drop the oldest one
                [RefV|lists:droplast(Window)];
            _ ->
                [RefV|Window]
        end,
    WLSBCxt#wlsb{window       = NewWindow,
                 window_width = WindowWidth}.

%%% -------------------------------------------------------------
%%% get_num_of_bits(WLSBCxt, Value) -> integer()
%%%
%%% get num of LSBs of given value need to be sent
%%% e.g. There four reference numbers in wlsb window, 8, 9, 11 and 13.
%%%      If 12 is to be sent, 3 is returned
%%%           RefV           Value    diff bits
%%%       8 - 1000       12 - 1100            3 <- max diff 
%%%       9 - 1001       12 - 1100            3
%%%      11 - 1011       12 - 1100            3
%%%      13 - 1101       12 - 1100            1
%%% -------------------------------------------------------------
get_num_of_bits(WLSBCxt, Value) ->
    #wlsb{window       = Window,
          bits         = MaxBits} = WLSBCxt,
    
    %% check if value is within range
    %% TODO maybe we should not check ?
    case ((1 bsl MaxBits ) - 1) < Value of
        true ->
            throw(value_out_of_range);
        _ ->
            do_nothing
    end,
    
    case Window of
        [] ->
            %% Ref window is empty, use maximum number of bits
            MaxBits;
        _ ->
            get_num_of_bits_int(Window, Value, 0, MaxBits)
    end.


get_num_of_bits_int([], _Value, NumOfBits, _MaxBits) ->
    NumOfBits;
get_num_of_bits_int([RefV|Rest], Value, NumOfBits, MaxBits) ->
    NewNumOfBits = get_num_of_diff_LSB(RefV, Value, MaxBits),
    case NewNumOfBits > NumOfBits of
        true ->
            get_num_of_bits_int(Rest, Value, NewNumOfBits, MaxBits);
        _ ->
            get_num_of_bits_int(Rest, Value, NumOfBits, MaxBits)
    end.

%%% -------------------------------------------------------------
%%% get_num_of_diff_LSB(Num1, Num2, MaxBits)  -> integer()
%%%
%%% get num of different LSBs of given two values
%%% Following example shows the algorithm
%%% e.g.     V1        V2     Mask  V1&Mask == V2&Mask?  diffBits   
%%%      2#1000    2#1010   2#1111       false                  0                
%%%      2#1000    2#1010   2#1110       false                  1
%%%      2#1000    2#1010   2#1100        true                  2
%%% Then diffBits 2 is returned.
%%% -------------------------------------------------------------
get_num_of_diff_LSB(Num1, Num2, MaxBits) ->
    get_num_of_diff_LSB_int(Num1, Num2, (1 bsl MaxBits) - 1, 0).

get_num_of_diff_LSB_int(Num1, Num2, Mask, NumOfDiffLSB) ->
    MaskWithZeroLSB = (Mask bsr NumOfDiffLSB) bsl NumOfDiffLSB,
    TempNum1 = Num1 band MaskWithZeroLSB,
    TempNum2 = Num2 band MaskWithZeroLSB,
    case TempNum1 of
        TempNum2 ->
            NumOfDiffLSB;
        _ ->
            get_num_of_diff_LSB_int(Num1, Num2, Mask, NumOfDiffLSB + 1)
    end.

