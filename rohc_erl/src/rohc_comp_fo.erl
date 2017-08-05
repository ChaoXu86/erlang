-module(rohc_comp_fo).

-include("rohc_cxt_common.hrl").

-export([compress/2]).

%%% -------------------------------------------------------------
%%% compress(Context, RawPackage) ->  Result
%%%
%%% Context    - #rohc_profile{}
%%% RawPackage - binary()
%%% Result     - {ok,  NewContext, CompressedPkt} |
%%%              {nok, NewContext, ErrorCode}
%%%
%%% compress new package according to profile context
%%% -------------------------------------------------------------
compress(Context, _RawPackage) ->
    PktType = decide_package_type(Context),
    
    case ?cxt_profile(Context):encode(Context, PktType) of
        {ok, NewContext, CompressedPackage} ->
            %% TODO: update FO statistics
            NewContext1 = update_counter(NewContext),
			NewContext2 = update_state(NewContext1, decide_next_state(NewContext1)),              
            {ok, NewContext2, CompressedPackage};
        {nok, NewContext, Error} ->
            %% TODO handle error code
            {nok, NewContext, Error}
    end.

%%% -------------------------------------------------------------
%%% decide_package_type(Context) -> PktType
%%%
%%% Context    - #rohc_profile{}
%%% PktType    - check rohc_cxt_common.hrl
%%%
%%% decide type for current packet
%%% -------------------------------------------------------------
decide_package_type(Context) ->
    ?cxt_profile(Context):decide_package_type(Context, ?state_fo).

%%% -------------------------------------------------------------
%%% decide_next_state(Context) -> NextState
%%%
%%% Context    - #rohc_profile{}
%%% NextState  - ?state_ir | ?state_fo | ?state_so
%%%
%%% decide next state of profile context
%%% -------------------------------------------------------------
decide_next_state(Context) ->
	?cxt_profile(Context):decide_next_state(Context, ?state_fo).

%%% -------------------------------------------------------------
%%% update_counter(Context) -> NewContext
%%%
%%% Context    - #rohc_profile{}
%%%
%%% update counter according to current state
%%% -------------------------------------------------------------
update_counter(Context) ->
	#rohc_profile{fo_count = FOCount} = Context,
	Context#rohc_profile{fo_count = FOCount + 1}.

%%% -------------------------------------------------------------
%%% update_state(Context, CurrState) -> NewContext
%%%
%%% Context    - #rohc_profile{}
%%% CurrState  - ?state_ir | ?state_fo | ?state_so
%%%
%%% update state of profile context
%%% -------------------------------------------------------------
update_state(Context, ?state_fo) ->
	Context;
update_state(Context, OtherState) ->
	Context#rohc_profile{ir_count = 0,
						 fo_count = 0,
						 so_count = 0,
						 state    = OtherState}.
            