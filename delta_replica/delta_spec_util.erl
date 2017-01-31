-module(delta_spec_util).
%%% -------------------------------------------------------------------------
%%% Delta spec util 
%%% 
%%% delta_spec_util:get_delta_spec(Old, New) -> DeltaSpec
%%%   to cacluate the delta spec of two given tuples
%%%      Old - {user_info, {timestamp,86996746}, {tid, 1}, {loc, {10,11}, na}}
%%%      New - {user_info, {timestamp,86996817}, {tid, 1}, {loc, {10,12}, na}}
%%%
%%%   When delta_spec_level = 3, the output should be
%%%      DeltaSpec -[{'$1',
%%%                     [{'$2',[{'$2',86996817}]},
%%%                      {'$4',[{'$2',[{'$2',12}]}]}]
%%%                  }]
%%%   DeltaSpec indicates the difference between New and Old, each difference 
%%%   is presented in format [{<pos>, <value>}].
%%%      {'$2',[{'$2',86996817}]} the first '$2' implies the the 2nd element 
%%%      of New is different from Old. In this case, it is the 'timestamp'.
%%%      The following [{'$2',86996817}] means the second element of timestamp
%%%      of New is differnt from of Old and the latest value is 86996817
%%%
%%% merge_delta_spec(OldSpec, NewSpec) -> NewSpec
%%%   to merge two specs
%%%   Considering the following scneario, when value changes from
%%%   {hello, 1,2,3} -> {hello, 1,2,4} -> {hello, 1,1,4}
%%%   Two specs will be generated,
%%%   OldSpec = [{'$4',4}], {hello, 1,2,3} -> {hello, 1,2,4}, 
%%%   NewSpec = [{'$3',1}], {hello, 1,2,4} -> {hello, 1,1,4} 
%%%   After merge, NewSpec = [{'$3',1}, {'$4',4}]
%%%
%%% apply_spec_to_data(Data, Spec) -> NewData
%%%   to apply spec to data
%%%   Data    - {1, [ 2,3], 4}
%%%   Spec    - [{'$1, {'$2',[{'$1',12}]},{'$3',5}]}]
%%%   NewData - {1, [12,3], 5}
%%% -------------------------------------------------------------------------

%%% =========================================================================
%%% REVISION LOG
%%% =========================================================================
%%% Date   Name     What
%%% ------ -------- ---------------------------------------------------------
%%% 170131 ChaoXu  creation
%%% -------------------------------------------------------------------------


-export([get_delta_spec/2,
		 merge_delta_spec/2,
		 
		 is_delta_spec_spec/1,
		 apply_spec_to_data/2]).

%% check get_delta_spec_level/0 for details
-define(delta_spec_level, 3).

%%% -------------------------------------------------------------------------
%%% Delta Spec :: = [{Position, Value}, ...]
%%% Position   :: = atom()
%%% Value      :: = Eterm() | Delta Spec
%%%
%%% NOTE:
%%% 1. Current Delta Spec support list() and tuple() with maximum 50 elements 
%%% 2. Position is atom to avoid the potiential collision with the true value
%%% -------------------------------------------------------------------------
-define(p1,  '$1').
-define(p2,  '$2').
-define(p3,  '$3').
-define(p4,  '$4').
-define(p5,  '$5').
-define(p6,  '$6').
-define(p7,  '$7').
-define(p8,  '$8').
-define(p9,  '$9').
-define(p10, '$10').
-define(p11, '$11').
-define(p12, '$12').
-define(p13, '$13').
-define(p14, '$14').
-define(p15, '$15').
-define(p16, '$16').
-define(p17, '$17').
-define(p18, '$18').
-define(p19, '$19').
-define(p20, '$20').
-define(p21, '$21').
-define(p22, '$22').
-define(p23, '$23').
-define(p24, '$24').
-define(p25, '$25').
-define(p26, '$26').
-define(p27, '$27').
-define(p28, '$28').
-define(p29, '$29').
-define(p30, '$30').
-define(p31, '$31').
-define(p32, '$32').
-define(p33, '$33').
-define(p34, '$34').
-define(p35, '$35').
-define(p36, '$36').
-define(p37, '$37').
-define(p38, '$38').
-define(p39, '$39').
-define(p40, '$40').
-define(p41, '$41').
-define(p42, '$42').
-define(p43, '$43').
-define(p44, '$44').
-define(p45, '$45').
-define(p46, '$46').
-define(p47, '$47').
-define(p48, '$48').
-define(p49, '$49').
-define(p50, '$50').

%% ChaoXu, proved to be useless.
%% -define(positions, { ?p1, ?p2, ?p3, ?p4, ?p5, ?p6, ?p7, ?p8, ?p9,?p10,
%%                     ?p11,?p12,?p13,?p14,?p15,?p16,?p17,?p18,?p19,?p20,
%%                     ?p21,?p22,?p23,?p24,?p25,?p26,?p27,?p28,?p29,?p30,
%%                     ?p31,?p32,?p33,?p34,?p35,?p36,?p37,?p38,?p39,?p40,
%%                     ?p41,?p42,?p43,?p44,?p45,?p46,?p47,?p48,?p49,?p50}).
%% 
%% -define(int_to_pos(Pos), element(Pos,?positions)).

%%% =========================================================================
%%% INTERNAL FUNCTION
%%% =========================================================================
%%% -------------------------------------------------------------------------
%%% pos_to_int
%%% '$1' -> 1
%%% -------------------------------------------------------------------------
pos_to_int(?p1) -> 1;
pos_to_int(?p2) -> 2;
pos_to_int(?p3) -> 3;
pos_to_int(?p4) -> 4;
pos_to_int(?p5) -> 5;
pos_to_int(?p6) -> 6;
pos_to_int(?p7) -> 7;
pos_to_int(?p8) -> 8;
pos_to_int(?p9) -> 9;
pos_to_int(?p10)->10;
pos_to_int(?p11)->11;
pos_to_int(?p12)->12;
pos_to_int(?p13)->13;
pos_to_int(?p14)->14;
pos_to_int(?p15)->15;
pos_to_int(?p16)->16;
pos_to_int(?p17)->17;
pos_to_int(?p18)->18;
pos_to_int(?p19)->19;
pos_to_int(?p20)->20;
pos_to_int(?p21)->21;
pos_to_int(?p22)->22;
pos_to_int(?p23)->23;
pos_to_int(?p24)->24;
pos_to_int(?p25)->25;
pos_to_int(?p26)->26;
pos_to_int(?p27)->27;
pos_to_int(?p28)->28;
pos_to_int(?p29)->29;
pos_to_int(?p30)->30;
pos_to_int(?p31)->31;
pos_to_int(?p32)->32;
pos_to_int(?p33)->33;
pos_to_int(?p34)->34;
pos_to_int(?p35)->35;
pos_to_int(?p36)->36;
pos_to_int(?p37)->37;
pos_to_int(?p38)->38;
pos_to_int(?p39)->39;
pos_to_int(?p40)->40;
pos_to_int(?p41)->41;
pos_to_int(?p42)->42;
pos_to_int(?p43)->43;
pos_to_int(?p44)->44;
pos_to_int(?p45)->45;
pos_to_int(?p46)->46;
pos_to_int(?p47)->47;
pos_to_int(?p48)->48;
pos_to_int(?p49)->49;
pos_to_int(?p50)->50;
pos_to_int(_) -> nok.

%%% -------------------------------------------------------------------------
%%% int_to_pos
%%% '$1' <- 1
%%% -------------------------------------------------------------------------
int_to_pos(1) -> ?p1;
int_to_pos(2) -> ?p2;
int_to_pos(3) -> ?p3;
int_to_pos(4) -> ?p4;
int_to_pos(5) -> ?p5;
int_to_pos(6) -> ?p6;
int_to_pos(7) -> ?p7;
int_to_pos(8) -> ?p8;
int_to_pos(9) -> ?p9;
int_to_pos(10)->?p10;
int_to_pos(11)->?p11;
int_to_pos(12)->?p12;
int_to_pos(13)->?p13;
int_to_pos(14)->?p14;
int_to_pos(15)->?p15;
int_to_pos(16)->?p16;
int_to_pos(17)->?p17;
int_to_pos(18)->?p18;
int_to_pos(19)->?p19;
int_to_pos(20)->?p20;
int_to_pos(21)->?p21;
int_to_pos(22)->?p22;
int_to_pos(23)->?p23;
int_to_pos(24)->?p24;
int_to_pos(25)->?p25;
int_to_pos(26)->?p26;
int_to_pos(27)->?p27;
int_to_pos(28)->?p28;
int_to_pos(29)->?p29;
int_to_pos(30)->?p30;
int_to_pos(31)->?p31;
int_to_pos(32)->?p32;
int_to_pos(33)->?p33;
int_to_pos(34)->?p34;
int_to_pos(35)->?p35;
int_to_pos(36)->?p36;
int_to_pos(37)->?p37;
int_to_pos(38)->?p38;
int_to_pos(39)->?p39;
int_to_pos(40)->?p40;
int_to_pos(41)->?p41; 
int_to_pos(42)->?p42;
int_to_pos(43)->?p43;
int_to_pos(44)->?p44;
int_to_pos(45)->?p45;
int_to_pos(46)->?p46;
int_to_pos(47)->?p47;
int_to_pos(48)->?p48;
int_to_pos(49)->?p49;
int_to_pos(50)->?p50;
int_to_pos(_) -> nok.

      

%%% -------------------------------------------------------------------------
%%% get_delta_spec_level() -> Level
%%%
%%% Level - integer()
%%%
%%% e.g. When{hello, 1,[2,3],3} -> {hello, 1,[2,4],4},
%%% In Level.1, the spec will be [{$3,[2,4]}]
%%% In Level.2, the spec will be [{$3,[{$2,4}]}]
%%% The more higher level, the more fine grind of delta spec will be.
%%% However, fine grind cost more CPU in calculating the spec
%%%
%%% Level.0 means no delta spec
%%%
%%% -------------------------------------------------------------------------
get_delta_spec_level() ->    
    ?delta_spec_level.
    
%%% =========================================================================
%%% EXTERNAL FUNCTION
%%% =========================================================================

%%% -------------------------------------------------------------------------
%%% get_delta_spec(OldData, NewData) -> DeltaSpec
%%%
%%% e.g. get_delta_spec( {hello,1,2    ,null},
%%%                      {hello,1,[2,1],value}) -> DSpec
%%%          
%%%      DSpec - [{'$3',[2,1]},{'$4',value}]
%%% 
%%% -------------------------------------------------------------------------
get_delta_spec(SameData,SameData) ->
	[];
get_delta_spec(OldData, NewData) ->
	case get_delta_spec_level() of
		0 ->
			[];
		Level ->             
			[{?p1, _DeltaSpec}] = create_delta_spec(OldData, NewData, Level)
	end.

%%% -------------------------------------------------------------------------
%%% merge_delta_spec(OldSpec, NewSpec) -> NewSpec
%%%
%%% e.g. {hello, 1,2,3} -> {hello, 1,2,4} -> {hello, 1,1,4}
%%%          {hello, 1,2,3} -> {hello, 1,2,4}, OldSpec = [{'$4',4}]
%%%          {hello, 1,2,4} -> {hello, 1,1,4}, NewSpec = [{'$3',1}]
%%%
%%%      [{'$4',4}] + [{'$3',1}] = [{'$3',1}, {'$4',4}]
%%% After merge, NewSpec = [{'$3',1}, {'$4',4}]
%%%
%%% NOTE, one case could not be handled, when the data changed from A to B, 
%%% then change back to A. The spec could not figure out that the data was
%%% unchanged. But this case should be rare.
%%% -------------------------------------------------------------------------
merge_delta_spec(OldSpec, NewSpec) ->    
    %% NB, spec is always sorted by position
    merge_delta_spec_int(OldSpec,NewSpec).

merge_delta_spec_int([],[]) ->
    [];
merge_delta_spec_int([],NewSpec) ->
    NewSpec;
merge_delta_spec_int(OldSpec,[]) ->
    OldSpec;
merge_delta_spec_int([{SamePos,OldValue}|OldRest],[{SamePos,NewValue}|NewRest]) ->    
    case is_delta_spec_spec(NewValue) of
        true ->        
            case is_delta_spec_spec(OldValue) of
                true ->
                    [{SamePos, merge_delta_spec_int(OldValue,NewValue)}|
                         merge_delta_spec_int(OldRest,NewRest)];
                false ->
                    %% Different level of spec, consider follow scenario
                    %% {record, u} -> {record,{val1,val2}} -> {record,{val3,val2}} 
                    %% OldSpec   - [{$2, {val1, val2}}],
                    %% NewSpec   - [{$2, [{$1, val3}]}],
                    %% MergeSpec - [{$2, {val3, val2}}]
                    %% Don't let lower layer spec overide the upper layer spec
                    [{SamePos, apply_spec_to_data(OldValue,NewValue)}|
                         merge_delta_spec_int(OldRest,NewRest)]
            end;                    
        false ->
            [{SamePos,NewValue}|merge_delta_spec_int(OldRest,NewRest)]
    end;
merge_delta_spec_int([{OldPos,OldValue}|OldRest],[{NewPos,NewValue}|NewRest]) ->
    OldPosInt = pos_to_int(OldPos),
    NewPosInt = pos_to_int(NewPos),
    case OldPosInt < NewPosInt of
        true ->
            [{OldPos,OldValue}|merge_delta_spec_int(OldRest,[{NewPos,NewValue}|NewRest])];
        false ->
            [{NewPos,NewValue}|merge_delta_spec_int([{OldPos,OldValue}|OldRest],NewRest)]
    end;
merge_delta_spec_int(OldValue,NewSpec) ->
    %% TODO, check if this clause is still needed... I think it could be removed
    %% Old is just value, no spec    
    %% When this happens, the delta replica shall degrade to normal replica.
    case is_delta_spec_spec(NewSpec) of
        true ->
            case is_delta_spec_spec(OldValue) of
                false ->
                    apply_spec_to_data(OldValue,NewSpec);
                true ->
                    %% That's embarrassing. Possibly application code also uses
                    %% charactor like '$1','$2'. We have no choice but crash.
                    OldValue = crash
            end;
        false ->
            %% NewOne is Value, should not really happen.
            %% If happens, override the old spec.
            NewSpec
    end.

%%% -------------------------------------------------------------------------
%%% create_delta_spec(OldSegData,NewSegData, Level) -> FullSpec
%%%
%%% OldSegData, NewSegData - Eterm
%%% Level                  - integer()
%%%
%%% e.g. create_delta_spec_info(esm_context,
%%%                                2,
%%%                                {hello,1,2,3},
%%%                                {hello,1,[2,1],1}) -> FullSpec
%%%          
%%%      FullSpec - [{'$1',[{'$3',[2,1]},{'$4',1}]}]
%%% 
%%% -------------------------------------------------------------------------
create_delta_spec(OldSegData,NewSegData, Level) when is_tuple(OldSegData),
                                                     is_tuple(NewSegData) ->    
    create_delta_spec_int(OldSegData, NewSegData, Level, 1).

create_delta_spec_int(_SameData, _SameData, _ExpandLevel, _Pos) ->
    %% Same data    
    [];
create_delta_spec_int(_OldData, NewData, 0, Pos) ->
    %% Expand level reaches 0    
    [{int_to_pos(Pos), NewData}];
create_delta_spec_int(OldData, NewData, ExpandLevel, Pos)
  when is_tuple(OldData) andalso
       is_tuple(NewData) ->    
    OldTupleSize = tuple_size(OldData),
    NewTupleSize = tuple_size(NewData),
    %% Tuple is handle here
    case OldTupleSize of
        NewTupleSize ->
            %% Same size, ExpandLevel > 0, continue lower level spec
            create_delta_spec_int(tuple_to_list(OldData),
                                  tuple_to_list(NewData),
                                  ExpandLevel,
                                  Pos);
        _ ->
            %% Different size, no need to check lower level
            [{int_to_pos(Pos), NewData}]
    end;
create_delta_spec_int(OldData, NewData, ExpandLevel, Pos)
  when is_list(OldData) andalso
       is_list(NewData) ->    
    %% List is handle here
    OldListSize = length(OldData),
    NewListSize = length(NewData),
    case OldListSize of
        NewListSize ->            
            %% Same length, ExpandLevel > 0, continue lower level spec
            [{int_to_pos(Pos),create_delta_spec_int_list_heler(OldData, 
                                                               NewData,
                                                               ExpandLevel - 1, 
                                                               1,
                                                               [])}];
        _ ->
            [{int_to_pos(Pos), NewData}]
    end;
create_delta_spec_int(_OldData, NewData, _ExpandLevel, Pos) ->    
    %% Default clause
    [{int_to_pos(Pos), NewData}].

create_delta_spec_int_list_heler([],[],_ExpandLevel,_Pos,Acc) ->
    lists:reverse(Acc);
create_delta_spec_int_list_heler([OldData|OldRest],[NewData|NewRest],ExpandLevel, Pos,Acc) ->    
    create_delta_spec_int_list_heler
      (OldRest, NewRest, ExpandLevel, Pos + 1,
       create_delta_spec_int(OldData,NewData,ExpandLevel,Pos) ++ Acc).

%%% -------------------------------------------------------------------------
%%% is_delta_spec_spec(Data) -> true | false
%%% 
%%% -------------------------------------------------------------------------
is_delta_spec_spec([FirstElem|_] = Data) ->    
    case is_tuple(FirstElem) of
        true ->
            case tuple_size(FirstElem) > 0 of
                true ->
                    is_integer(pos_to_int(element(1,hd(Data))));
                _ ->
                    false
            end;
        false ->
            false
    end;    
is_delta_spec_spec(_) ->
    false.

    
%%% -------------------------------------------------------------------------
%%% apply_spec_to_data(Data, Spec) -> NewData
%%% 
%%% e.g. Data    - {1, [ 2,3], 4}
%%%      Spec    - [{'$1, {'$2',[{'$1',12}]},{'$3',5}]}]
%%%      NewData - {1, [12,3], 5}
%%% -------------------------------------------------------------------------
apply_spec_to_data(Data, Spec) ->    
    [{?p1, DeltaSpec}] = Spec,
    apply_spec_to_data_int(Data, DeltaSpec).

apply_spec_to_data_int(Data,[]) ->
    Data;
apply_spec_to_data_int(Data,Spec) when is_list(Data) ->
    %% XXX From my experience, tuple is more efficient than list ... 
    tuple_to_list(apply_spec_to_data_int(list_to_tuple(Data),Spec));
apply_spec_to_data_int(Data,[{Pos,SpecOrVal}|RestSpec]) when is_tuple(Data) ->
    PosInt = pos_to_int(Pos),
    case is_delta_spec_spec(SpecOrVal) of
        true ->            
            %% spec, apply the spec to the element at PosInt, then update tuple
            NewElem = apply_spec_to_data_int(element(PosInt, Data),SpecOrVal),            
            apply_spec_to_data_int(setelement(PosInt, Data, NewElem), RestSpec);
        false ->
            %% value, directly update the tuple
            apply_spec_to_data_int(setelement(PosInt, Data, SpecOrVal),RestSpec)
    end.
   
