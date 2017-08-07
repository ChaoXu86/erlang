-module(rohc_uncompressed_rfc5795).

-define(rohc_profile_behavior,true). %% <- important
-include("rohc_cxt_common.hrl").

%%% -------------------------------------------------------------
%%% create_context(CxtId, IsLarge, RawPackage)  -> #rohc_profile
%%%
%%% CxtId      - integer()
%%% IsLarge    - boolean() 
%%% RawPackage - binary()
%%%
%%% Create init context for rohc tunnel
%%% -------------------------------------------------------------
create_context(CxtId, IsLarge, RawPackage) ->
    case parse_package(RawPackage) of
        nok ->
            not_support;
        Package ->
            Ts = erlang:system_time(milli_seconds),
            #rohc_profile
            {context_id  = CxtId,
             large_cid   = IsLarge,
             state       = ?state_ir,
             mode        = ?mode_u,
             last_act_ts = Ts,                        
             sn          = rohc_util:gen_rand_sn(Ts), 
             package_tmp = Package}
    end.

%%% -------------------------------------------------------------
%%% could_handle(Context, RawPackage) -> {true|false, NewContext} 
%%%
%%% Context    - #rohc_profile{}
%%% RawPackage - binary()
%%%
%%% check whether current context could handle the package
%%% -------------------------------------------------------------
could_handle(Context, RawPackage) ->
    case parse_package(RawPackage) of
        nok ->
            {false, Context};
        #{header_info := #{}} = NewPackage ->
            {true, Context#rohc_profile{package_tmp = NewPackage}}
    end.    

%%% -------------------------------------------------------------
%%% parse_package(RawPackage) -> #{package} | nok
%%%
%%% parse the package
%%% -------------------------------------------------------------
parse_package(RawPackage) when is_binary(RawPackage)->
    #{type        => raw,
      header_info => #{},  %% no header info for raw package
      payload     => RawPackage};
parse_package(_RawPakcage) ->
    false.

%%% -------------------------------------------------------------
%%% encode(Context, PackageType) -> {Result, NewContext, Pkt}
%%%
%%% Context     - #rohc_profile{}
%%% PackageType - check rohc_cxt_common.hrl
%%% Result      - ok | nok
%%% NewContext  - #rohc_profile{}
%%% Pkt         - binary(), encoded
%%%
%%% check two ip header belongs to the same ip stream
%%% -------------------------------------------------------------
encode(Context, ?pkt_IR) ->    
    %%     0   1   2   3   4   5   6   7
    %%     --- --- --- --- --- --- --- ---
    %%    :         Add-CID octet         : if for small CIDs and (CID != 0)
    %%    +---+---+---+---+---+---+---+---+
    %%    | 1   1   1   1   1   1   0 |res|
    %%    +---+---+---+---+---+---+---+---+
    %%    :                               :
    %%    /    0-2 octets of CID info     / 1-2 octets if for large CIDs
    %%    :                               :
    %%    +---+---+---+---+---+---+---+---+
    %%    |          Profile = 0          | 1 octet
    %%    +---+---+---+---+---+---+---+---+
    %%    |              CRC              | 1 octet
    %%    +---+---+---+---+---+---+---+---+
    %%    :                               : (optional)
    %%    /           IP packet           / variable length
    %%    :                               :
    %%     --- --- --- --- --- --- --- ---
    #rohc_profile{context_id  = Cid,
                  large_cid   = _IsLarge,
                  package_tmp = PackageInfo} = Context,
    
    #{payload := RawPackage} = PackageInfo,
    
    %% TODO support for large cid
    CidAndType = 
        case Cid of 
            0 ->
                << 16#fc >>;
            C when C > 15 ->
                throw(large_cid_not_supported);
            _ ->
                << Cid:8, 16#fc>>
        end,
    Profile = << 16#00 >>,   
    PktNoCRC = <<CidAndType/binary,Profile/binary, 0>>,
    
    %% calculate cRC
    CRC = rohc_util:crc8(PktNoCRC),  
    
    %% final result 
    {ok, Context#rohc_profile{package_tmp = undefined,
                              package = PackageInfo},
     <<CidAndType/binary,
       Profile/binary,
       CRC/binary,
       RawPackage/binary>>
    };

encode(Context, ?pkt_normal) ->
    %% 	    0   1   2   3   4   5   6   7
    %%     --- --- --- --- --- --- --- ---
    %%    :         Add-CID octet         : if for small CIDs and (CID != 0)
    %%    +---+---+---+---+---+---+---+---+
    %%    |   first octet of IP packet    |
    %%    +---+---+---+---+---+---+---+---+
    %%    :                               :
    %%    /    0-2 octets of CID info     / 1-2 octets if for large CIDs
    %%    :                               :
    %%    +---+---+---+---+---+---+---+---+
    %%    |                               |
    %%    /      rest of IP packet        / variable length
    %%    |                               |
    %%    +---+---+---+---+---+---+---+---+
    #rohc_profile{context_id  = Cid,
                  large_cid   = _IsLarge,
                  package_tmp = PackageInfo} = Context,
    
    #{payload := RawPackage} = PackageInfo,
    
    %% TODO support for large cid
    Pkt = 
        case Cid of 
            0 ->
                RawPackage;
            C when C > 15 ->
                throw(large_cid_not_supported);
            _ ->
                << Cid:8, RawPackage/binary>>
        end,
    
    %% final result 
    {ok, Context#rohc_profile{package_tmp = undefined,
                              package = PackageInfo},
     Pkt
    }.


%%% -------------------------------------------------------------
%%% decide_package_type(Context, State) -> Packet_Type
%%%
%%% decide package type to sent
%%% NB: for uncompressed profile, there are only two package type
%%% -------------------------------------------------------------
decide_package_type(_Context, ?state_ir) ->
    ?pkt_IR;
decide_package_type(_Context, ?state_fo) ->
    ?pkt_normal;            
decide_package_type(_Context, ?state_so) ->
    ?pkt_normal.

%%% -------------------------------------------------------------
%%% decide_next_state(Context) -> ?state_ir | ?state_fo | ?state_so
%%%
%%% decide next state of profile context
%%% -------------------------------------------------------------
decide_next_state(Context,?state_ir) ->
    #rohc_profile{ir_count = IRCount} = Context,
    case IRCount >= ?ir_count_max of
        true ->
            ?state_fo;
        false ->
            ?state_ir
    end;
decide_next_state(Context, ?state_fo) ->
    %% TODO implement expontienal backoff algorithm instead of hard code
    #rohc_profile{ir_count = IRCount} = Context,
    case IRCount >= 1000 of
        true ->
            ?state_ir;
        false ->
            ?state_fo
    end.
