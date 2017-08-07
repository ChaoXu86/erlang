-module(rohc_ip_rfc3843).

-define(rohc_profile_behavior,true). %% <- important
-include("rohc_cxt_common.hrl").


%% TODO 
%% 1. remove item we don't need for ROHC
%% 2. try to merge ipv4 and ipv6 header info into one
%% -record(ipv4_header_info,        
%%         {
%%          ip_version,
%%          ip_header_length,      
%%          ip_tos,
%%          ip_total_length,
%%          ip_identification,
%%          ip_flags,
%%          ip_fragment_offset,         
%%          ip_ttl,
%%          ip_protocol,
%%          ip_crc,
%%          ip_src_addr,
%%          ip_dst_addr,
%%          ip_optional
%%         }).
%% 
%% -record(ipv6_header_info,
%%         {
%%          ip_version,
%%          ip_toc,
%%          ip_flow_lable,
%%          ip_payload_length,
%%          ip_next_header,
%%          ip_hop_limit,
%%          ip_src_addr,
%%          ip_dst_addr
%%         }). 

%%% -------------------------------------------------------------
%%% create_context(CxtId, IsLarge, RawPackage)  -> #rohc_profile
%%%
%%% CxtId      - integer() | #rohc_profile{}
%%% IsLarge    - boolean() 
%%% RawPackage - binary()
%%%
%%% Create init context for rohc tunnel
%%% -------------------------------------------------------------
create_context(CxtId, IsLarge, RawPackage) when is_integer(CxtId) ->
    %% create context from raw package
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
    end;
create_context(#rohc_profile{profile=?rohc_uncompressed_rfc5795} = Context, 
               _IsLarge,
               _RawPackage)  ->
    %% create context from existing context. 
    %% i.e. the RawPackage has already be parsed by other profile,
    %% let's try whether currect profile could also handle the profile
    
    PktFromLowLayer = Context#rohc_profile.package_tmp,	
    #{header_info := HInfoLow,
      payload     := PayloadLow} = PktFromLowLayer,
    
    %% try to parse the payload as ip profile
    case parse_package(PayloadLow) of
        nok ->
            %% could not be handled by ip profile, 
            %% return lower layer context without changing
            Context;
        #{header_info := HInfo} = Package ->
            %% merge current header info with lower layer header
            NewPackage = Package#{header_info => maps:merge(HInfoLow, HInfo)},
            Context#rohc_profile{profile     = ?rohc_ip_rfc3843,
                                 package_tmp =  NewPackage}
    end;	
create_context(_Context, _IsLarge, _RawPackage) ->
    not_support.

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
        #{header_info := IPH_New} = NewPackage ->            
            #rohc_profile{package = PrePackage} = Context,
            #{header_info := IPH_Pre} = PrePackage,            
            case is_same_ip_stream(IPH_New, IPH_Pre) of
                true ->                    
                    {true, Context#rohc_profile
                     {package_tmp = NewPackage}};
                false ->
                    {false, Context}
            end
    end.    

%%% -------------------------------------------------------------
%%% parse_package(RawPackage) -> #{package} | nok
%%%
%%% check if given binary is a IP package or not
%%% return value is used by rohc_comp.erl
%%% -------------------------------------------------------------
parse_package(RawPackage) when is_binary(RawPackage) ->
    try
        <<Version:4, _/bitstring>> = RawPackage,
        case Version of
            4 -> parse_ipv4_package(RawPackage);
            6 -> parse_ipv6_package(RawPackage)
        end
    catch _:_ ->
              nok    
    end;
parse_package(_) ->
    nok.

parse_ipv4_package(RawPackage) ->      
    TotLen = byte_size(RawPackage),
    << 4:4, HLen:4, SrvcType:8, TotLen:16, 
       ID:16, Flgs:3, FragOff:13,
       TTL:8, Proto:8, HdrChkSum:2/binary,
       SrcIP:4/binary, 
       DstIP:4/binary, 
       RestDgram/binary>> = RawPackage,
    
    %% check length
    if (HLen < 5) or (4*HLen > TotLen) ->
           throw(invalid_ipv4_header);
       true ->
           do_nothing
    end,
    
    %% parse option header
    {OptH, Payload} = 
        if HLen > 5 ->
               OptsLen = 4*(HLen - 5),
               <<Opts:OptsLen/binary,Data/binary>> = RestDgram,
               {Opts, Data};
           true ->
               {undefined, RestDgram}
        end,
    
    #{type        => ip,
      header_info => #{ ip_version         => 4,
                        ip_header_length   => HLen,      
                        ip_tos             => SrvcType,
                        ip_total_length    => TotLen,
                        ip_identification  => ID,
                        ip_flags           => Flgs,
                        ip_fragment_offset => FragOff,         
                        ip_ttl             => TTL,
                        ip_protocol        => Proto,
                        ip_crc             => HdrChkSum,
                        ip_src_addr        => SrcIP,
                        ip_dst_addr        => DstIP,
                        ip_optional        => OptH},
      payload     => Payload}.


parse_ipv6_package(RawPackage) ->
    TotLen = byte_size(RawPackage),
    << 6:4, ToC:8, FlowLbl:20, 
       PayLoadLen:16, NextH:8, HopL:8,
       SrcIP:16/binary, 
       DestIP:16/binary, 
       Payload/binary>> = RawPackage,
    
    %% check length, jumbo payload is not considered
    if PayLoadLen + 40 /= TotLen ->
           throw(invalid_ipv6_header);
       true ->
           do_nothing
    end,
    
    #{type        => ip,
      header_info => #{ ip_version        => 6,
                        ip_toc            => ToC,
                        ip_flow_lable     => FlowLbl,
                        ip_payload_length => PayLoadLen,
                        ip_next_header    => NextH,
                        ip_hop_limit      => HopL,
                        ip_src_addr       => SrcIP,
                        ip_dst_addr       => DestIP},
      payload     => Payload}.

%%% -------------------------------------------------------------
%%% is_same_ip_stream(IPH1, IPH2) -> true | false
%%%
%%% IPH    - #maps{}
%%%
%%% check two ip header belongs to the same ip stream
%%% TODO support for ip v6, next_header and etc need to be checked
%%% -------------------------------------------------------------
is_same_ip_stream(#{ip_version  := 4,                    
                    ip_src_addr := Src,
                    ip_dst_addr := Dst},
                  #{ip_version  := 4,                    
                    ip_src_addr := Src,
                    ip_dst_addr := Dst}) ->
    true;
is_same_ip_stream(_, _) ->
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
    #rohc_profile{sn          = SN,
                  context_id  = Cid,
                  large_cid   = _IsLarge,
                  package_tmp = PackageInfo} = Context,
    %%     0   1   2   3   4   5   6   7
    %%     --- --- --- --- --- --- --- ---
    %%    |         Add-CID octet         |  if for small CIDs and CID != 0
    %%    +---+---+---+---+---+---+---+---+
    %%    | 1   1   1   1   1   1   0 | D |
    %%    +---+---+---+---+---+---+---+---+
    %%    |                               |
    %%    /    0-2 octets of CID info     /  1-2 octets if for large CIDs
    %%    |                               |
    %%    +---+---+---+---+---+---+---+---+
    %%    |            Profile            |  1 octet
    %%    +---+---+---+---+---+---+---+---+
    %%    |              CRC              |  1 octet
    %%    +---+---+---+---+---+---+---+---+
    %%    |                               |
    %%    |         Static chain          |  variable length
    %%    |                               |
    %%    +---+---+---+---+---+---+---+---+
    %%    |                               |
    %%    |         Dynamic chain         |  present if D = 1, variable length
    %%    |                               |
    %%     - - - - - - - - - - - - - - - -
    %%    |                               |
    %%    |           Payload             |  variable length
    %%    |                               |
    %%     - - - - - - - - - - - - - - - -
    
    %% exxucao: type of IR is always <<16#fc>> for IP-only profile
    CidAndType = 
        case Cid of 
            0 ->
                << 16#fc >>;
            C when C > 15 ->
                throw(large_cid_not_supported);
            _ ->
                << Cid:8, 16#fc>>
        end,
    
    Profile = << 16#04 >>,   
    
    #{header_info := PackageHeader} = PackageInfo,
    #{ip_version         := 4,            
      ip_tos             := SrvcType,      
      ip_identification  := ID,               
      ip_ttl             := TTL,
      ip_protocol        := Proto,      
      ip_src_addr        := SrcIP,
      ip_dst_addr        := DstIP} = PackageHeader,
    
    StaticChain = 
        <<4:4,0:4, Proto:8, SrcIP/binary, DstIP/binary>>,
    DynChain = << SrvcType:8, TTL:8, ID:16#20>>,
    Seq = << SN:16>>,
    
    %% calculate CRC
    PktNoCRC = <<CidAndType/binary,Profile/binary, 0,
                 StaticChain/binary,DynChain/binary,Seq/binary>>,
    CRC = rohc_util:crc8(PktNoCRC),  
    
    {ok, Context#rohc_profile{sn=SN+1,
                              package_tmp = undefined,
                              package = PackageInfo},
     <<CidAndType/binary,Profile/binary,CRC/binary,
       StaticChain/binary,DynChain/binary,Seq/binary>>
    };

encode(Context, ?pkt_IR_DYN) ->
    #rohc_profile{sn          = SN,
                  context_id  = Cid,
                  large_cid   = _IsLarge,
                  package_tmp = PackageInfo} = Context,
    %%      0   1   2   3   4   5   6   7
    %%     --- --- --- --- --- --- --- ---
    %%    :         Add-CID octet         : if for small CIDs and CID != 0
    %%    +---+---+---+---+---+---+---+---+
    %%    | 1   1   1   1   1   0   0   0 | IR-DYN packet type
    %%    +---+---+---+---+---+---+---+---+
    %%    :                               :
    %%    /     0-2 octets of CID info    / 1-2 octets if for large CIDs
    %%    :                               :
    %%    +---+---+---+---+---+---+---+---+
    %%    |            Profile            | 1 octet
    %%    +---+---+---+---+---+---+---+---+
    %%    |              CRC              | 1 octet
    %%    +---+---+---+---+---+---+---+---+
    %%    |                               |
    %%    /         Dynamic chain         / variable length
    %%    |                               |
    %%    +---+---+---+---+---+---+---+---+
    %%    :                               :
    %%    /           Payload             / variable length
    %%    :                               :
    %%     - - - - - - - - - - - - - - - -
    
    CidAndType = 
        case Cid of 
            0 ->
                << 16#f8 >>;
            C when C > 15 ->
                throw(large_cid_not_supported);
            _ ->
                << Cid:8, 16#f8>>
        end,
    
    Profile = << 16#04 >>,   
    
    #{header_info := PackageHeader} = PackageInfo,
    #{ip_version         := 4,            
      ip_tos             := SrvcType,      
      ip_identification  := ID,               
      ip_ttl             := TTL
     } = PackageHeader,
    
    DynChain = << SrvcType:8, TTL:8, ID:16#20>>,
    Seq = << SN:16>>,
    
    %% calculate CRC
    PktNoCRC = <<CidAndType/binary,Profile/binary, 0,
                 DynChain/binary,Seq/binary>>,
    CRC = rohc_util:crc8(PktNoCRC),  
    
    {ok, Context#rohc_profile{sn=SN+1,
                              package_tmp = undefined,
                              package = PackageInfo},
     <<CidAndType/binary,Profile/binary,CRC/binary,
       DynChain/binary,Seq/binary>>
    };
encode(Context, ?pkt_UO_2) ->
    {ok, Context, <<>>};

encode(Context, _) ->
    {nok, Context, not_supported}.

%%% -------------------------------------------------------------
%%% decide_package_type(Context, State) -> Packet_Type
%%%
%%% decide package type to sent
%%% -------------------------------------------------------------
decide_package_type(_Context, ?state_ir) ->
    ?pkt_IR;
decide_package_type(Context, ?state_fo) ->
    #rohc_profile{package     = LastPkt,
                  package_tmp = CurrPkt} = Context,
    case get_diff_fields(LastPkt, CurrPkt) of
        [] ->
            %% Static IP ID, not supported
            sid_not_supported;
        [ip_identification] ->
            %% if only ip id changed
            ?pkt_UO_2;
        Diff_fields ->
            case Diff_fields -- [ip_identification, ip_tos, ip_ttl] of
                [] ->
                    %% could be handled by IR-Dyn
                    ?pkt_IR_DYN;
                _ ->
                    %% more fields changed, fallback to IR
                    %% TODO should try pkt_UO_2 extenstion3 rather than pkt_IR
                    ?pkt_IR
            end
    end;        
decide_package_type(_Context, ?state_so) ->
    not_supported_yet.

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
decide_next_state(_Context, State) ->
    list_to_atom(?MODULE_STRING ++ "_" ++ 
                     atom_to_list(State) ++ "_change_state_not_supported").

%%% -------------------------------------------------------------
%%% get_diff_fields(OldPkt, NewPkt) -> [atom()]
%%%
%%% OldPkt - #maps{}
%%% NewPkt - #maps{}
%%%
%%% get the diff field of given two packets
%%% -------------------------------------------------------------    
get_diff_fields(#{header_info:=OldH},#{header_info:=NewH}) ->
    Keys = maps:keys(OldH),
    [Key || Key<-Keys, maps:get(Key, OldH) /= maps:get(Key, NewH)].


