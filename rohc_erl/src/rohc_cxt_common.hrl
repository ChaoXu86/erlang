-ifndef(ROHC_CXT_COMMON_HRL).
-define(ROHC_CXT_COMMON_HRL, true).


-ifdef(rohc_profile_behavior).
-export([could_handle/2,
         parse_package/1,
         create_context/3,
         encode/2,
         decide_package_type/2,
         decide_next_state/1]).
-endif.

-record(rohc_profile,
        {
         context_id,           %% integer(), unique context id within the compressor
         profile = ?MODULE,    %% atom(), profile name of context, rohc_ip_rfc3843 | rohc_uncompressed_rfc5795
         state,                %% atom(), state of context, rohc_comp_ir | rohc_comp_fo | rohc_comp_so
         mode,                 %% atom(), mode of context, mode_u | mode_o | mode_r
         target_mode,          %% atom(), target mode of context, mode_u | mode_o | mode_r
         last_act_ts,          %% integer(), creation timestamp of context
         large_cid = false,    %% boolean(), inherit from compressor context, used for encoding CID field
         
         ir_count = 0,         %% integer(), number of package sent in ir state
         fo_count = 0,         %% integer(), number of package sent in fo state
         so_count = 0,         %% integer(), number of package sent in so state 
         
         sn,                   %% integer(), current sequence number
         sn_window,            %% #wlsb{}, window used to encode sn
         package,              %% maps(), common package info of last sent package
         package_tmp,          %% maps(), common package info of current processing package
         generic_tmp           %% maps(), profile specific generic tmp vars
        }).

%% common format of package field.
%% type, header_info and payload must exists
%%   #{type        => ip,
%%     header_info => #{ ip_version        => 6,
%%                       ip_toc            => ToC,
%%                       ip_flow_lable     => FlowLbl,
%%                       ip_payload_length => PayLoadLen,
%%                       ip_next_header    => NextH,
%%                       ip_hop_limit      => HopL,
%%                       ip_src_addr       => SrcIP,
%%                       ip_dst_addr       => DestIP},
%%     payload => Payload}  
%%
%% Maps is flexiable to handle package cross different layer

%% format of generic_tmp
%%   #{diff_fields     => [ ip_identification, ip_crc, ip_ttl ...]
%%     static_changed  => false,
%%     dynamic_changed => true}

  
-define(cxt_id(Cxt),       (element(#rohc_profile.context_id,Cxt))).
-define(cxt_profile(Cxt),  (element(#rohc_profile.profile,Cxt))).
-define(cxt_timestamp(Cxt),(element(#rohc_profile.last_act_ts,Cxt))).

-define(cxt_state(Cxt),    (element(#rohc_profile.state,Cxt))).
-define(cxt_ir_count(Cxt), (element(#rohc_profile.ir_count,Cxt))).
-define(cxt_fo_count(Cxt), (element(#rohc_profile.fo_count,Cxt))).
-define(cxt_so_count(Cxt), (element(#rohc_profile.so_count,Cxt))).

%% -define(cxt_state_set(Cxt, State),(setelement(#rohc_profile.state,
%%                                               Cxt,
%%                                               State))).
%% -define(cxt_ir_count_set(Cxt, Num), (setelement(#rohc_profile.ir_count, 
%%                                                 Cxt,
%%                                                 Num))).
%% -define(cxt_fo_count_set(Cxt, Num), (setelement(#rohc_profile.fo_count, 
%%                                                 Cxt,
%%                                                 Num))).
%% -define(cxt_so_count_set(Cxt, Num), (setelement(#rohc_profile.so_count, 
%%                                                 Cxt,
%%                                                 Num))).

%% profile is encoded in 8 bits
%% hcc profile definition TS 24.301 chapter 9.9.4.22
%% profile id definition RFC 5795 chapter 8
%% +-------+-------+-------+-------+-------+-------+-------+-------+
%% |Spare  |P0x0104|P0x0103|P0x0102|P0x0006|P0x0004|P0x0003|P0x0002|
%% +-------+-------+-------+-------+-------+-------+-------+-------+
%%
%% ID      Profile                    Doc          MME supported 
%% 0x0000  ROHC uncompressed          RFC 5795     Yes           
%% 0x0001  ROHC RTP                   RFC 3095     No
%% 0x0002  ROHC UDP                   RFC 3095     No
%% 0x0003  ROHC ESP                   RFC 3095     No
%% 0x0004  ROHC IP                    RFC 3843     Yes
%% 0x0005  ROHC LLA                   RFC 3242     No
%% 0x0105  ROHC LLA with R-mode       RFC 3408     No
%% 0x0006  ROHC TCP                   RFC 4996     No
%% 0x0007  ROHC RTP/UDP-Lite          RFC 4019     No
%% 0x0008  ROHC UDP-Lite              RFC 4019     No
%% 0x0101  ROHCv2 RTP                 RFC 5225     No
%% 0x0102  ROHCv2 UDP                 RFC 5225     No
%% 0x0103  ROHCv2 ESP                 RFC 5225     No
%% 0x0104  ROHCv2 IP                  RFC 5225     No
%% 0x0107  ROHCv2 RTP/UDP-Lite        RFC 5225     No
%% 0x0108  ROHCv2 UDP-Lite            RFC 5225     No

%% profile name, i.e. callback module name
-define(rohc_uncompressed_rfc5795,rohc_uncompressed_rfc5795).
-define(rohc_ip_rfc3843,rohc_ip_rfc3843).

-record(mme_supported_profile_info,
		{id,           %% id defined above
		 callback,     %% callback
		 preference    %% preference, larger number with higher preference
		 }).

-define(mme_supported_profiles,              
		[
		 #mme_supported_profile_info
		 {id         = 16#00,
		  callback   = ?rohc_uncompressed_rfc5795,
		  preference = 0
		  },
		 #mme_supported_profile_info
		 {id         = 16#04,
		  callback   = ?rohc_ip_rfc3843,
		  preference = 1
		 }
		]).
		
%% rohc compressor state
-define(state_ir, rohc_comp_ir).
-define(state_fo, rohc_comp_fo).
-define(state_so, rohc_comp_so).

%% rohc mode
-define(mode_u, mode_u).
-define(mode_o, mode_o).
-define(mode_r, mode_r).

%% rohc package type
-define(pkt_IR,     pkt_IR).
-define(pkt_IR_DYN, pkt_IR_DYN).
-define(pkt_UO_2,   pkt_UO_2).

%% uncompressed profile
-define(pkt_normal, pkt_normal).

-define(ir_count_max, 3).
-define(fo_count_max, 3).

%% TODO remove DBG
-define(DBG(Format, Params), case rohc_comp_context:is_debug_on() of
                                 true -> io:format("[ROHC DEBUG] [~p:~p(~p)] "++ Format, [?MODULE,?FUNCTION_NAME,?LINE|Params]),
                                         io:format("~n");
                                 false -> do_nothing
                             end).

-endif.