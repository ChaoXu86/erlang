%% %% @author epeipji
%% %% @doc @todo Add description to rohc_util.
%% 
%% 
%% -module(rohc_util).
%% 
%% -include("rohc_util.hrl").
%% %% ====================================================================
%% %% API functions
%% %% ====================================================================
%% -export([
%%          crc_calculate/5,
%%          decompression/1,
%%          create_configuration/1]).
%% 
%% 
%% 
%% %% ====================================================================
%% %% Internal functions
%% %% ====================================================================
%% %% Calculate the checksum for the given data.
%% crc_calculate(CRCType, PayLoad, Length, Init_val, CRCTable) ->    
%%     
%%     %% call the function that corresponds to the CRC type
%%     case CRCType of
%%         ?ROHC_CRC_TYPE_3 ->
%%             crc_calc_3(PayLoad, Length, Init_val, CRCTable);
%%         ?ROHC_CRC_TYPE_7 ->
%%             crc_calc_7(PayLoad, Length, Init_val, CRCTable);
%%         ?ROHC_CRC_TYPE_8 ->
%%             crc_calc_8(PayLoad, Length, Init_val, CRCTable)
%%     end.
%% 
%% crc_calc_3(PayLoad, 0, CRC, CRCTable) ->
%%     CRC;
%% crc_calc_3(PayLoad, Length, CRC, CRCTable) ->
%%        N = PayLoad bxor (CRC band 2#0111),                                     
%%        NewCRC = lists:nth(N, CRCTable),
%%      crc_calc_3(PayLoad, Length-1, NewCRC, CRCTable).
%% 
%% crc_calc_7(PayLoad, 0, CRC, CRCTable) ->
%%     CRC;
%% crc_calc_7(PayLoad, Length, CRC, CRCTable) ->
%%        N = PayLoad bxor (CRC band 2#01111111),                                     
%%        NewCRC = lists:nth(N, CRCTable),
%%      crc_calc_7(PayLoad, Length-1, NewCRC, CRCTable).
%% 
%% crc_calc_8(PayLoad, 0, CRC, CRCTable) ->
%%     CRC;
%% crc_calc_8(PayLoad, Length, CRC, CRCTable) ->
%%        N = PayLoad bxor (CRC band 2#11111111),                                     
%%        NewCRC = lists:nth(N, CRCTable),
%%      crc_calc_8(PayLoad, Length-1, NewCRC, CRCTable).
%%     
%% %% crc_calc_3(const uint8_t *const buf,
%% %%                                  const size_t size,
%% %%                                  const uint8_t init_val,
%% %%                                  const uint8_t *const crc_table)
%% %% {
%% %%         uint8_t crc = init_val;
%% %%         size_t i;
%% %% 
%% %%         for(i = 0; i < size; i++)
%% %%         {
%% %%                 crc = crc_table[buf[i] ^ (crc & 7)];
%% %%         }
%% %% 
%% %%         return crc;
%% %% }
%% 
%% %% Compute the CRC-STATIC part of an IP header
%% %%  all fields expect those for CRC-DYNAMIC
%% %%    - bytes 1-2, 7-10, 13-20 in original IPv4 header
%% %%    - bytes 1-4, 7-40 in original IPv6 header
%% %%
%% %% This function is one of the functions that must exist in one profile for the
%% %% framework to work.
%% %%
%% %% @param outer_ip    The outer IP packet
%% %% @param inner_ip    The inner IP packet if there is 2 IP headers, NULL otherwise
%% %% @param next_header The next header located after the IP header(s)
%% %% @param crc_type    The type of CRC
%% %% @param init_val    The initial CRC value
%% %% @param crc_table   The pre-computed table for fast CRC computation
%% %% @return            The checksum
%% compute_crc_static(Outer_ip, Inner_ip, NextHeader, CRCType, InitVal, CRCTable) ->
%%         const struct ip_hdr *const outer_ip_hdr = (struct ip_hdr *) outer_ip;
%%         uint8_t crc = init_val;
%% 
%%         %% first IPv4 header 
%% 
%%         case outer_ip_hdr->version of
%%         IPV4 ->
%%                 const struct ipv4_hdr *ip_hdr = (struct ipv4_hdr *) outer_ip,
%% 
%%                 %% bytes 1-2 (Version, Header length, TOS) 
%%                 crc = crc_calculate(crc_type, (uint8_t *)(ip_hdr), 2, crc, crc_table),
%%                 %% bytes 7-10 (Flags, Fragment Offset, TTL, Protocol)
%%                 crc = crc_calculate(crc_type, (uint8_t *)(&ip_hdr->frag_off), 4, crc, crc_table),
%%                 %% bytes 13-20 (Source Address, Destination Address)
%%                 crc = crc_calculate(crc_type, (uint8_t *)(&ip_hdr->saddr), 8, crc, crc_table);
%%         _IPV6 ->
%%                 %%first IPv6 header
%%                 const struct ipv6_hdr *ip_hdr = (struct ipv6_hdr *) outer_ip,
%% 
%%                 %% bytes 1-4 (Version, TC, Flow Label) 
%%                 crc = crc_calculate(crc_type, (uint8_t *)(&ip_hdr->version_tc_flow), 4, crc, crc_table),
%%                 %% bytes 7-40 (Next Header, Hop Limit, Source Address, Destination Address)
%%                 crc = crc_calculate(crc_type, (uint8_t *)(&ip_hdr->nh), 34, crc, crc_table),
%%                 %% IPv6 extensions 
%%                 crc = ipv6_ext_calc_crc_static(outer_ip, crc_type, crc, crc_table)
%%         end,
%% 
%%         %% second header 
%%         case inner_ip of
%%              NULL -> do_nothing;
%%             _InnerIp ->
%%                 const struct ip_hdr *const inner_ip_hdr = (struct ip_hdr *) inner_ip,
%% 
%%                 %% IPv4
%%                 case inner_ip_hdr->version of
%%                 IPV4 ->
%%                         const struct ipv4_hdr *ip_hdr = (struct ipv4_hdr *) inner_ip,
%% 
%%                         %% bytes 1-2 (Version, Header length, TOS) 
%%                         crc = crc_calculate(crc_type, (uint8_t *)(ip_hdr), 2, crc, crc_table),
%%                         %% bytes 7-10 (Flags, Fragment Offset, TTL, Protocol) 
%%                         crc = crc_calculate(crc_type, (uint8_t *)(&ip_hdr->frag_off), 4, crc, crc_table),
%%                         %% bytes 13-20 (Source Address, Destination Address) 
%%                         crc = crc_calculate(crc_type, (uint8_t *)(&ip_hdr->saddr), 8, crc, crc_table);
%%                 _IPv6 ->
%%                         const struct ipv6_hdr *ip_hdr = (struct ipv6_hdr *) inner_ip,
%% 
%%                         %% bytes 1-4 (Version, TC, Flow Label) 
%%                         crc = crc_calculate(crc_type, (uint8_t *)(&ip_hdr->version_tc_flow), 4, crc, crc_table),
%%                         %% bytes 7-40 (Next Header, Hop Limit, Source Address, Destination Address) 
%%                         crc = crc_calculate(crc_type, (uint8_t *)(&ip_hdr->nh), 34, crc, crc_table),
%%                         %% IPv6 extensions 
%%                         crc = ipv6_ext_calc_crc_static(inner_ip, crc_type, crc, crc_table)
%%                 end
%%         end.
%% %% @brief Compute the CRC-DYNAMIC part of an IP header
%% %% Concerned fields are:
%% %%   - bytes 3-4, 5-6, 11-12 in original IPv4 header
%% %%   - bytes 5-6 in original IPv6 header
%% %%
%% %% @param outer_ip    The outer IP packet
%% %% @param inner_ip    The inner IP packet if there is 2 IP headers, NULL otherwise
%% %% @param next_header The next header located after the IP header(s)
%% %% @param crc_type    The type of CRC
%% %% @param init_val    The initial CRC value
%% %% @param crc_table   The pre-computed table for fast CRC computation
%% %% @return            The checksum
%% compute_crc_dynamic( outer_ip, inner_ip, next_header, crc_type, init_val, crc_table) ->
%%         const struct ip_hdr *const outer_ip_hdr = (struct ip_hdr *) outer_ip;
%%         uint8_t crc = init_val;
%% 
%%         %% first IPv4 header 
%%         case outer_ip_hdr->version of
%%         IPV4 ->
%%                 const struct ipv4_hdr *ip_hdr = (struct ipv4_hdr *) outer_ip,
%%                 %% bytes 3-6 (Total Length, Identification) 
%%                 crc = crc_calculate(crc_type, (uint8_t *)(&ip_hdr->tot_len), 4, crc, crc_table),
%%                 %% bytes 11-12 (Header Checksum) 
%%                 crc = crc_calculate(crc_type, (uint8_t *)(&ip_hdr->check), 2, crc, crc_table);
%%         _IPV6 ->
%%         %% first IPv6 header 
%%                 const struct ipv6_hdr *ip_hdr = (struct ipv6_hdr *) outer_ip,
%%                 %% bytes 5-6 (Payload Length) 
%%                 crc = crc_calculate(crc_type, (uint8_t *)(&ip_hdr->plen), 2, crc, crc_table),
%%                 %% IPv6 extensions (only AH is CRC-DYNAMIC) 
%%                 crc = ipv6_ext_calc_crc_dyn(outer_ip, crc_type, crc, crc_table)
%%         end,
%% 
%%         %% second_header 
%%         case inner_ip of
%%         NULL -> do_nothing;
%%         _INnerIp ->
%%                 const struct ip_hdr *const inner_ip_hdr = (struct ip_hdr *) inner_ip,
%%                 %% IPv4
%%                 case inner_ip_hdr->version of
%%                 IPV4 ->
%%                         const struct ipv4_hdr *ip_hdr = (struct ipv4_hdr *) inner_ip,
%%                         %% bytes 3-6 (Total Length, Identification)
%%                         crc = crc_calculate(crc_type, (uint8_t *)(&ip_hdr->tot_len), 4, crc, crc_table),
%%                         %% bytes 11-12 (Header Checksum) 
%%                         crc = crc_calculate(crc_type, (uint8_t *)(&ip_hdr->check), 2, crc, crc_table);
%%                 _IPV6 ->
%%                 %% IPv6
%%                         const struct ipv6_hdr *ip_hdr = (struct ipv6_hdr *) inner_ip,
%%                         %% bytes 5-6 (Payload Length)
%%                         crc = crc_calculate(crc_type, (uint8_t *)(&ip_hdr->plen), 2, crc, crc_table),
%%                         %% IPv6 extensions (only AH is CRC-DYNAMIC) 
%%                         crc = ipv6_ext_calc_crc_dyn(inner_ip, crc_type, crc, crc_table)
%%                 end
%%         end.
%% 
%% %% Compute the CRC-STATIC part of IPv6 extensions
%% %% All extensions are concerned except entire AH header
%% %% @param ip          The IPv6 packet
%% %% @param crc_type    The type of CRC
%% %% @param init_val    The initial CRC value
%% %% @param crc_table   The pre-computed table for fast CRC computation
%% %% @return            The checksum
%% ipv6_ext_calc_crc_static(ip, crc_type,init_val, crc_table) ->
%%         uint8_t crc = init_val,
%%         const uint8_t *ext,
%%         uint8_t ext_type,
%% 
%%         ext = ipv6_get_first_extension(ip, &ext_type),
%%         while(ext != NULL)
%%         {
%%                 if(ext_type != ROHC_IPPROTO_AH)
%%                 {
%%                         crc = crc_calculate(crc_type, ext, ip_get_extension_size(ext),
%%                                             crc, crc_table);
%%                 }
%%                 ext = ip_get_next_ext_from_ext(ext, &ext_type);
%%         }
%% 
%%         return crc;
%% }
%% 
%% 
%% /**
%%  * @brief Compute the CRC-DYNAMIC part of IPv6 extensions
%%  *
%%  * Only entire AH header is concerned.
%%  *
%%  * @param ip          The IPv6 packet
%%  * @param crc_type    The type of CRC
%%  * @param init_val    The initial CRC value
%%  * @param crc_table   The pre-computed table for fast CRC computation
%%  * @return            The checksum
%%  */
%% ipv6_ext_calc_crc_dyn(ip,crc_type,init_val,crc_table) ->
%%         uint8_t crc = init_val;
%%         const uint8_t *ext;
%%         uint8_t ext_type;
%% 
%%         assert(ip != NULL);
%% 
%%         ext = ipv6_get_first_extension(ip, &ext_type);
%%         while(ext != NULL)
%%         {
%%                 if(ext_type == ROHC_IPPROTO_AH)
%%                 {
%%                         crc = crc_calculate(crc_type, ext, ip_get_extension_size(ext),
%%                                             crc, crc_table);
%%                 }
%%                 ext = ip_get_next_ext_from_ext(ext, &ext_type);
%%         }
%% 
%%         return crc;
%% }.
%% 
%% %%% -------------------------------------------------------------
%% %%% rohc_crc_get_polynom(CRCType, Poly) -> boolen()
%% %%%
%% %%% CRCType:rohc_crc_type_t
%% %%% Poly: the polynom for the requested CRC type
%% %%% Description: Get the polynom for the given CRC type
%% %%% -------------------------------------------------------------
%% rohc_crc_get_polynom(?ROHC_CRC_TYPE_3) ->    %% CRC-3/MMC ploynom 1 + x + x^3
%%     16#B; 
%% rohc_crc_get_polynom(?ROHC_CRC_TYPE_7) ->    %% CRC-7/MMC ploynom 1 + x + x^2 + x^3 + x^6 + x^7
%%     16#CF;
%% rohc_crc_get_polynom(?ROHC_CRC_TYPE_8) ->    %% CRC-8/ROHC ploynom x8+x2+x+1
%%     16#07;
%% rohc_crc_get_polynom(OtherCRCType) ->    
%%     undefined.
%% 
%% 
%% /**
%%  * @brief Get the first extension in an IPv6 packet
%%  *
%%  * @param ip   The IPv6 packet
%%  * @param type The type of the extension
%%  * @return     The extension, NULL if there is no extension
%%  */
%% static uint8_t * ipv6_get_first_extension(const uint8_t *const ip,
%%                                           uint8_t *const type)
%% {
%%         struct ipv6_hdr *ip_hdr;
%% 
%%         assert(ip != NULL);
%%         assert(type != NULL);
%% 
%%         ip_hdr = (struct ipv6_hdr *) ip;
%%         *type = ip_hdr->nh;
%% 
%%         if(rohc_is_ipv6_opt(*type))
%%         {
%%                 /* known extension header */
%%                 return (((uint8_t *) ip) + sizeof(struct ipv6_hdr));
%%         }
%%         else
%%         {
%%                 /* no known extension header */
%%                 return NULL;
%%         }
%% }
%% 
%% 
%% 
%% 
%% 
