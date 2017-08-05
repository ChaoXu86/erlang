-module(rohc_comp_context).
%%% ==============================================================
%%%  BASIC INFORMATION
%%% ==============================================================
%%% Description :
%%% This module implements the context of ROHC compressor
%%%
%%% ==============================================================
%%%  REVISION LOG
%%% ==============================================================
%%% Date    User      What
%%% ------  --------  --------------------------------------------
%%% 170721  exxucao   Creation
%%% ==============================================================

%%% some basic rules for rohc module
%%%
%%% 1. try to use ?ci_var as less as possible
%%% 2. only update context when everything is ok.

-export([init/2,
         get_context/1,
         put_context/1]).

-export([init_dyn/0]).

-include("eps_common.hrl").
-include("rohc_cxt_common.hrl").

-define(nontree_module,true).
-define(library_module,true).
-define(dynamic_proc, true).
-define(sysfunc_vsn_002, true).
-include("ncs_SystemFunctions.hrl").

-record(rohc_compressor,
        {
         max_cid,           %% negotiated Max cid between UE and MME
         large_cid = false, %% not supported yet
         profile,           %% list of supported profile. 
                            %% [rohc_ip_rfc3843, rohc_uncompressed_rfc5795]
        
         contexts = []      %% list of profile context
        }).

-record(rohc_compressor_seg7,
        {
         max_cid,
         large_cid = false,
         profile,
         contexts = []
        }).

%%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
%%% bind_module_data/2
%%% Binding of module data
%%% Packing/unpacking of persistent data (seg7 and seg12).
%%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bind_module_data(L, creation) ->
    ?ci_var(compressor,#rohc_compressor{}),
    L;

bind_module_data(L, _Level) -> 
    Mv = uncompress_pdata(?read_pd(?seg7,#rohc_compressor_seg7{})),
    ?ci_var(compressor, Mv),
    
    L.

%%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
%%% op_stableState/2
%%% Handling of operation stableState
%%% Check if the module is in a stable state
%%% and collect data to save persistent.
%%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
op_stableState({L, P}, Op) ->
    CompContextSeg7 = compress_pdata(?copy_var(compressor)),    
    ?replace_pd(?seg7, CompContextSeg7),
    
    {{L, P}, Op}.

%%% -------------------------------------------------------------
%%% uncompress_pdata(#rohc_compressor{}) -> #rohc_compressor_seg7{} 
%%% -------------------------------------------------------------
compress_pdata(#rohc_compressor
               {max_cid = MaxCid,
                large_cid = LargeCid,
                profile = Profile,
                contexts = Contexts}) ->
    #rohc_compressor_seg7{max_cid  = MaxCid,
                          large_cid = LargeCid,
                          profile  = Profile,
                          contexts = Contexts}.

%%% -------------------------------------------------------------
%%% uncompress_pdata(#rohc_compressor_seg7{}) -> #esm_bearer_context{} 
%%% -------------------------------------------------------------
uncompress_pdata(#rohc_compressor_seg7
                 {max_cid = MaxCid,
                  large_cid = LargeCid,
                  profile = Profile,
                  contexts = Contexts}) ->
    #rohc_compressor{max_cid  = MaxCid,
                     large_cid = LargeCid,
                     profile  = Profile,
                     contexts = Contexts}.


%%% -------------------------------------------------------------
%%% init(MaxCid, Profile) -> void
%%%
%%% init the compressor
%%% -------------------------------------------------------------
init(MaxCid, UEProfile) ->    
    Profile = convert_profile(UEProfile),
    ?co_ci_var(compressor, #rohc_compressor{max_cid = MaxCid,
                                            profile = Profile}).

%%% -------------------------------------------------------------
%%% convert_profile(Profile)) -> ProfileList
%%%
%%% convert profile to human readable profile
%%% -------------------------------------------------------------
convert_profile(Profile) ->
	%% !!! IMPORTANT
	%% sort by preference
	SortedProfiles = lists:keysort(#mme_supported_profile_info.preference,
								   ?mme_supported_profiles),
	[Cb ||#mme_supported_profile_info
		  {id = Id, 
		   callback=Cb} <- SortedProfiles, 
		  Profile == (Profile bor Id)].

%%% -------------------------------------------------------------
%%% get_context(RawPackage) -> Context
%%%
%%% RawPackage - binary()
%%% Context    - #rohc_profile{}
%%%
%%% One context of compressor will be returned
%%% -------------------------------------------------------------
get_context(RawPackage) ->    
    Compressor = ?copy_var(compressor),    
    case find_context(Compressor, RawPackage) of
        false ->            
            create_context(Compressor, RawPackage);
        Context ->
            Context
    end.
    
%%% -------------------------------------------------------------
%%% put_context(Context) -> void
%%%
%%% Context    - #rohc_profile{}
%%%
%%% Update one context of compressor
%%% -------------------------------------------------------------
put_context(Context) ->
    #rohc_compressor
    {
     contexts = Contexts
    } = Compressor = ?copy_var(compressor),
    
    Id = ?cxt_id(Context),
    NewContexts = lists:keystore(Id, #rohc_profile.context_id,
                                 Contexts, Context),
    ?co_ci_var(compressor, 
               Compressor#rohc_compressor{contexts = NewContexts}),
    ok.
        
%%% -------------------------------------------------------------
%%% find_context(Compressor, RawPackage) -> Context | false
%%%
%%% find if there is any existing context could handle the RawPackage
%%% TODO 1. only one uncompressed profile context each compressor
%%%      2. try associate cid0 with uncompressed profile context
%%%         if uncompressed profile is frequent used.
%%% -------------------------------------------------------------
find_context(Compressor, RawPackage) ->
    #rohc_compressor{contexts = ExistingCxts} = Compressor,
    find_context_int(ExistingCxts, RawPackage).

find_context_int([], _) ->
    false;
find_context_int([Context|RestContexts], RawPackage) ->
    case ?cxt_profile(Context):could_handle(Context, RawPackage) of
        {true, UpdatedContext} ->
            UpdatedContext;
        {false, _ } ->
            find_context_int(RestContexts, RawPackage)
    end.

%%% -------------------------------------------------------------
%%% create_context(Compressor, RawPackage) -> Context 
%%%
%%% create rohc profile context
%%% -------------------------------------------------------------
create_context(Compressor, RawPackage) ->
     Cid = allocate_context_id(Compressor),
     #rohc_compressor{profile   = Profiles,
					  large_cid = IsLargeCid} = Compressor,
     create_context_int(Profiles, Cid, IsLargeCid, RawPackage).

create_context_int([], CidorCxt, _IsLargeCid, _RawPackage) ->
    CidorCxt;  
create_context_int([Profile|RestProfiles], CidorCxt, IsLargeCid, RawPackage) ->
    case Profile:create_context(CidorCxt, IsLargeCid, RawPackage) of
        Cxt when is_record(Cxt, rohc_profile) ->
            create_context_int(RestProfiles, Cxt, IsLargeCid, RawPackage);
        _ ->
            %% try next profile
            create_context_int(RestProfiles, CidorCxt, IsLargeCid, RawPackage)
    end.

%%% -------------------------------------------------------------
%%% allocate_context_id(Compressor) -> NewCid 
%%%
%%% allocate context id for new rohc tunnel
%%% 1. find the smallest free cid in the range [0, MaxCID]
%%% 2. If no free cid, reuse the cid of oldest context
%%%
%%% TODO
%%% One special rule for uncompressed profile, RFC 5795 chapter 5.4,
%%% Profile 0x0000 should be associated with a CID the size of
%%% zero or one octet.  Profile 0x0000 SHOULD be associated with at most
%%% one CID.
%%% -------------------------------------------------------------
allocate_context_id(Compressor) ->
    #rohc_compressor{max_cid  = MaxCid,                 
                     contexts = ExistingCxts} = Compressor,
    
    case length(ExistingCxts) == MaxCid + 1 of
         true ->
             %% all cid used, get the oldest context id
             {OldestCid, _} = 
                 lists:foldl(fun(Cxt, {Cid, Ts}) ->
                                     CurCxtTs = ?cxt_timestamp(Cxt),
                                     case  CurCxtTs < Ts of
                                         true ->
                                             {?cxt_id(Cxt), CurCxtTs};
                                         false ->
                                             {Cid, Ts}
                                     end
                             end, 0, ExistingCxts),
             OldestCid;
         false ->
             UsedCids = lists:sort([?cxt_id(Cxt)||Cxt<-ExistingCxts]),
             get_free_cxt_id(UsedCids, 0, MaxCid)
    end.

%%% -------------------------------------------------------------
%%% get_free_cxt_id(UsedIds,IdStart,IdEnd) -> FreeCxtId
%%%
%%% Find first unused Id in range [IdStart, IdEnd]
%%% -------------------------------------------------------------
get_free_cxt_id(_,MaxCxtId,MaxCxtId) ->
    %% not possible, let it crash
    MaxCxtId = crash;
get_free_cxt_id([],FreeId,_MaxCxtId) ->
    %% normal
    FreeId;
get_free_cxt_id([UsedId|Rest],UsedId,MaxCxtId) ->
    get_free_cxt_id(Rest, UsedId + 1, MaxCxtId);
get_free_cxt_id([_UsedId|_Rest],FreeId,_MaxCxtId) ->
    FreeId.

%% test for gtt
%% on AP, run
%% 1> rohc_comp_context:init_dyn().
%% 2> rohc_comp:create(15, 4).
%% 3> rohc_comp:compress(Ipv4).
init_dyn() ->
    put(nsf_init_data,{nsfwos_initData,worker_class, worker_key, parent_if,
        parent_proc, server_ref, ccTag, creation, dynamic_tc,
        park_timer, min_heap_size, worker_if,
                          replica_state}).

    