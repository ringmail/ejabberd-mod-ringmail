%%%----------------------------------------------------------------------
%%% File    : mod_ringmail.erl
%%% Author  : Mike Frager <mfrager@dyl.com>
%%% Purpose : Modificiations needed for RingMail
%%% Created : 6 Jun 2016 by Mike Frager <mfrager@dyl.com>
%%%
%%%
%%% Copyright (C) 2016   Mike Frager
%%%
%%% This program is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation; either version 2 of the
%%% License, or (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with this program; if not, write to the Free Software
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
%%% 02111-1307 USA
%%%
%%%----------------------------------------------------------------------

-module(mod_ringmail).
-author('mfrager@dyl.com').

-compile([{ejabberd_sql_pt}]).

-behaviour(gen_mod).

-export([start/2,
	 init/2,
	 stop/1,
	 on_user_send_packet_to/3
]).

-define(PROCNAME, ?MODULE).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("logger.hrl").
-include("ejabberd_sql_pt.hrl").

-record(state, {socket,
		sockmod,
		socket_monitor,
		xml_socket,
		streamid,
		sasl_state,
		access,
		shaper,
		zlib = false,
		tls = false,
		tls_required = false,
		tls_enabled = false,
		tls_options = [],
		authenticated = false,
		jid,
		user = <<"">>, server = <<"">>, resource = <<"">>,
		sid,
		pres_t,
		pres_f,
		pres_a,
		pres_last,
		pres_timestamp,
		privacy_list,
		conn = unknown,
		auth_module = unknown,
		ip,
		aux_fields = [],
		csi_state = active,
		csi_queue = [],
		mgmt_state,
		mgmt_xmlns,
		mgmt_queue,
		mgmt_max_queue,
		mgmt_pending_since,
		mgmt_timeout,
		mgmt_max_timeout,
		mgmt_resend,
		mgmt_stanzas_in = 0,
		mgmt_stanzas_out = 0,
		ask_offline = true,
		lang = <<"">>}).

start(Host, Opts) ->
    ?INFO_MSG("Starting mod_ringmail", [] ),
    register(?PROCNAME,spawn(?MODULE, init, [Host, Opts])),  
    ok.

init(Host, _Opts) ->
    ejabberd_hooks:add(user_send_packet_to, Host, ?MODULE, on_user_send_packet_to, 10),
    ok.

stop(Host) ->
    ?INFO_MSG("Stopping mod_ringmail", [] ),
    ejabberd_hooks:delete(user_send_packet_to, Host, ?MODULE, on_user_send_packet_to, 10),
    ok.

on_user_send_packet_to(To, From, C2SState) ->
	ToParts = re:split(To, "\@", [{parts, 2}]),
	ToItem = lists:nth(1, ToParts),
	ToHost = lists:nth(2, ToParts),
	MatchSlash = "/",
	ToHostFinal = case re:run(ToHost, MatchSlash) of
		{match, Captured1} ->
			ToHostList = re:split(ToHost, "/", [{parts, 2}]),
			lists:nth(1, ToHostList);
		nomatch ->
			ToHost
	end,
    ?INFO_MSG("From: ~p -- To: ~p", [element(2, From), ToItem]),
	MatchAt = "\%40",
	ReplyDomain = <<"c.ring.ml">>,
	{NewTo, ReplyAddr} = case re:run(ToItem, MatchAt) of
		{match, Captured2} ->
			ToCode = re:split(ToItem, "\%40", [{parts, 2}]),
			ToUser = lists:nth(1, ToCode),
			ToDomain = lists:nth(2, ToCode),
			case ToDomain of 
				ReplyDomain -> 
					% reply of reply :)
    				?INFO_MSG("To Reply Code: ~p", [ToUser]),
					{get_user_from_code(C2SState, ToUser), get_target_from_reply_code(C2SState, ToUser, From)};
				_ ->
					Codes = get_codes_from_target(C2SState, From, bjoin([ToUser, <<"@">>, ToDomain])),
					Reply = lists:nth(2, Codes),
					{get_user_from_code(C2SState, lists:nth(1, Codes)), bjoin([Reply, <<"%40">>, ReplyDomain])}
			end;
		nomatch -> 
			Codes = get_codes_from_target(C2SState, From, ToItem),
			Reply = lists:nth(2, Codes),
			{get_user_from_code(C2SState, lists:nth(1, Codes)), bjoin([Reply, <<"%40">>, ReplyDomain])}
	end,
	FinalTo = bjoin([NewTo, <<"@">>, ToHostFinal]),
    ?INFO_MSG("NewTo: ~p Reply: ~p", [FinalTo, ReplyAddr]),
	%{FinalTo, From}.
	{FinalTo, jid:from_string(bjoin([ReplyAddr, <<"@">>, ToHostFinal]))}.

get_target_from_reply_code(C2SState, ReplyCode, From) ->
	FromItem = element(2, From),
	Q = case catch ejabberd_sql:sql_query(C2SState#state.server, [<<"SELECT (SELECT CONCAT('+', d.did_code, d.did_number) FROM ringmail_staging.ring_did d WHERE d.id=t.did_id) AS phone, (SELECT REPLACE(e.email, '@', '%40') FROM ringmail_staging.ring_email e WHERE e.id=t.email_id) AS email FROM ringmail_staging.ring_user u, ringmail_staging.ring_conversation c, ringmail_staging.ring_target t WHERE c.conversation_code = ">>, quote(ReplyCode), <<" AND u.login = REPLACE(">>, quote(FromItem), <<", '%40', '@') AND u.id = c.from_user_id AND t.id = c.to_user_target_id AND t.user_id = c.from_user_id">>]) of 
		{selected, [<<"phone">>, <<"email">>], Rs} when is_list(Rs) -> Rs;
		Error -> ?ERROR_MSG("~p", [Error]), []
    end,
    ?INFO_MSG("Get Original Target: ~p", [Q]),
	if
		length(Q) > 0 -> 
			case lists:nth(1, Q) of 
				[null, Email] -> Email;
				[Phone, null] -> Phone
			end;
		true ->
			<<"error">>
	end.

get_user_from_code(C2SState, ToCode) -> 
	Q = case catch ejabberd_sql:sql_query(C2SState#state.server, [<<"SELECT REPLACE(u.login, '@', '%40') AS val FROM ringmail_staging.ring_user u, ringmail_staging.ring_conversation c WHERE u.id = c.to_user_id AND c.conversation_code = ">>, quote(ToCode)]) of
		{selected, [<<"val">>], Rs} when is_list(Rs) -> Rs;
		Error -> ?ERROR_MSG("~p", [Error]), []
	end,
    ?INFO_MSG("Get To User By Code: ~p", [Q]),
	lists:nth(1, lists:nth(1, Q)).

get_codes_from_target(C2SState, From, Target) ->
	FromItem = element(2, From),
	Q = case catch ejabberd_sql:sql_query(C2SState#state.server, [<<"SELECT conversation_code AS val, reply_code AS val2 FROM conversation WHERE username = ">>, quote(FromItem), <<" AND target_hash = UNHEX(SHA2(">>, quote(Target), <<", 256))">>]) of
		{selected, [<<"val">>, <<"val2">>], Rs} when is_list(Rs) -> Rs;
		Error -> ?ERROR_MSG("~p", [Error]), []
	end,
	Codes = if
		length(Q) > 0 -> 
			lists:nth(1, Q);
		true ->
			request_codes_for_target(C2SState, FromItem, Target)
	end,
    ?INFO_MSG("Get Code From Target: ~p", [Codes]),
	Codes.

request_codes_for_target(C2SState, FromItem, Target) ->
	PostUrl = gen_mod:get_module_opt(C2SState#state.server, ?MODULE, conversation_url, fun(S) -> iolist_to_binary(S) end, list_to_binary("")),
    Sep = "&",
	Post = [
	  "login=", url_encode(binary_to_list(FromItem)), Sep,
	  "to=", url_encode(binary_to_list(Target))
	],
	%?INFO_MSG("Sending post request to ~s with body \"~s\"", [PostUrl, Post]),
	R = httpc:request(post, {binary_to_list(PostUrl), [], "application/x-www-form-urlencoded", list_to_binary(Post)},[],[]),
	Codes = case R of
		{ok, {{Version,ReturnCode, State}, Head, Body}} -> 
			jiffy:decode(Body);
		{error, Reason} -> 
			[<<"error">>, <<"error">>]
	end,
% store code
	case Codes of 
		[<<"error">>, <<"error">>] -> ok;
		[<<"notfound">>, <<"notfound">>] -> ok;
		_ -> 
			store_codes_for_target(C2SState, FromItem, Target, Codes)
	end,
	Codes.

store_codes_for_target(C2SState, FromItem, Target, Codes) ->
	?INFO_MSG("Store Code \"~p\"", [Codes]),
	ejabberd_sql:sql_query(C2SState#state.server, [<<"INSERT INTO conversation (username, target_hash, conversation_code, reply_code) VALUES (">>, quote(FromItem), <<", UNHEX(SHA2(">>, quote(Target), <<", 256)), ">>, quote(lists:nth(1, Codes)), <<", ">>, quote(lists:nth(2, Codes)), <<")">>]).
	
bjoin(List) ->
	F = fun(A, B) -> <<A/binary, B/binary>> end,
	lists:foldr(F, <<>>, List).

quote(String) when is_list(String) ->
    [39 | lists:reverse([39 | quote(String, [])])]; %% 39 is $'
quote(Bin) when is_binary(Bin) ->
    list_to_binary(quote(binary_to_list(Bin))).

quote([], Acc) ->
    Acc;
quote([0 | Rest], Acc) ->
    quote(Rest, [$0, $\\ | Acc]);
quote([10 | Rest], Acc) ->
    quote(Rest, [$n, $\\ | Acc]);
quote([13 | Rest], Acc) ->
    quote(Rest, [$r, $\\ | Acc]);
quote([$\\ | Rest], Acc) ->
    quote(Rest, [$\\ , $\\ | Acc]);
quote([39 | Rest], Acc) ->        %% 39 is $'
    quote(Rest, [39, $\\ | Acc]); %% 39 is $'
quote([34 | Rest], Acc) ->        %% 34 is $"
    quote(Rest, [34, $\\ | Acc]); %% 34 is $"
quote([26 | Rest], Acc) ->
    quote(Rest, [$Z, $\\ | Acc]);
quote([C | Rest], Acc) ->
    quote(Rest, [C | Acc]).

%%% The following url encoding code is from the yaws project and retains it's original license.
%%% https://github.com/klacke/yaws/blob/master/LICENSE
%%% Copyright (c) 2006, Claes Wikstrom, klacke@hyber.org
%%% All rights reserved.
url_encode([H|T]) when is_list(H) ->
    [url_encode(H) | url_encode(T)];
url_encode([H|T]) ->
    if
        H >= $a, $z >= H ->
            [H|url_encode(T)];
        H >= $A, $Z >= H ->
            [H|url_encode(T)];
        H >= $0, $9 >= H ->
            [H|url_encode(T)];
        H == $_; H == $.; H == $-; H == $/; H == $: -> % FIXME: more..
            [H|url_encode(T)];
        true ->
            case integer_to_hex(H) of
                [X, Y] ->
                    [$%, X, Y | url_encode(T)];
                [X] ->
                    [$%, $0, X | url_encode(T)]
            end
     end;

url_encode([]) ->
    [].

integer_to_hex(I) ->
    case catch erlang:integer_to_list(I, 16) of
        {'EXIT', _} -> old_integer_to_hex(I);
        Int         -> Int
    end.

old_integer_to_hex(I) when I < 10 ->
    integer_to_list(I);
old_integer_to_hex(I) when I < 16 ->
    [I-10+$A];
old_integer_to_hex(I) when I >= 16 ->
    N = trunc(I/16),
    old_integer_to_hex(N) ++ old_integer_to_hex(I rem 16).

