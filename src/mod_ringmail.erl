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
	 on_user_send_packet_to/5,
	 on_user_send_packet_update/2
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
    ejabberd_hooks:add(user_send_packet_update, Host, ?MODULE, on_user_send_packet_update, 10),
    ok.

stop(Host) ->
    ?INFO_MSG("Stopping mod_ringmail", [] ),
    ejabberd_hooks:delete(user_send_packet_to, Host, ?MODULE, on_user_send_packet_to, 10),
    ejabberd_hooks:delete(user_send_packet_update, Host, ?MODULE, on_user_send_packet_update, 10),
    ok.

on_user_send_packet_to(To, From, C2SState, Attrs, El) ->
    %?INFO_MSG("Attrs: ~p", [Attrs]),
    %?INFO_MSG("XML Element: ~p", [El]),
    %?INFO_MSG("From: ~p", [From]),
	ToParts = re:split(To, "\@", [{parts, 2}]),
	ToID = lists:nth(1, ToParts),
	Host1 = lists:nth(2, ToParts),
	FromID = element(2, From),

	% decode host
	MatchSlash = "/",
	Host = case re:run(Host1, MatchSlash) of
		{match, Captured1} ->
			Host2 = re:split(Host1, "/", [{parts, 2}]),
			lists:nth(1, Host2);
		nomatch ->
			Host1
	end,

	% read conversation uuid from sender
	ConvID = proplists:get_value(<<"conversation">>, Attrs, <<"">>),

	% read reply-to from sender
	ReplyTo = proplists:get_value(<<"reply-to">>, Attrs, <<"">>),

	% request conversation data
	ConvData = request_conversation_data(C2SState, ConvID, FromID, ToID, ReplyTo),
    %?INFO_MSG("Conversation Data: ~p", [ConvData]),
	ConvValid = lists:nth(1, ConvData),

	{ToFinal, FromConv, UUID, Contact, OrigTo} = case ConvValid of 
        <<"ok">> -> {lists:nth(2, ConvData), lists:nth(3, ConvData), lists:nth(4, ConvData), lists:nth(5, ConvData), lists:nth(6, ConvData)};
		_ -> {ToID, FromID, <<"">>, <<"">>, <<"">>}
	end,

	Receipt = case fxml:get_subtag(El, <<"received">>) of
		false -> false;
		Receipt1 -> true
	end,
	FromFinal = case Receipt of
		true -> <<"receipts">>;
		false -> FromConv
	end,
	DataList = [UUID, Contact, OrigTo],

    ?INFO_MSG("Receipt:~p OrigFrom:~p OrigTo:~p NewFrom:~p -> NewTo:~p UUID:~p Contact:~p", [Receipt, FromID, ToID, FromFinal, ToFinal, lists:nth(1, DataList), lists:nth(2, DataList)]),
	{bjoin([ToFinal, <<"@">>, Host]), jid:from_string(bjoin([FromFinal, <<"@">>, Host])), DataList}.

on_user_send_packet_update(El, DataList) ->
	Contact = lists:nth(2, DataList),
	List1 = lists:keystore(<<"conversation">>, 1, El#xmlel.attrs, {<<"conversation">>, lists:nth(1, DataList)}),
    List2 = case lists:nth(3, DataList) of
		null -> List1;
		<<"">> -> List1;
		_ ->
			lists:keystore(<<"originalto">>, 1, List1, {<<"original-to">>, lists:nth(3, DataList)})
	end,
	El1 = El#xmlel{attrs=List2},
	case Contact of 
		null -> El1;
		<<"">> -> El1;
		_ ->
			List = lists:keystore(<<"contact">>, 1, El1#xmlel.attrs, {<<"contact">>, Contact}),
    		%?INFO_MSG("Contact Data: ~p", [List]),
			NewEl = El1#xmlel{attrs=List}
	end.

request_conversation_data(C2SState, ConvID, FromID, ToID, ReplyTo) ->
	PostUrl = gen_mod:get_module_opt(C2SState#state.server, ?MODULE, conversation_url, fun(S) -> iolist_to_binary(S) end, list_to_binary("")),
    Sep = "&",
	Post = [
	  "conv=", url_encode(binary_to_list(ConvID)), Sep,
	  "from=", url_encode(binary_to_list(FromID)), Sep,
	  "to=", url_encode(binary_to_list(ToID)), Sep,
	  "reply=", url_encode(binary_to_list(ReplyTo))
	],
	%?INFO_MSG("Sending post request to ~s with body \"~s\"", [PostUrl, Post]),
	R = httpc:request(post, {binary_to_list(PostUrl), [], "application/x-www-form-urlencoded", list_to_binary(Post)},[],[]),
	Result = case R of
		{ok, {{Version, ReturnCode, State}, Head, Body}} -> 
			jiffy:decode(Body);
		{error, Reason} -> 
			[<<"error">>, <<"error">>]
	end,
	Result.

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

