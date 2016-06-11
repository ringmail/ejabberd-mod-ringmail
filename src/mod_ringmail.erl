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
	 on_user_send_packet_to/4]).

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

on_user_send_packet_to(To, C2SState, From, OrigTo) ->
    ?INFO_MSG("Input: ~p", [To]),
%    ?INFO_MSG("State: ~p", [C2SState]),
	Q = case catch ejabberd_sql:sql_query(C2SState#state.server, [<<"select count(username) as val from users;">>]) of
		{selected, [<<"val">>], Rs} when is_list(Rs) -> Rs;
		Error -> ?ERROR_MSG("~p", [Error]), []
	end,
    ?INFO_MSG("Query: ~p", [Q]),
	To.

