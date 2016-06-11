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

-behaviour(gen_mod).

-export([start/2,
	 init/2,
	 stop/1,
	 on_user_send_packet_to/1]).

-define(PROCNAME, ?MODULE).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("logger.hrl").

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

on_user_send_packet_to(Rec) ->
    ?INFO_MSG("Input: ~p", [Rec]),
    To = element(3, Rec),
	To.


