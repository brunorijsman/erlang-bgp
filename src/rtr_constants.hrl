%%%=====================================================================================================================
%%% Copyright (c) 2009, Bruno Rijsman
%%%
%%% Permission to use, copy, modify, and/or distribute this software for any purpose with or without fee is hereby 
%%% granted, provided that the above copyright notice and this permission notice appear in all copies.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL 
%%% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, 
%%% INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN
%%% AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR 
%%% PERFORMANCE OF THIS SOFTWARE.
%%%=====================================================================================================================

%% @author Bruno Rijsman
%% @copyright 2009 Bruno Rijsman

%% @doc Routing instances
%%
-define(RTR_ROUTING_INSTANCE_CORE, 0).

%% @doc Address Family Identifiers (AFIs)
%%
-define(RTR_AFI_IPV4, 1).
-define(RTR_AFI_IPV6, 2).

%% @doc Subsequent Address Family Identifiers (SAFIs)
%%
-define(RTR_SAFI_UNICAST, 1).
-define(RTR_SAFI_MULTICAST, 2).
