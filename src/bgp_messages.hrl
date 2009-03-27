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

-record(as_path_segment, {
    type,
    as_numbers = []
    }).

-record(attributes, {
    origin = not_present,
    as_path = not_present,
    next_hop = not_present,
    med = not_present,
    local_pref = not_present,
    atomic_aggregate = not_present,
    aggregator = not_present,
    unrecognized = [] 
    }).

-record(bgp_open, {
    version = 4,
    my_as,
    hold_time = 180,
    identifier,
    options = []
    }).

-record(bgp_update, {
    withdrawn_routes = [],
    attributes = #attributes{},
    advertised_routes = []
    }).

-record(bgp_notification, {
    error_code,
    error_sub_code,
    data
    }).

-record(bgp_keep_alive, {}).

%% TODO: add fields for route refresh (including ORF)

-record(bgp_route_refresh, {}).

