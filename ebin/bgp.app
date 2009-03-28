{application, bgp,
  [{description, "RFC 4271: Border Gateway Protocol (BGP) version 4."},
   {mod, {bgp_app, []}},
   {modules, [bgp,
              bgp_connection_fsm,
              bgp_listener,
              bgp_messages,
              bgp_receive_scheduler,
              bgp_send_scheduler]}
  ]
}.