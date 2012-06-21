{application,jn_component,
             [{description,"Jingle Relay"},
              {vsn,"1"},
              {modules,[jingle_handler,jingle_relay,jn_component,
                        jn_component_app,jn_component_sup,notify_handler]},
              {registered,[jnrelay]},
              {applications,[compiler,syntax_tools,lager]},
              {mod,{jn_component_app,[]}}]}.
