-type string_list() :: [nonempty_string()].
-type service_attr_input() :: { address | cookie | dist_min | dist_max,
								string()
							  }.
-type service_attr_list() :: [service_attr_input()].

-record(agent_instance, {
    instance_id :: undefined | atom() | '_',
    bind_addr   :: undefined | string() | '_',
    cookie      :: undefined | atom() | '_',
    dist_min    :: undefined | pos_integer() | '_',
    dist_max    :: undefined | pos_integer() | '_'
  }).