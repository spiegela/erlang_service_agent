-type string_list() :: [nonempty_string()].
-type service_attr_input() :: { address | cookie | dist_min | dist_max,
								string()
							  }.
-type service_attr_list() :: [service_attr_input()].

-record(instance, {
    name      :: undefined | atom(),
    bind_addr :: undefined | string(),
    cookie    :: undefined | atom(),
    dist_min  :: undefined | integer(),
    dist_max  :: undefined | integer()
  }).