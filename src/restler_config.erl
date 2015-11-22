-module(restler_config).

-export([
    dispatch/0,
    web_config/0
]).

-spec dispatch() -> [webmachine_dispatcher:route()].
dispatch() ->
    lists:flatten([
        {["users", username, "sensors", sensorid, "history"], restler_sensor_history_resource, []},
        {["users", username, "sensors", sensorid], restler_sensor_resource, []},
        {["users", username], restler_user_resource, []},
        {[], restler_resource, []}
    ]).

web_config() ->
    {ok, App} = application:get_application(?MODULE),
    {ok, Ip} = application:get_env(App, web_ip),
    {ok, Port} = application:get_env(App, web_port),
    [
        {ip, Ip},
        {port, Port},
        {log_dir, "priv/log"},
        {dispatch, dispatch()}
    ].
