-module(fcourse_router).
-behaviour(nova_router).

-export([
         routes/1
        ]).

%% The Environment-variable is defined in your sys.config in {nova, [{environment, Value}]}
routes(_Environment) ->
    [#{prefix => "/users",
      security => false,
      routes => [
        {"/", { fcourse_main_controller, index}, #{methods => [options,get]}},
        {"/add",{fcourse_main_controller,add},#{methods=>[post]}},
        {"/update",{fcourse_main_controller,add},#{methods=>[update]}},
        {"/delete",{fcourse_main_controller,delete},#{methods=>[delete]}},
        {"/get",{fcourse_main_controller,get},#{methods=>[get]}},
        {"/getall",{fcourse_main_controller,getall},#{methods=>[get]}},
        {"/assets/[...]", "assets"}
                ]
      }].
