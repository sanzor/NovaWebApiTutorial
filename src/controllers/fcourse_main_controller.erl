-module(fcourse_main_controller).
-export([get/1,
         getall/1,
         add/1,
         delete/1,
         update/1]).

split(List) when is_list(List) ->
    case length(List) rem 2 of
        0 -> split(List,[]);
        1 -> throw(odd_list)
end.
split([],Accu)->Accu;
split([Key,Value|Rest],Accu)->split(Rest,[#{Key => Value}|Accu]).

get(#{ bindings := #{<<"user">> := UserId}})->
    try
        {ok,Port}=eredis:start_link(),
        case eredis:q(Port,["hget","users",UserId]) of
            {ok,Result} ->{json,200,#{},#{<<"UserId">> => list_to_binary(UserId) , <<"value">> => Result}};
             _ -> {status,404}
        end
    catch
        Error:Cause -> {json,500,#{<<"Authorization">> => <<"Basic 1212121">>, <<"Content-Type">> => <<"json">>},#{<<"error">> =>Error , <<"cause">> => Cause}}
    end.

getall(_Request)->
    try
        {ok,Port}=eredis:start_link(),
        {ok,Result}=eredis:q(Port,["hgetall","users"]),
        io:format("List: ~p",[Result]),
        TupleList=split(Result),
        io:format("Formatted : ~p",[TupleList]),
        {json,200,#{},#{<<"users">> => TupleList}}
    catch
        Error:Cause -> {json,500,#{},#{ <<"error">> => Error , <<"cause">> => Cause}}
    end.
add(#{json := #{<<"id">> := Id , <<"age">> := Age}})->
    try
        {ok,Port}=eredis:start_link(),
        {ok,Result}=eredis:q(Port,["hset","users"|[Id,Age]]),
        {json,200,#{},#{<<"result">> => Result}}
    catch
        Error:Cause -> {json,500,#{<<"Content-Type">> => <<"json">>},#{<<"error">> =>Error , <<"cause">> => Cause}}
    end.

delete(#{bindings := #{<<"id">> :=Id}})->
    try
        {ok,Port}=eredis:start_link(),
        {ok,_}=eredis:q(Port,["hdel","users",Id]),
        {status,200}
    catch
        Error:Clause ->{json,500,#{},#{<<"error">> => Error ,<<"cause">> => Clause}}
    end.

update(#{json := #{<<"user">> := User , <<"new_age">> := NewAge}})->
    try
        {ok,Port}=eredis:start_link(),
        case eredis:q(Port,["hget","users",User]) of
            {ok,OldAge} -> {ok,_}=eredis:q(Port,["hset","users"|[User,NewAge]]),
                           {status,200};
            _ ->{status,404}
        end
    catch
        Error:Clause -> {status,500,#{},#{<<"error">> => Error ,<<"clause">> =>Clause}}
    end.

