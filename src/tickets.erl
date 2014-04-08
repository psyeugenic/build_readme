%%
%% Copyright (C) 2014 Björn-Egil Dahlberg
%%
%% File:    tickets.erl
%% Author:  Björn-Egil Dahlberg
%% Created: 2014-04-07
%%

-module(tickets).
-export([test/0,context/1,from_id/2]).
-record(ctx,{
	path="."
    }).
-include_lib("build_readme/include/tickets.hrl").


context([]) -> context([{path,"/home/otp/TICKETS"}]);
context([{path,Path}]) ->
    #ctx{
	path=Path
    }.

from_id(<<"OTP-",_/binary>>=Id, #ctx{path=Path}) ->
    {ok,Bin} = file:read_file(filename:join([Path,Id])),
    parse_ticket(binary:split(Bin,[<<"\n">>], [global]),#ticket{id=Id}).

%parse_ticket([],Ticket) -> Ticket;
%parse_ticket([Line|Lines], T0) ->
%    case line(Line,T0) of
%	{done,T1} -> T1;
%	T1 -> parse_ticket(Lines, T1)
%    end.
%

parse_ticket(Lines,#ticket{}=T) ->
    %% two iterations,
    %% 1) property list
    %% 2) record
    Ps = to_property(Lines),
    to_ticket(Ps,T).


to_ticket([{K,V}|Ps],T) ->
    to_ticket(Ps,property(K,V,T));
to_ticket([],T) -> T.


property(label,V,T) ->
    T#ticket{ label = V };
property(status,V,T) ->
    T#ticket{ status = to_status(V) };
property(priority,V,T) ->
    T#ticket{ priority = to_priority(V) };
property(type,V,T) ->
    T#ticket{ type = to_type(V) };
property(source,V,T) ->
    T#ticket{ source = to_source(V) };
property(highlight,V,T) ->
    T#ticket{ highlight = to_boolean(V) };
property(incompatible,V,T) ->
    T#ticket{ incompatible = to_boolean(V) };
property(created,V,T) ->
    T#ticket{ created_on = to_datetime(V) };
property(created_by,V,T) ->
    T#ticket{ created_by = to_created_by(V) };
property(otp_releases,V,T) ->
    T#ticket{ otp_releases = to_otp_releases(V) };
property(applications,V,T) ->
    T#ticket{ applications = to_applications(V) };
property(assigned_to,V,T) ->
    T#ticket{ assigned_to = to_assigned_to(V) };
property(fixed_in,V,T) ->
    T#ticket{ fixed_in = to_fixed_in(V) };
property(planned_for,V,T) ->
    T#ticket{ planned_for = to_fixed_in(V) };
property(release_note,V,T) ->
    T#ticket{ release_note = V };
property(last_update,V,T) ->
    {Author,Datetime} = to_author_datetime(V),
    T#ticket{
	updated_by = Author,
	updated_on = Datetime
    };
property(_,_,T) -> T.

%% to_property(Lines) -> [{atom(), binary()}].

to_property(Lines) ->
    to_property(Lines,none,[],[]).

to_property([],K0,Vs,Out) -> update_result(K0,Vs,Out);
to_property([Line|Lines],K0,Vs,Out) ->
    case line(Line) of
	ignore ->
	    to_property(Lines,K0,Vs,Out);
	{value,Val} ->
	    to_property(Lines,K0,[Val|Vs],Out);
	{key,Key,Val} ->
	    to_property(Lines,Key,[Val], update_result(K0,Vs,Out))
    end.

update_result(none,_,Result) ->
    Result;
update_result(K,Vs,Result) ->
    [{K,strip(list_to_binary(lists:reverse(Vs)))}|Result].

line(<<"*Id:",V/binary>>)            -> {key,id,V};
line(<<"*Label:",V/binary>>)         -> {key,label,V};
line(<<"*Status:",V/binary>>)        -> {key,status,V};
line(<<"*Type:",V/binary>>)          -> {key,type,V };
line(<<"*Source:",V/binary>>)        -> {key,source,V };
line(<<"*Priority:",V/binary>>)      -> {key,priority,V };
line(<<"*Creator:",V/binary>>)       -> {key,created_by,V };
line(<<"*AssignedTo:",V/binary>>)    -> {key,assigned_to,V};
line(<<"*Application:",V/binary>>)   -> {key,applications,V};
line(<<"*Highlight:",V/binary>>)     -> {key,highlight, V};
line(<<"*Incompatible:",V/binary>>)  -> {key,incompatible,V};
line(<<"*FixedInRel:",V/binary>>)    -> {key,fixed_in,V};
line(<<"*PlannedForRel:",V/binary>>) -> {key,planned_for,V};
line(<<"*TicketRev:",V/binary>>)     -> {key,revision,V};
line(<<"*RelatedId(s):",V/binary>>)  -> {key,related_ids,V};
line(<<"*Purpose:",V/binary>>)       -> {key,purpose,V};
line(<<"*Description:",V/binary>>)   -> {key,description,V};
line(<<"*Release note:",V/binary>>)  -> {key,release_note,V};
line(<<"*CreationDate:",V/binary>>)  -> {key,created,V};
line(<<"*Originator(s):",V/binary>>) -> {key,originators,V};
line(<<"*OTP rel(s):",V/binary>>)    -> {key,otp_releases,V};
line(<<"*Test case:",V/binary>>)     -> {key,test_case,V};
line(<<"*LastUpdate:",V/binary>>)    -> {key,last_update,V};
line(<<"*GitBranch:",V/binary>>)     -> {key,git_branch,V};
line(<<"*GitRange:",V/binary>>)      -> {key,git_range,V};
line(<<"*Notes:",V/binary>>)         -> {key,notes,V};
%% old values
line(<<"*Document(s):",V/binary>>)  -> {key,documents,V};
line(<<"*Todo:",V/binary>>)         -> {key,todo,V};
line(<<"*Problem:",V/binary>>)      -> {key,problem,V};
line(<<"*Cost:",V/binary>>)         -> {key,cost,V};
line(<<"*Done:",V/binary>>)         -> {key,done,V};
line(<<"*Module(s):",V/binary>>)    -> {key,modules,V};
line(<<"*NewRev(s):",V/binary>>)    -> {key,new_revs,V};
line(<<"*FixInRelease:",V/binary>>) -> {key,fixed_in,V};
line(<<"*SubStatus:",V/binary>>)    -> {key,sub_status,V};
%% newline
line(<<>>) -> {value, <<"\n">>};
%% ignore
line(<<"--------------------------- OTP Ticket -",_/binary>>) -> ignore;
line(<<"----------------------------------------",_/binary>>) -> ignore;

line(B) ->
    {value, B}.

%% aux
%%


split(<<>>) -> [];
split(B) ->
    [Item||Item <- binary:split(B,[
		<<" ">>,<<",">>,<<"\t">>,<<"(">>,<<")">>,<<"/">>
	    ],[global]), Item =/= <<>>].

strip(B) -> strip_trail(strip_lead(B)).
strip_lead(<<32,Rest/binary>>) -> strip_lead(Rest);
strip_lead(<<13,Rest/binary>>) -> strip_lead(Rest);
strip_lead(<<10,Rest/binary>>) -> strip_lead(Rest);
strip_lead(<<9, Rest/binary>>) -> strip_lead(Rest);
strip_lead(B) -> B.

strip_trail(<<>>=B) -> B;
strip_trail(B) ->
    strip_trail(byte_size(B)-1,B).

strip_trail(N,B0) -> 
    <<B:N/binary,A>> = B0,
    case A of
	32 -> strip_trail(B);
	13 -> strip_trail(B);
	10 -> strip_trail(B);
	 9 -> strip_trail(B);
	 _ -> B0
    end.

%% result values

to_boolean(<<>>) -> false;
to_boolean(<<"no",_/binary>>) -> false;
to_boolean(<<"on",_/binary>>) -> false; %% typo?
to_boolean(<<"No",_/binary>>) -> false;
to_boolean(<<"false",_/binary>>) -> false;
to_boolean(<<"-">>) -> false;
to_boolean(<<"\"\"">>) -> false;
%to_boolean(<<"Yes",_/binary>>) -> true;
%to_boolean(<<"yes",_/binary>>) -> true;
%to_boolean(<<"ja",_/binary>>) -> true;
%to_boolean(<<"TBD",_/binary>>) -> true;
to_boolean(_) -> true. %% srsly, wtf

to_status(<<"new">>=V) -> V;
to_status(<<"open">>=V) -> V;
to_status(<<"closed">>=V) -> V;
to_status(<<"cancelled">>=V) -> V;
to_status(<<"solved">>=V) -> V;
to_status(<<"promoted">>=V) -> V;
to_status(<<"reviewed">>=V) -> V;
to_status(<<"deferred">>=V) -> V;
to_status(<<"cancelled",_/binary>>) -> <<"cancelled">>.

to_priority(<<"1">>=V) -> V;
to_priority(<<"2">>=V) -> V;
to_priority(<<"3">>=V) -> V;
to_priority(<<"4">>=V) -> V;
to_priority(_) -> <<"4">>.

to_type(<<"bug">>=V) -> V;
to_type(<<"bug",_/binary>>) -> <<"bug">>;
to_type(<<"job">>=V) -> V;
to_type(<<"request">>=V) -> V;
to_type(<<"feature">>) -> <<"request">>;
to_type(<<"jobrequest">>) -> <<"request">>;
to_type(<<"other">>) -> <<"job">>;
to_type(_) -> <<"job">>.

to_source(<<"internal">>=V) -> V;
to_source(<<"internal",_/binary>>) -> <<"internal">>;
to_source(<<"external">>=V) -> V;
to_source(<<"opensource">>=V) -> V;
to_source(_) -> <<"external">>.

to_datetime(V) -> %% <<"2013-10-15 12:31:33">>
    V.

to_otp_releases(V) ->
    [to_otp_release(Rel)||Rel<-split(V)].

to_otp_release(V) ->
    list_to_binary(string:to_upper(binary_to_list(V))).

to_applications(V) ->
    [to_application(strip(App))||App <- split(V)].

to_application(<<"otp">>=V) -> V;
to_application(<<"new">>=V) -> V;
to_application(<<"documentation">>=V) -> V;
to_application(<<"erts">>=V) -> V;
to_application(<<"kernel">>=V) -> V;
to_application(<<"stdlib">>=V) -> V;

to_application(<<"appmon">>=V) -> V;
to_application(<<"asn1">>=V) -> V;
to_application(<<"common_test">>=V) -> V;
to_application(<<"compiler">>=V) -> V;
to_application(<<"comte">>=V) -> V;
to_application(<<"cos",_/binary>>=V) -> V;
to_application(<<"crypto">>=V) -> V;
to_application(<<"debugger">>=V) -> V;
to_application(<<"dialyzer">>=V) -> V;
to_application(<<"diameter">>=V) -> V;
to_application(<<"docbuilder">>=V) -> V;
to_application(<<"edoc">>=V) -> V;
to_application(<<"eldap">>=V) -> V;
to_application(<<"eunit">>=V) -> V;
to_application(<<"erl_docgen">>=V) -> V;
to_application(<<"erl_interface">>=V) -> V;
to_application(<<"et">>=V) -> V;
to_application(<<"gs">>=V) -> V;
to_application(<<"hipe">>=V) -> V;
to_application(<<"ic">>=V) -> V;
to_application(<<"inets">>=V) -> V;
to_application(<<"inviso">>=V) -> V;
to_application(<<"itc">>=V) -> V;
to_application(<<"jinterface">>=V) -> V;
to_application(<<"megaco">>=V) -> V;
to_application(<<"mnesia">>=V) -> V;
to_application(<<"mnesia_session">>=V) -> V;
to_application(<<"netconf">>=V) -> V;
to_application(<<"observer">>=V) -> V;
to_application(<<"odbc">>=V) -> V;
to_application(<<"old_docbuilder">>=V) -> V;
to_application(<<"orber">>=V) -> V;
to_application(<<"os_mon">>=V) -> V;
to_application(<<"ose">>=V) -> V;
to_application(<<"otp_mibs">>=V) -> V;
to_application(<<"parsetools">>=V) -> V;
to_application(<<"percept">>=V) -> V;
to_application(<<"pman">>=V) -> V;
to_application(<<"public_key">>=V) -> V;
to_application(<<"reltool">>=V) -> V;
to_application(<<"runtime_tools">>=V) -> V;
to_application(<<"safe">>=V) -> V;
to_application(<<"sasl">>=V) -> V;
to_application(<<"snmp">>=V) -> V;
to_application(<<"ssh">>=V) -> V;
to_application(<<"ssl">>=V) -> V;
to_application(<<"syntax_tools">>=V) -> V;
to_application(<<"test_server">>=V) -> V;
to_application(<<"ticket">>=V) -> V;
to_application(<<"toolbar">>=V) -> V;
to_application(<<"tools">>=V) -> V;
to_application(<<"typer">>=V) -> V;
to_application(<<"tv">>=V) -> V;
to_application(<<"xmerl">>=V) -> V;
to_application(<<"webtool">>=V) -> V;
to_application(<<"wx">>=V) -> V;

%% old or defunct
to_application(<<"nessie">>=V) -> V;
to_application(<<"jive">>=V) -> V;
to_application(<<"mnemosyne">>=V) -> V;

%% fixups
to_application(<<"C-diameter">>) -> <<"c_diameter">>;
to_application(<<"Cos",V/binary>>) -> to_application(<<"cos",V/binary>>);
to_application(<<"emulator",_/binary>>) -> <<"erts">>;
to_application(<<"ERTS",_/binary>>) -> <<"erts">>;
to_application(<<"compiler",_/binary>>) -> <<"compiler">>;
to_application(<<"odbc",_/binary>>) -> <<"odbc">>;
to_application(<<"inets",_/binary>>) -> <<"inets">>;
to_application(<<"snmp",_/binary>>) -> <<"snmp">>;
to_application(<<"ssl",_/binary>>) -> <<"ssl">>;
to_application(<<"emacs",_/binary>>) -> <<"tools">>;
to_application(<<"xref",_/binary>>) -> <<"tools">>;
to_application(<<"tools",_/binary>>) -> <<"tools">>;
to_application(<<"doc">>) -> <<"documentation">>;
to_application(<<"Docu",_/binary>>) -> <<"documentation">>;
to_application(App) -> App.

to_assigned_to(V) ->
    %io:format("~p~n", [V]),
    V.

to_created_by(V) ->
    %io:format("~p~n", [V]),
    V.

to_fixed_in(V) ->
    [to_version(Vsn) || Vsn <- split(V)].

to_version(<<"erl_",_/binary>>=Patch) -> Patch;
to_version(AppVsn) ->
    case [Item || Item <- binary:split(AppVsn,<<"-">>,[global]),Item =/= <<>>] of
	[<<"C">>,App0,Vsn] ->
	    App = to_application(<<"C-",App0/binary>>),
	    <<App/binary,"-",Vsn/binary>>;
	[App0,Vsn] ->
	    App = to_application(App0),
	    <<App/binary,"-",Vsn/binary>>;
	_ ->
	    to_otp_release(AppVsn)
    end.

%% <<"$Date: 2014/04/03 13:08:22 $ $Author: rickard $">>
to_author_datetime(V) ->
    RE = <<"(Date: ([0-9]+)/([0-9]+)/([0-9]+) ([0-9]+:[0-9]+:[0-9]+).+Author: ([a-zA-Z0-9]+))"
           "|(Date: ([0-9]+)/([0-9]+)/([0-9]+) ([0-9]+:[0-9]+:[0-9]+))">>,
    case re:run(V, RE, [global,{capture, all_but_first, binary}]) of
	{match,[[_,Y,Mo,D,Time,Author]]} ->
	    {<<Y/binary,"-",Mo/binary,"-",D/binary," ",Time/binary>>, Author};
	{match,[[_,_,_,_,_,_,_,Y,Mo,D,Time]]} -> %% no author
	    {<<Y/binary,"-",Mo/binary,"-",D/binary," ",Time/binary>>, <<>>};
	_ ->
	    {<<>>,<<>>}
    end.

%% test

test() ->
    Ctx = #ctx{path=Path} = context([]),
    {ok,Ls} = file:list_dir(Path),
    _ = [from_id(list_to_binary(Id),Ctx)|| "OTP-"++_ = Id <- Ls],
    ok.
