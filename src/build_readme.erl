%% Copyright (C) 2012 Björn-Egil Dahlberg
%%
%% File:    otp_build_readme_md.erl
%% Author:  Björn-Egil Dahlberg
%% Created: 2012-08-23

-module(build_readme).

-export([main/1]).

-include_lib("build_readme/include/git.hrl").
-include_lib("build_readme/include/tickets.hrl").

-record(versions,{
	otp = <<>>,
	applications = []
    }).

-record(opts, {
	range = "HEAD",
	repo = ".",
	upcoming,
	stats = lines, %% none | percent | lines
	maxlen = 0, %% topic length in output
	filter = [
	    <<"dev">>,
	    <<"maint">>,
	    <<"master">>,
	    <<"upstream/maint">>
	]
    }).


parse_opts(["-h"], _Opts) -> usage();
parse_opts(["-s="++Type|Args],Opts) -> parse_opts(Args,Opts#opts{ stats = list_to_atom(Type) });
parse_opts(["-r="++Repo|Args],Opts) -> parse_opts(Args,Opts#opts{ repo=Repo });
parse_opts(["-versions="++File|Args], Opts) -> parse_opts(Args,Opts#opts{ upcoming=File });
parse_opts(["-filter="++Filter|Args],Opts) ->
    parse_opts(Args,Opts#opts{
	    filter = [list_to_binary(F)||F <- string:tokens(Filter,",")]
	});
parse_opts(["-"++Opt|_],_) -> erlang:error({unrecognized_option, Opt});
parse_opts([To],Opts) -> Opts#opts{ range = To ++ "..HEAD" };
parse_opts([To,From],Opts) -> Opts#opts{ range = To ++ ".." ++From }.

usage() ->
    io:put_chars([
	    "build_readme [-h] [-s=<stats>] [-r=<repository>] [-filter=<branches>] <to> [<from>]\n",
	    "\n"
	    "    -s=<stats> :: none | percent | line, line is default\n"
	    "    -r=<repository> :: path(), . is default\n",
	    "    -filter=<branches> :: branch1,branch2,.. : branches that should be not included in the readme\n"
	    "    -h   this usage text\n"
	]).

main([]) -> usage();
main(Args) ->
    try
	Opts = parse_opts(Args,#opts{}),
	Topics = topics(Opts),
	TopicTree = ticket_tree_from_topics(Topics),
	Vsns = upcoming_versions(Opts),
	VsnTree = ticket_tree_from_versions(Vsns),
	%{Items,_Ws} = aggregate(VsnTree,TopicTree),
	dump_versions(VsnTree),
	%dump(Items),
	ok
    catch 
	E:R ->
	    system:format_error(E,R)
    end.

topics(#opts{ repo=Repo, range=Range, filter=Filter }) ->
    Branches = git:get_branches(Repo, Range, Filter),
    git:get_topics(Repo, Branches).

ticket_tree_from_topics(Topics) ->
    lists:foldl(fun(#topic{tickets=Tickets}=Topic,Ti1) ->
		lists:foldl(fun(Id,Ti2) ->
			    case gb_trees:lookup(Id,Ti2) of
				none -> gb_trees:enter(Id,[Topic],Ti2);
				{value,Vs} -> gb_trees:enter(Id,[Topic|Vs],Ti2)
			    end
		    end, Ti1,Tickets)
	end, gb_trees:empty(), Topics).

ticket_tree_from_versions(#versions{otp=Otp,applications=Apps}) ->
    ticket_tree_from_versions([Otp|lists:sort(Apps)]);
ticket_tree_from_versions(Vsns) ->
    Ctx0 = tickets:context([]),
    Ctx1 = tickets:load_mapping(Ctx0),
    ticket_tree_from_versions(Vsns,Ctx1,gb_trees:empty()).

ticket_tree_from_versions([],_,T) -> T;
ticket_tree_from_versions([Vsn|Vsns],Ctx,T) ->
    T1 = lists:foldl(fun(Id,Ti1) ->
		case gb_trees:lookup(Id,Ti1) of
		    none -> gb_trees:enter(Id,tickets:from_id(Id,Ctx),Ti1);
		    {value,_} -> Ti1
		end
	end,T,tickets:from_version(Vsn,Ctx)),
    ticket_tree_from_versions(Vsns,Ctx,T1).

 
%% upcoming versions
upcoming_versions(#opts{upcoming=File}) when is_list(File) ->
    {ok,B}=file:read_file(File),
    lists:foldl(fun
	    (<<>>,V) -> V;
	    (<<"#",_/binary>>, V) -> V;
	    (<<"otp-",_/binary>>=Otp,V) ->
		V#versions{ otp=Otp };
	    (AppVsn,#versions{ applications = Apps }=V) ->
		V#versions{ applications = [tickets:to_version(AppVsn)|Apps] }
	end,#versions{},binary:split(B, [<<"\n">>], [global])).

-record(item,{
	%% from git
	merged_by=[],
	authors = [],
	%% from ticket
	applications=[],
	release_note= <<>>
    }).


dump_versions(VsnTree) ->
    Ts = gb_trees:keys(VsnTree),
    Ctx = tickets:context([]),
    S = dump_versions(Ts,Ctx),
    file:write_file("wat.md", iolist_to_binary(S)),
    ok.

dump_versions([],_) -> [];
dump_versions([T|Ts],Ctx) ->
    case tickets:from_id(T,Ctx) of
	#ticket{ status=Status } when Status =:= <<"cancelled">> ->
	    %% check not merged
	    dump_versions(Ts,Ctx);
	#ticket{ type=Type } when Type =:= <<"job">> ->
	    dump_versions(Ts,Ctx);
	#ticket{release_note=Note} ->
	    Nl = readme_layout:from_release_note(Note),
	    [readme_layout:format(
		    readme_layout:header(T,Nl)
		)|dump_versions(Ts,Ctx)]
    end.


aggregate(VsnTree,TopicTree) ->
    %% collect all tickets
    Tickets = lists:usort(gb_trees:keys(VsnTree) ++ gb_trees:keys(TopicTree)),
    aggregate(Tickets,VsnTree,TopicTree,[],[]).

aggregate([],_,_,Res,Ws) -> {Res,Ws};
aggregate([Id|Ids],Vt,Tt,Res,Ws) ->
    case gb_trees:lookup(Id,Vt) of
	none ->
	    aggregate(Ids,Vt,Tt,Res,[{Id,no_version}|Ws]);

	{value, Ticket} ->
	    case Ticket of
		#ticket{ status=Status } when Status =:= <<"cancelled">> ->
		    %% check not merged
		    aggregate(Ids,Vt,Tt,Res,Ws);
		#ticket{ type=Type } when Type =:= <<"job">> ->
		    %% check if release note exists
		    aggregate(Ids,Vt,Tt,Res,Ws);
		Ticket ->
		    Item = aggregate_item(Ticket,Tt),
		    aggregate(Ids,Vt,Tt,[{Id,Item}|Res],Ws)
	    end
    end.

aggregate_item(#ticket{ id=Id,fixed_in=Apps,release_note=Note },Tt) ->
    case gb_trees:lookup(Id,Tt) of
	none -> #item{
		applications=Apps,
		release_note=Note
	    };
	{value, Topics} ->
	    Merger = lists:usort([merger_from_topic(Topic)||Topic<-Topics]),
	    Authors = lists:usort(lists:flatten(
		    [[Name||{Name,_}<-authors_from_topic(Topic)]||Topic<-Topics]
		)),
	    #item{
		applications=Apps,
		release_note=Note,
		merged_by=Merger,
		authors=Authors
	    }
    end.

merger_from_topic(#topic{ merged_by={Name,_,_} }) -> Name.

authors_from_topic(#topic{ commits=Cs }) ->
    authors_from_commits(Cs).

authors_from_commits(Cs) -> 
    gb_trees:to_list(authors_from_commits(Cs,gb_trees:empty())).

authors_from_commits([],T) -> T;
authors_from_commits([#commit{author={Name,_,_},stats={{A,D},_}}|Cs],T) ->
    T1 = case gb_trees:lookup(Name,T) of
	none -> gb_trees:enter(Name, {A,D}, T);
	{value, {A0,D0}} -> gb_trees:enter(Name, {A+A0,D+D0}, T)
    end,
    authors_from_commits(Cs,T1).


%% debug
dump(Items) ->
    file:write_file("wat.md",list_to_binary(dump_items(Items))).

dump_items([]) -> [];
dump_items([{Id,#item{
	authors=Authors,
	applications=Apps,
	merged_by=Merger,
	release_note=Note}}|Items]) ->
    [[
	    "### ",Id," ###\n",
	    "* Authors: ", join(lists:sort(Authors),", "), "\n",
	    "* Applications: ", join(lists:sort(Apps),", "), "\n",
	    "\n",
	    format_note(Note),
	    "\n","\n"

    ] | dump_items(Items)].


join([],_) -> [];
join([I],_) -> [I];
join([I|Is],Sep) -> [[I,Sep]|join(Is,Sep)].

format_note(Note0) ->
    Note1 = re:replace(Note0,<<"<c>">>,<<"`">>,[global]),
    Note  = iolist_to_binary(re:replace(iolist_to_binary(Note1),<<"</c>">>,<<"`">>,[global])),
    Ws = [W||W <- binary:split(Note,[<<"\n">>,<<"\t">>,<<" ">>,<<"\n">>],[global]), W =/= <<>>],
    format_note_1(Ws,75).

format_note_1([],_) -> [];
format_note_1([W|Ws],N) ->
    Len = byte_size(W),
    if
	N - Len < 0 ->
	    [<<"\n">>,W|format_note_1(Ws,75)];
	true ->
	    [W,<<" ">>|format_note_1(Ws,N-byte_size(W))]
    end.
