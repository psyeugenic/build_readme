%% Copyright (C) 2012 Björn-Egil Dahlberg
%%
%% File:    otp_build_readme_md.erl
%% Author:  Björn-Egil Dahlberg
%% Created: 2012-08-23

-module(build_readme).

-export([
	main/1
    ]).

-include_lib("build_readme/include/git.hrl").

-record(opts, {
	range = "HEAD",
	repo = ".",
	stats = lines, %% none | percent | lines
	maxlen = 0, %% topic length in output
	filter = [
	    <<"dev">>,
	    <<"maint">>,
	    <<"master">>,
	    <<"upstream/maint">>
	]
    }).


parse_opts([], Opts) -> Opts;
parse_opts(["-s="++Type|Args],Opts) -> parse_opts(Args,Opts#opts{ stats = list_to_atom(Type) });
parse_opts(["-r="++Repo|Args],Opts) -> parse_opts(Args,Opts#opts{ repo=Repo });
parse_opts(["-filter="++Filter|Args],Opts) ->
    parse_opts(Args,Opts#opts{
	    filter = [list_to_binary(F)||F <- string:tokens(Filter,",")]
	});
parse_opts(["-"++Opt|_],_) -> erlang:error({unrecognized_option, Opt});
parse_opts([To],Opts) -> Opts#opts{ range = To ++ "..HEAD" };
parse_opts([To,From],Opts) -> Opts#opts{ range = To ++ ".." ++From }.

main(Args) ->
    try
	Opts = parse_opts(Args,#opts{}),
	dump(collect(parse_opts(Args,Opts)),Opts)
    catch 
	E:R ->
	    system:format_error(E,R)
    end.

collect(#opts{ repo=Repo, range=Range, filter=Filter }) ->
    Branches = git:get_branches(Repo, Range, Filter),
    Topics = git:get_topics(Repo, Branches),
    %git:check_topics(Repo,Topics,Range),
    Topics.

dump(Topics,Opts) ->
    MaxLen = lists:foldl(fun
	    (#topic{name=Name},Len) ->
		max(Len,byte_size(Name))
	end, 0, Topics),
    io:put_chars(topics(Topics,Opts#opts{maxlen=MaxLen})).

topics([],_) -> [];
topics([T|Ts],Opts) -> [topic(T,Opts)|topics(Ts,Opts)].

topic(#topic{
	name = Name,
	merged_by={Committer,_,When},
	tickets = Tickets,
	applications=Apps
    }=Topic,#opts{maxlen=Len}=Opts) ->
    Preamble = header(Len,""),
    [
	spaces(Len-byte_size(Name)), Name, " | merged by ", s(Committer), ", ",When, "\n",
	header(Len,"applications:"), format_applications(Preamble,Apps), "\n",
	header(Len,"tickets:"), format_applications(Preamble,Tickets), "\n",
	header(Len,"authors:"), format_authors(Preamble,authors_from_topic(Topic),Opts), "\n",
	header(Len,"subjects:"), format_subjects(Preamble,subjects_from_topic(Topic)), "\n",
	"\n"
    ].

format_authors(P,Authors,Opts) ->
    Ds = [D||{_Name,{_A,D}}<-Authors],
    As = [A||{_Name,{A,_D}}<-Authors],
    format_authors(P,Authors,{lists:sum(As),lists:sum(Ds)},Opts).
format_authors(_,[],_,_) -> [];
format_authors(_,[Author],T,Opts) ->
    [format_author(Author,T,Opts)];
format_authors(P,[Author|Authors],T,Opts) ->
    A = format_author(Author,T,Opts),
    [A,"\n",P|format_authors(P,Authors,T,Opts)].

format_author({Name,{A,D}},{Ta,Td},#opts{stats=Type}) ->
    case {Type,Ta+Td} of
	{lines, _} ->
	    io_lib:format("~s (~w lines)", [Name,A+D]);
	{percent, N} when N > 0 ->
	    R = (A+D)/(N),
	    io_lib:format("~s (~.2f %)", [Name,R*100]);
	_ ->
	    io_lib:format("~s", [Name])
    end.

format_applications(P,Apps) ->
    format_applications(P,Apps,0,70).
format_applications(_,[],_,_) -> [];
format_applications(_,[App],_,_) -> [App];
format_applications(P,[App|Apps],I,N) when 1+byte_size(App) + I < N ->
    [App,", "|format_applications(P,Apps,2 + I+byte_size(App),N)];
format_applications(P,[App|Apps],_,N) ->
    [App,",\n",P|format_applications(P,Apps,0,N)].


format_subjects(_,[]) -> [];
format_subjects(P,[T]) -> ["* ",format_subject(P,T)];
format_subjects(P,[T|Ts]) ->
    ["* ",format_subject(P,T),"\n",P|format_subjects(P,Ts)].

format_subject(P,S) ->
    N = byte_size(S),
    Limit = 72,
    case N < Limit of
	true -> S;
	false ->
	    Bs = binary:split(S,<<" ">>, [global]),
	    {Txt,_}=lists:mapfoldl(fun
		    (B,I) when I + byte_size(B) < Limit -> 
			{[B, <<" ">>], I + byte_size(B)};
		    (B,_) ->
			{["\n",P,<<"  ">>, B, <<" ">>],byte_size(B) + 1}
		end, 0, Bs),
	    Txt
    end.





subjects_from_topic(#topic{commits=Cs}) ->
    [Subject||#commit{subject=Subject} <- Cs].

authors_from_topic(#topic{ commits = Cs }) ->
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

header(Len,Topic) ->
    [spaces(Len), format(" | ~13s ", [Topic])].


format(F,Ts) -> io_lib:format(F,Ts).
s(T) -> format("~s",[T]).
spaces(N) -> lists:duplicate(N, 32).
