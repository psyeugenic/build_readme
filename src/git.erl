%% Copyright (C) 2012 Björn-Egil Dahlberg
%%
%% File:    otp_git.erl
%% Author:  Björn-Egil Dahlberg
%% Created: 2012-08-23

-module(git).

-export([
	get_branches/2, get_branches/3,
	get_topics/2,
	check_topics/3
    ]).

-include_lib("build_readme/include/git.hrl").


get_branches(Repo, Range) ->
    get_branches(Repo, Range, []).
get_branches(Repo,Range,Filter) ->
    {0,Output} = system:run(Repo, "git", ["log", "--merges", "--oneline", "--first-parent", Range]),
    lists:foldl(fun
	    (Line, Cs) ->
		case capture_line(Line,Filter) of
		    {true, BranchInfo} -> [BranchInfo|Cs];
		    false -> Cs
		end
	end, [], Output).

capture_line(Line, Filter) ->
    Opts = [{capture, all_but_first, binary}],
    HexRx = <<"([0-9a-f]+)">>,
    MergeRx = <<" Merge (branch|remote branch|remote-tracking branch|tag) ">>,
    BranchRx = <<"'([0-9a-zA-Z/\\-_\.]+)'">>,
    case re:run(Line, <<HexRx/binary,MergeRx/binary,BranchRx/binary>>, Opts) of
	{match,[Sha, Type, BranchName]} when Type =/= <<"tag">> ->
	    case lists:member(BranchName,Filter) of
		true -> false;
		false -> {true,{Sha, BranchName}}
	    end;
	_ ->
	    false
    end.

get_topics(_, []) -> [];
get_topics(Repo, [{SHA, Branch}|Branches]) ->
    Range = <<SHA/binary, "^1..", SHA/binary,"^2">>,
    {0, Revs} = system:run(Repo, "git", ["rev-list", "--no-merges", Range]),
    Merge   = commit_to_record(Repo, SHA),
    Commits = [commit_to_record(Repo, C) || C <- Revs],

    Topic = #topic{
	name = Branch,
	tickets = tickets_from_commit(Merge),
	commits = Commits,
	applications = applications_from_commits(Commits),
	merged_by = Merge#commit.committer
    },

    [ Topic | get_topics(Repo, Branches)].
 
commit_to_record(Repo, SHA) ->
    Format = 
	"##commit## %H%n"
	"##tree## %T%n"
	"##parent## %P%n"
	"##author## %aN%n"
	"##author_mail## %aE%n"
	"##author_date## %ar%n"
	"##committer## %cN%n"
	"##committer_mail## %cE%n"
	"##committer_date## %ci%n"
	"##subject## %s%n"
	"##body## %b%n##files## ",

   {0,Output} = system:run(Repo, "git", ["log","--no-walk","--numstat", "--format="++Format, binary_to_list(SHA)]),

    Data = [binary_to_list(B) || B <- Output],
    [ "##commit## "         ++ Commit,
      "##tree## "           ++ Tree,
      "##parent## "         ++ Parent,
      "##author## "         ++ Author,
      "##author_mail## "    ++ AuthorMail,
      "##author_date## "    ++ AuthorDate,
      "##committer## "      ++ Committer,
      "##committer_mail## " ++ CommitterMail,
      "##committer_date## " ++ CommitterDate0
      |SubjectBodyFiles] = Data,

    {_, SubjectT,BodyT, FilesT} = lists:foldl(fun
	    ("##subject## " ++ S, {subject, _, Bo, Fo}) ->
		{subject, S, Bo, Fo};
	    ("##body## " ++ B, {_, So, _, Fo}) ->
		{body, So, B, Fo};
	    ("##files## " ++ F, {body, So, Bo, _}) ->
		{files, So, Bo, F};
	    (Msg, {subject, So, Bo, Fo}) ->
		{subject, So ++ " " ++ Msg, Bo, Fo};
	    ([], {body, So, Bo, Fo}) when Bo =/= [] ->
		{body, So, Bo, Fo};
	    (Msg, {body, So, Bo, Fo}) ->
		{body, So, Bo ++ " " ++ Msg, Fo};
	    (Msg, {files, So, Bo, Fo}) ->
		{files, So, Bo, [Msg|Fo]}
	end, {subject, [], [], []}, SubjectBodyFiles),

    Subject = white_clean_string(SubjectT),
    Body    = BodyT,
    Files   = [File||File <- FilesT, File =/= []],
    Stats   = lists:foldl(fun
	    (Line, {{Atot,Dtot}=Total, Filechanges}) ->
		case string:tokens(Line, "\t") of
		    ["-","-",  File] -> 
			{Total, [{binary, list_to_binary(File)}|Filechanges]};
		    [AddStr, DelStr, File] -> 
			Adds = list_to_integer(AddStr),
			Dels = list_to_integer(DelStr),
			{{Atot + Adds, Dtot + Dels}, [{{Adds,Dels}, list_to_binary(File)}|Filechanges]}
		end
	end, {{0,0}, []}, Files),
      
    {match,CommitterDate}= re:run(CommitterDate0, <<"([0-9]+-[0-9]+-[0-9]+ [0-9]+:[0-9]+:[0-9]+)">>, [{capture, all_but_first, binary}]),
    #commit{
        commit = list_to_binary(Commit),
        tree   = list_to_binary(Tree),
        parent = list_to_binary(Parent),
        author = {
	    list_to_binary(Author),
	    list_to_binary(AuthorMail),
	    list_to_binary(AuthorDate)
	},
        committer = {
	    list_to_binary(Committer),
	    list_to_binary(CommitterMail),
	    list_to_binary(CommitterDate)
	},
	subject = list_to_binary(Subject),
        body    = list_to_binary(Body),
	stats   = Stats
    }.

white_clean_string(S) when is_list(S) ->
    white_clean_string(string:tokens(S, "\n\t "), []).
white_clean_string([],  Out) -> Out;
white_clean_string([T], Out) -> Out ++ T;
white_clean_string([T1, T2|Ts], Out) ->
    white_clean_string([T2|Ts], Out ++ T1 ++ " ").


tickets_from_commit(#commit{ subject = S, body = B }) ->
    T1 = case re:run(S, <<"OTP[-_]([0-9]+)">>, [{capture, all_but_first, binary}]) of
	{match, Ts1} -> Ts1;
	_ -> []
    end,
    T2 = case re:run(B, <<"OTP[-_]([0-9]+)">>, [global,{capture, all_but_first, binary}]) of
	{match, Ts2} -> Ts2;
	_ -> []
    end,
    [<<"OTP-",T/binary>>||T <- lists:usort(lists:flatten([T1, T2]))].


applications_from_commits(Cs) ->
    applications_from_commits(Cs, []).
applications_from_commits([], O) -> lists:usort(O);
applications_from_commits([C|Cs], O) ->
    As = applications_from_commit(C),
    applications_from_commits(Cs, As ++ O).

applications_from_commit(#commit{ stats = {_,Stats} }) ->
    lists:flatten([file_to_application(File) || {_,File} <- Stats]).

file_to_application(<<"lib/", Rest/binary>>) ->
    Match = <<"([0-9a-zA-Z\_-]+)/">>,
    Res = case re:run(Rest, Match, [{capture, all_but_first, binary}]) of
	{match, [App]} -> App;
	_ -> []
    end,
    %io:format("file_to_application: ~p~n", [Res]),
    Res;
file_to_application(<<"erts/epmd", _Rest/binary>>) ->
    <<"epmd">>;
file_to_application(<<"erts/", _Rest/binary>>) ->
    <<"emulator">>;
file_to_application(File) when is_binary(File) ->
    <<"otp">>;
file_to_application(File) ->
    io:format("[nf] file_to_application: ~p~n", [File]),
    File.


check_topics(Repo,Topics, Range) ->
    {0, Revs} = system:run(Repo, "git", ["rev-list", "--no-merges", Range]),
    Revs1 = check_topics(Topics,Revs),
    Commits = [commit_to_record(Repo, C) || C <- Revs1],
    io:format("remaining revs ~p~n", [Commits]),
    ok.

check_topics([], Revs) -> Revs;
check_topics([#topic{commits=Commits}|Topics],Revs0) ->
    Revs1 = check_commits(Commits,Revs0),
    check_topics(Topics,Revs1).

check_commits([], Revs) -> Revs;
check_commits([#commit{commit=Sha}=C|Cs],Revs) ->
    case lists:any(fun(E) -> E =:= Sha end,Revs) of
	true -> check_commits(Cs, Revs -- [Sha]);
	false ->
	    io:format("missing ~p~n", [C]),
	    check_commits(Cs,Revs)
    end.
