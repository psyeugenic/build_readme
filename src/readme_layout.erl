%%
%% Copyright (C) 2014 Björn-Egil Dahlberg
%%
%% File:    readme_layout.erl
%% Author:  Björn-Egil Dahlberg
%% Created: 2014-04-08
%%

-module(readme_layout).

-include_lib("build_readme/include/git.hrl").
-include_lib("build_readme/include/tickets.hrl").

-export([from_release_note/1,format/1,t/0,t/1]).
-export([header/2]).

-record(t_cdata,{ val }).
-record(t_em,{ val }).
-record(t_italic,{ val }).
-record(t_item,{ val }).
-record(t_p,{ val }).
-record(t_code,{ val }).
-record(t_seealso,{ val,ref }).

-record(t_h,{val= <<>>,body=[]}).


t() ->
    t(<<"OTP-11616">>).
t(Id) ->
    Ctx = tickets:context([]),
    %#ticket{release_note=Note} = tickets:from_id(<<"OTP-11593">>,Ctx),
    #ticket{release_note=Note} = tickets:from_id(Id,Ctx),
    Tree = from_release_note(Note),
    io:format(" Raw:~n~p~n",[Note]),
    io:format("Tree:~n~p~n",[Tree]),
    io:format("Text:~n~s~n",[
	    format([#t_h{val=Id,body=Tree}])
	]),
    ok.


header(Val,Body) -> #t_h{val=Val,body=Body}.

from_release_note(Note) when is_binary(Note) ->
    Tree = retag_container(Note),
    merge_binaries(Tree).


retag_container(<<>>) -> [];
retag_container(<<"\n",R/binary>>) ->
    retag_container(R);
retag_container(<<"<",_/binary>>=Note) ->
    retag(Note);
retag_container(Note) ->
    {V,C} = cut_paragraph(Note),
    [#t_p{val=retag(strip_whitespace(V))}|retag_container(C)].

retag(<<"<![CDATA[", R/binary>>) ->
    {V,C} = cut_with(R, <<"]]>">>),
    [#t_cdata{val=V}|retag(C)];
retag(<<"<em>",R/binary>>) ->
    {V,C} = cut_with(R, <<"</em>">>),
    [#t_em{val=retag(V)}|retag(C)];
retag(<<"<b>",R/binary>>) ->
    {V,C} = cut_with(R, <<"</b>">>),
    [#t_em{val=retag(V)}|retag(C)];
retag(<<"<item>", R/binary>>) ->
    {V,C} = cut_with(R, <<"</item>">>),
    [#t_item{val=retag(V)}|retag_container(C)];
retag(<<"<tag>", R/binary>>) ->
    {V,C} = cut_with(R, <<"</tag>">>),
    [#t_item{val=retag(V)}|retag_container(C)];
retag(<<"<i>", R/binary>>) ->
    {V,C} = cut_with(R, <<"</i>">>),
    [#t_italic{val=retag(V)}|retag(C)];
retag(<<"<p>", R/binary>>) ->
    {V,C} = cut_with(R, <<"</p>">>),
    [#t_p{val=retag(strip_whitespace(V))}|retag(C)];
retag(<<"<br", R/binary>>) ->
    {_,C0} = cut_with(R, <<">">>),
    [#t_p{val=[]}|retag(C0)];
retag(<<"<c>", R/binary>>) ->
    {V,C} = cut_with(R, <<"</c>">>),
    [#t_code{val=retag(V)}|retag(C)];
retag(<<"<code>", R/binary>>) ->
    {V,C} = cut_with(R, <<"</code>">>),
    [#t_code{val=retag(V)}|retag(C)];
retag(<<"<C>", R/binary>>) ->
    {V,C} = cut_with(R, <<"</C>">>),
    [#t_code{val=retag(V)}|retag(C)];
retag(<<"<seealso", R/binary>>) ->
    {Ref,C0} = cut_with(R, <<">">>),
    {V,C} = cut_with(C0, <<"</seealso>">>),
    [#t_seealso{val=retag(V),ref=Ref}|retag(C)];
%% probably binary
retag(<<"<<", R/binary>>) ->
    {V,C} = cut_with(R, <<">>">>),
    [<<"<<",V/binary,">>">>|retag(C)];
%% clear other tags
retag(<<"<", R/binary>>) ->
    {_V,C} = cut_with(R, <<">">>),
    io:format("cut ~p~n", [_V]),
    retag_container(C);

retag(<<>>) -> [];
retag(B) ->
    {V,R} = collect_until_tag(B),
    [V|retag_container(R)].

collect_until_tag(<<>>) ->
    {<<>>,<<>>};
collect_until_tag(<<"&lt;",R/binary>>) ->
    {Res,C} = collect_until_tag(R),
    {<<"<",Res/binary>>,C};
collect_until_tag(<<"&gt;",R/binary>>) ->
    {Res,C} = collect_until_tag(R),
    {<<">",Res/binary>>, C};
collect_until_tag(<<"<",_/binary>>=R) ->
    {<<>>,R};
collect_until_tag(<<V,R/binary>>) ->
    {Res,C} = collect_until_tag(R),
    {<<V,Res/binary>>, C}.

cut_paragraph(Note) ->
    cut_with(Note,<<"\n\n">>).

strip_whitespace(B) ->
    strip_trailing_whitespace(strip_leading_whitespace(B)).

strip_leading_whitespace(<<>>) -> <<>>;
strip_leading_whitespace(<<V,C/binary>>=B) ->
    case is_whitespace(V) of
	true -> strip_leading_whitespace(C);
	false -> B
    end.

strip_trailing_whitespace(<<>>=B) -> B;
strip_trailing_whitespace(B) ->
    strip_trailing_whitespace(byte_size(B)-1,B).

strip_trailing_whitespace(N,B) -> 
    <<B1:N/binary,V>> = B,
    case is_whitespace(V) of
	true -> strip_trailing_whitespace(B1);
	false -> B
    end.


cut_with(<<>>,_) -> {<<>>,<<>>};
cut_with(B,Tag) ->
    Sz = byte_size(Tag),
    case B of
	<<Tag:Sz/binary, R/binary>> -> {<<>>, R};
	<<V,R/binary>> ->
	    {Res,C} = cut_with(R,Tag),
	    {<<V,Res/binary>>,C}
    end.



% - merge binaries in tree
% [[<<"wat ">>],<<"wazzup">>] -> [<<"wat wazzup">>]
merge_binaries(#t_p{val=V0}) ->
    V1 = case merge_binaries(V0) of
	<<" ",R/binary>> -> R;
	R -> R
    end,
    #t_p{val=V1};
merge_binaries({Tag,Val}) -> {Tag,merge_binaries(Val)};
merge_binaries(#t_seealso{val=V}=T) -> T#t_seealso{ val=merge_binaries(V) };
%% in list
merge_binaries([#t_item{}=V1,#t_item{}=V2|Vs]) ->
    [merge_binaries(V1)|merge_binaries([V2|Vs])];
merge_binaries([#t_item{}=V1,_,#t_item{}=V2|Vs]) ->
    %% drop non-items between items (only <<whitespaces>>)
    [merge_binaries(V1)|merge_binaries([V2|Vs])];
merge_binaries([[B]|T]) when is_binary(B) -> merge_binaries([B|T]);
merge_binaries([B1,B2|Next]) when is_binary(B1), is_binary(B2) ->
    merge_binaries([<<B1/binary,B2/binary>>|Next]);
merge_binaries([V|Vs]) -> [merge_binaries(V)|merge_binaries(Vs)];
merge_binaries(V) when is_binary(V) ->
    whitespace_reformat(V);
merge_binaries([]) -> [].

%% leave one whitespace
whitespace_reformat(<<Ws,R/binary>>=B) ->
    case is_whitespace(Ws) of
	true ->
	    V = reformat_ws(R),
	    <<" ", V/binary>>;
	false -> reformat_ws(B)
    end.

reformat_ws(<<>>) -> <<>>;
reformat_ws(<<Ws,R/binary>>=B) ->
    case is_whitespace(Ws) of
	true -> reformat_ws(R);
	false -> reformat_word(B)
    end.

reformat_word(<<>>) -> <<>>;
reformat_word(<<Ws,R/binary>>) ->
    case is_whitespace(Ws) of
	true ->
	    V = reformat_ws(R),
	    <<" ", V/binary>>;
	false ->
	    C = reformat_word(R),
	    <<Ws, C/binary>>
    end.

is_whitespace(V) ->
    case V of
	32 -> true;
	13 -> true;
	10 -> true;
	9  -> true;
	_  -> false
    end.



%% format(Tree) -> iolist().
-record(s,{
	i = 0,
	c = 0,
	h = 1,   %% heading level
	no_p=false,
	breakable=true
    }).

format(T) ->
    {Res,_} = format_md(T,#s{}),
    Res.

format_md([],S) -> {[],S};
format_md([I|Is],S0) ->
    {ResI,S1} = format_md(I,S0),
    {ResIs,S2} = format_md(Is,S1),
    {[ResI|ResIs],S2};

format_md(#t_p{val=V},#s{no_p=false}=S0) ->
    {Res,S1}=format_md(V,S0#s{c=0,i=0}),
    {[<<"\n">>,Res,<<"\n">>],S1#s{c=0,i=0}};
format_md(#t_p{val=V},S0) ->
    format_md(V,S0);

format_md(#t_code{val=V},#s{c=C0}=S0) ->
    {Res,#s{c=C1}=S1} = format_md(V,S0#s{c=C0+1,breakable=false}),
    {[<<"`">>,Res,<<"`">>],S1#s{c=C1+1,breakable=true}};

format_md(#t_em{val=V},#s{c=C0}=S0) ->
    {Res,#s{c=C1}=S1} = format_md(V,S0#s{c=C0+1}),
    {[<<"*">>,Res,<<"*">>],S1#s{c=C1+1}};

format_md(#t_seealso{val=V},#s{c=C0}=S0) ->
    {Res,#s{c=C1}=S1} = format_md(V,S0#s{c=C0+0}),
    {[Res],S1#s{c=C1+0}};

format_md(#t_item{val=V},#s{i=I0}=S0) ->
    {Res,S1} = format_md(V,S0#s{c=I0+4,i=I0+4,no_p=true}),
    Item = [<<"\n\n">>,
	ident(I0), <<"  * ">>,
	Res
    ],
    {Item, S1#s{i=I0,c=I0,no_p=false}};

format_md(#t_h{val=V,body=Body},S0) ->
    {HeadRes,_} = format_md(V,S0#s{ breakable=false }),
    {BodyRes,S1} = format_md(Body,S0#s{c=0,i=0}),
    Res = [
	<<"\n">>,<<"### ">>,HeadRes,<<" ###">>,<<"\n\n">>,
	BodyRes,<<"\n">>
    ],
    {Res,S1#s{c=0,i=0}};

format_md(B,S) when is_binary(B) ->
    format_binary(B,S).

format_binary(<<>>, S) -> {[],S};
format_binary(B,#s{i=I}=S0) ->
    case break_binary(B,S0) of
	{L,<<>>,S1} -> {[L],S1};
	{L,R,S1} ->
	    {Rest,S2} = format_binary(R,S1#s{c=I}),
	    {[L,<<"\n">>,ident(I)|Rest],S2}
    end.


break_binary(<<>>,S) -> {<<>>,<<>>,S};
break_binary(<<V,R/binary>>,#s{c=C,breakable=true}=S) when C > 78 ->
    case is_whitespace(V) of
	true -> {<<>>,R,S};
	false ->
	    {B,Rest,Sr} = break_binary(R,S#s{c=C+1}),
	    {<<V,B/binary>>, Rest, Sr}
    end;
break_binary(<<V,R/binary>>,#s{c=C}=S) ->
    {B,Rest,Sr} = break_binary(R,S#s{c=C+1}),
    {<<V,B/binary>>, Rest, Sr}.


ident(0) -> <<>>;
ident(I) ->
    list_to_binary(lists:duplicate(I,32)).
