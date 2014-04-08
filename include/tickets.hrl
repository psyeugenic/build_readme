%%
%% Copyright (C) 2014 Björn-Egil Dahlberg
%%
%% File:    tickets.hrl
%% Author:  Björn-Egil Dahlberg
%% Created: 2014-04-07
%%

-record(ticket,{
	id           :: binary(),
	label        :: binary(),
	status       :: binary(), %%<<"solved">> | <<"new">> | <<"open">> | <<"cancelled">>,
	priority     :: binary(), %% <<"1">> | <<"2">> | <<"3">> | <<"4">>,
	type         :: binary(), %% <<"bug">> | <<"request">> | <<"job">>
	source       :: binary(), %% <<"internal">> | <<"external">> | <<"opensource">>
	highlight    :: boolean(),
	incompatible :: boolean(),
	otp_releases :: [binary()],
	applications :: [binary()],
	planned_for  :: [binary()], %% [otprel() | <<"app-vsn">>
	fixed_in     :: [binary()], %% [otprel() | <<"app-vsn">>
	assigned_to  :: binary(),
	release_note :: binary(),
	created_by   :: binary(),
	created_on   :: binary(),  %% <<"2013-10-15 12:31:33">>
	updated_by   :: binary(),  
	updated_on   :: binary()   %% <<"2013-10-15 12:31:33">>
    }).
