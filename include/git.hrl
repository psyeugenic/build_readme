
-record(commit, {
	commit,
	parent,
	tree,
	author = {undefined,undefined,undefined},
	committer = {undefined, undefined,undefined},
	subject,
	body,
	stats
    }).

-record(topic, {
	name,
	merged_by,
	tickets,
	applications,
	commits
    }).


