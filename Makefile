REBAR=./rebar

build_readme: $(REBAR) src/build_readme.erl src/git.erl src/system.erl src/tickets.erl src/readme_layout.erl
	$(REBAR) compile
	$(REBAR) escriptize

$(REBAR):
	wget --output-document=$(REBAR) http://cloud.github.com/downloads/basho/rebar/rebar && chmod u+x $(REBAR)


