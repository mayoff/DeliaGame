
builddir := ../build

.PHONY: reload
reload: $(builddir)/index.html
		./reload-safari.js

.PHONY: clean
clean:
		rm -rf $(builddir)

$(builddir)/index.html: index.html.m4 $(builddir)/mini.js puzzles.json
		m4 -I$(builddir) $< > $@.new
		mv $@.new $@

$(builddir)/mini.js: $(builddir)/main.js
		closure-compiler --js_output_file $@.new $^
		mv $@.new $@

$(builddir)/main.js: elm.json $(wildcard src/*.elm)
		elm make --output=$(dir $@)new.$(notdir $@) src/Main.elm
		mv $(dir $@)new.$(notdir $@) $@

$(builddir):
		mkdir -p $(builddir)

.PHONY: scramble
scramble:
		swift run scramble puzzles.json > puzzles.json.new
		mv puzzles.json.new puzzles.json
