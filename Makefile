
.PHONY: reload
reload: build/index.html
		./reload-safari.js

.PHONY: clean
clean:
		rm -rf build

public: build/index.html
		rm -rf $@
		mkdir -p $@.new
		cp $< $@.new
		mv $@.new $@

build/index.html: index.html.m4 build/mini.js puzzles.json
		m4 -Ibuild $< > $@.new
		mv $@.new $@

build/mini.js: build/main.js
		closure-compiler --js_output_file $@.new $^
		#cp $^ $@.new
		mv $@.new $@

build/main.js: elm.json $(wildcard src/*.elm)
		elm make --output=$(dir $@)new.$(notdir $@) src/App.elm
		mv $(dir $@)new.$(notdir $@) $@

build:
		mkdir -p build

.PHONY: scramble
scramble:
		swift run scramble puzzles.json > puzzles.json.new
		mv puzzles.json.new puzzles.json
