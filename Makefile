
.PHONY: reload
reload: public
		./reload-safari.js

.PHONY: clean
clean:
		rm -rf build public

public: build/index.html
		rm -rf $@
		mkdir -p $@.new
		cp $< $@.new
		mv $@.new $@

build/index.html: index.html.m4 build/mini.js puzzles.json
		mkdir -p build
		m4 -Ibuild $< > $@.new
		mv $@.new $@

build/mini.js: build/main.js
		mkdir -p build
		#closure-compiler --js_output_file $@.new $^
		cp $^ $@.new
		mv $@.new $@

build/main.js: elm.json $(wildcard src/*.elm)
		mkdir -p build
		elm make --output=$(dir $@)new.$(notdir $@) src/App.elm
		mv $(dir $@)new.$(notdir $@) $@

.PHONY: scramble
scramble:
		(cd swift-tools && swift run scramble ../puzzles.json) > puzzles.json.new
		mv puzzles.json.new puzzles.json

