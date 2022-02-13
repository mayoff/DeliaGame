
#debug = 1

ifdef debug
buildDir = build.debug
elmOptions = --debug
else
buildDir = build.release
elmOptions = --optimize
endif

.PHONY: reload
reload: public Makefile
	./reload-safari.js

.PHONY: clean
clean:
	rm -rf build.debug build.release

public: $(buildDir)/index.html Makefile
	rm -rf $@
	mkdir -p $@.new
	cp $< $@.new
	mv $@.new $@

$(buildDir)/index.html: index.html.m4 $(buildDir)/mini.js puzzles.json Makefile
	mkdir -p $(buildDir)
	m4 -I$(buildDir) $< > $@.new
	mv $@.new $@

$(buildDir)/mini.js: $(buildDir)/main.js Makefile
	mkdir -p build
ifdef debug
	cp $< $@.new
else
	closure-compiler --js_output_file $@.new $<
endif
	mv $@.new $@

$(buildDir)/main.js: elm.json $(wildcard src/*.elm) Makefile
	mkdir -p $(buildDir)
	elm make $(elmOptions) --output=$(dir $@)new.$(notdir $@) src/App.elm
	mv $(dir $@)new.$(notdir $@) $@

.PHONY: scramble
scramble:
	(cd swift-tools && swift run scramble ../puzzles.json) > puzzles.json.new
	mv puzzles.json.new puzzles.json

