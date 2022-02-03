
.PHONY: reload
reload: index.html
		./reload-safari.sh

index.html: index.html.m4 mini.js
		m4 $< > $@.new
		mv $@.new $@

mini.js: main.js
		closure-compiler --js_output_file $@.new $^
		mv $@.new $@

main.js: $(wildcard src/*.elm)
		elm make --output=main.js $^
