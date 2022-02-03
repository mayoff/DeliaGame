
index.html: index.html.m4 main.js
		m4 $< > $@.new
		mv $@.new $@

main.js: $(wildcard src/*.elm)
		elm make --output=main.js src/Main.elm
