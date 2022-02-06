
builddir := ../build

.PHONY: reload
reload: $(builddir)/index.html
		./reload-safari.sh

.PHONY: clean
clean:
		rm -rf $(builddir)

$(builddir)/index.html: index.html.m4 $(builddir)/mini.js
		m4 -I$(builddir) $< > $@.new
		mv $@.new $@

$(builddir)/mini.js: $(builddir)/main.js
		closure-compiler --js_output_file $@.new $^
		mv $@.new $@

$(builddir)/main.js: elm.json $(wildcard src/*.elm)
		elm make --output=$(dir $@)new.$(notdir $@) $(filter %.elm,$^)
		mv $(dir $@)new.$(notdir $@) $@

$(builddir):
		mkdir -p $(builddir)
