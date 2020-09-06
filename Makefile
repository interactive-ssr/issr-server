MINOPTIONS = --compress --mangle

issr.min.js: issr.js
	uglifyjs $< $(MINOPTIONS) --output $@
