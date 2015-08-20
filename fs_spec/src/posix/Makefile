# S HELL=bash

all: link.html mkdir.html open.html rename.html rmdir.html

#	pandoc --css=style.css rename -o rename.html

%.html: %.md FORCE
	pandoc --css=style.css $< -o $@

FORCE:
