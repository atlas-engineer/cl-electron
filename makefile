NPM = npm
DESTDIR = node_modules

install: package.json
	$(NPM) install

.PHONY: all
all: install

clean:
	rm -r $(DESTDIR)
