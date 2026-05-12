.PHONY: build test test-extension clean fmt lint install deb help deps dev-extension

.DEFAULT_GOAL := build

help: ## Show this help
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | awk 'BEGIN {FS = ":.*?## "}; {printf "  \033[36m%-18s\033[0m %s\n", $$1, $$2}'

build: ## Build all targets
	dune build @all

test: ## Run OCaml tests
	dune runtest

_build/node_modules/.stamp: extension/package.json
	@mkdir -p _build
	cp extension/package.json _build/
	cd _build && npm install --no-package-lock --quiet
	@touch $@

deps: _build/node_modules/.stamp ## Install opam and node dependencies
	opam install . --deps-only --with-test --yes

test-extension: build _build/node_modules/.stamp ## Build and run extension tests
	cd extension && NODE_PATH=../_build/node_modules ../_build/node_modules/.bin/jest --forceExit

dev-extension: build ## Stage unpacked extension in _build/dev-extension
	dune build extension/main.js extension/popup.js extension/config.js \
		extension/options.js extension/add_rule.js extension/history_panel.js
	mkdir -p _build/dev-extension/icons
	cp extension/manifest.json extension/popup.html extension/add_rule.html \
		extension/config.html extension/options.html extension/history_panel.html \
		_build/dev-extension/
	cp _build/default/extension/main.js _build/dev-extension/
	cp _build/default/extension/popup.js _build/dev-extension/
	cp _build/default/extension/config.js _build/dev-extension/
	cp _build/default/extension/options.js _build/dev-extension/
	cp _build/default/extension/add_rule.js _build/dev-extension/
	cp _build/default/extension/history_panel.js _build/dev-extension/
	cp extension/icons/* _build/dev-extension/icons/
	@echo "Extension staged in _build/dev-extension/"
	@echo "Load as unpacked extension in chrome://extensions"

clean: ## Clean build artifacts
	dune clean

fmt: ## Format source code
	dune fmt

lint: ## Run lint checks
	dune build @check

install: ## Install via dune
	dune install

VERSION ?= 0.0.0

deb: ## Build debian packages (VERSION=x.y.z)
	sed --in-place='.orig' "1s/([^)]*)/($(VERSION))/" debian/changelog
	sed --in-place='.orig' 's/"version": "[^"]*"/"version": "$(VERSION)"/' extension/manifest.json
	dpkg-buildpackage -us -uc -b -d -nc
	@mkdir -p _build/deb
	mv ../alloyd_$(VERSION)_*.deb _build/deb/
	mv ../alloy_$(VERSION)_*.deb _build/deb/
	mv -f debian/changelog.orig debian/changelog
	mv -f extension/manifest.json.orig extension/manifest.json
	@echo "Built packages in _build/deb/"
