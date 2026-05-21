SHELL := bash
.ONESHELL:
.PHONY: build bundle test format check-format clean generate-purs-scripts
.SHELLFLAGS := -eu -o pipefail -c

ps-sources := $(shell fd --no-ignore-parent -epurs)
js-sources := $(shell fd --no-ignore-parent -ejs -ecjs)

# Whether to bundle for the browser
browser-runtime := 1 # Use "1" for true and "" for false

ps-entrypoint := Dao.Web.Api

purs-args := "--stash --censor-lib --censor-codes=ImplicitImport,ImplicitQualifiedImport,ImplicitQualifiedImportReExport,UserDefinedWarning,UnusedName,ShadowedName,MissingTypeDeclaration"

build:
	spago build --purs-args ${purs-args}

bundle: build
	BROWSER_RUNTIME=${browser-runtime} node esbuild/bundle.js output/${ps-entrypoint}/index.js dist/index.js

test:
	spago run --main Test.Localnet

format:
	@purs-tidy format-in-place ${ps-sources}
	prettier -w ${js-sources}

check-format:
	@purs-tidy check ${ps-sources}
	@prettier --loglevel warn -c ${js-sources}
	@eslint --quiet ${js-sources} --parser-options 'sourceType: module'

clean:
	@ rm -r .psc-ide-port || true
	@ rm -rf .psci_modules || true
	@ rm -rf .spago || true
	@ rm -rf generated-docs || true
	@ rm -rf .spago2nix || true
	@ rm -rf node_modules || true
	@ rm -rf output || true
	@ rm -rf dist || true

generate-purs-scripts:
	@for script_type in Debug Optimised; do \
		json_dir=./src/Dao/Scripts/Json/$$script_type; \
		output_file=./src/Dao/Scripts/Serialized/$$script_type.purs; \
		echo "Generating PureScript scripts for $$json_dir into $$output_file..."; \
		mkdir -p $$(dirname $$output_file); \
		> $$output_file; \
		printf "module Dao.Scripts.Serialized.$$script_type where\n\n" >> $$output_file; \
		for json_file in $$json_dir/*.json; do \
			var_name=$$(basename $$json_file .json | sed 's/^\(.\)/\L\1/'); \
			value=$$(jq -r . $$json_file); \
			echo "$$var_name = \"$$value\"" >> $$output_file; \
		done; \
	done
	@echo "All PureScript scripts generated."
