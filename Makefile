SHELL := bash
.ONESHELL:
.SHELLFLAGS := -eu -o pipefail -c
.DELETE_ON_ERROR:
MAKEFLAGS += --warn-undefined-variables
MAKEFLAGS += --no-builtin-rules

SRC_DIR := src
ELM_FILES := $(shell find . -type f -name "*.elm")
SRC_FILE := $(SRC_DIR)/HomePage.elm

HTML_FILE := index.html
OUTPUT_FILE := elm.js

# Targets

$(HTML_FILE): $(OUTPUT_FILE)

$(OUTPUT_FILE): $(ELM_FILES)
	elm make $(SRC_FILE)

# Convenience targets

.PHONY: view
view: $(HTML_FILE)
	open $<

.PHONY: test
test: $(ELM_FILES)
	elm-test
