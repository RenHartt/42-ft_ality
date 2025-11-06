PROJECT := ft_ality
SRC_DIR := src
BIN_DIR := bin
BUILD_DIR := $(BIN_DIR)/build

SOURCES := \
  $(SRC_DIR)/grammar.ml \
  $(SRC_DIR)/mapping.ml \
  $(SRC_DIR)/automaton.ml \
  $(SRC_DIR)/game_loop.ml \
  $(SRC_DIR)/main.ml

CMOS := $(patsubst $(SRC_DIR)/%.ml,$(BUILD_DIR)/%.cmo,$(SOURCES))

SHELL := /usr/bin/sh

OCAMLC     := $(shell opam var bin)/ocamlfind ocamlc
OCAMLDEP   := $(shell opam var bin)/ocamlfind ocamldep
OCAMLFLAGS := -g -bin-annot -I $(SRC_DIR) -I $(BUILD_DIR) -package tsdl
LINKFLAGS  := -package tsdl -linkpkg

MKDIR_P := mkdir -p
RM      := rm -f
RMR     := rm -rf

.PHONY: all clean fclean re deps

all: $(BIN_DIR)/$(PROJECT)

$(BIN_DIR)/$(PROJECT): $(CMOS) | $(BIN_DIR)
	$(OCAMLC) $(OCAMLFLAGS) $(LINKFLAGS) -o $@ $(CMOS)

$(BUILD_DIR)/%.cmo: $(SRC_DIR)/%.ml | $(BUILD_DIR)
	$(OCAMLC) $(OCAMLFLAGS) -c $< -o $@

$(BIN_DIR) $(BUILD_DIR):
	@$(MKDIR_P) $@

.depend: $(SOURCES)
	-ocamldep -I $(SRC_DIR) $(SOURCES) > .depend

deps: .depend
-include .depend

clean:
	-$(RM) $(BUILD_DIR)/*

fclean: clean
	-$(RM) $(BIN_DIR)/$(PROJECT) .depend
	-$(RMR) $(BUILD_DIR) $(BIN_DIR)

re: fclean all
