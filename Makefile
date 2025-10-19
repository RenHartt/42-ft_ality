PROJECT := ft_ality
SRC_DIR := src
BIN_DIR := bin

SOURCES := \
  $(SRC_DIR)/grammar.ml \
  $(SRC_DIR)/mapping.ml \
  $(SRC_DIR)/automaton.ml \
  $(SRC_DIR)/game_loop.ml \
  $(SRC_DIR)/main.ml

CMOS := $(SOURCES:.ml=.cmo)

ifeq ($(OS),Windows_NT)
  EXEEXT := .exe
  MKDIR_P := if not exist "$(BIN_DIR)" mkdir "$(BIN_DIR)"
  RM := del /q
  RMR := rmdir /q /s
  SEP := \\
  RUN := .\\$(BIN_DIR)\\$(PROJECT)$(EXEEXT)
else
  EXEEXT :=
  MKDIR_P := mkdir -p $(BIN_DIR)
  RM := rm -f
  RMR := rm -rf
  SEP := /
  RUN := ./$(BIN_DIR)/$(PROJECT)
endif

OCAMLC := ocamlc
OCAMLFLAGS := -g -bin-annot -I $(SRC_DIR)

.PHONY: all clean fclean re run deps

all: $(BIN_DIR)/$(PROJECT)$(EXEEXT)

$(BIN_DIR)/$(PROJECT)$(EXEEXT): $(CMOS) | $(BIN_DIR)
	$(OCAMLC) $(OCAMLFLAGS) -o $@ $(CMOS)

$(SRC_DIR)/%.cmo: $(SRC_DIR)/%.ml
	$(OCAMLC) $(OCAMLFLAGS) -c $< -o $@

$(BIN_DIR):
	@$(MKDIR_P)

.depend: $(SOURCES)
	-ocamldep $(SOURCES) > .depend

deps: .depend
-include .depend

ifeq ($(OS),Windows_NT)
clean:
	@if exist $(SRC_DIR)\*.cmo  $(RM) $(SRC_DIR)\*.cmo
	@if exist $(SRC_DIR)\*.cmi  $(RM) $(SRC_DIR)\*.cmi
	@if exist $(SRC_DIR)\*.cmt  $(RM) $(SRC_DIR)\*.cmt
	@if exist $(SRC_DIR)\*.cmti $(RM) $(SRC_DIR)\*.cmti
	@if exist $(SRC_DIR)\*.annot $(RM) $(SRC_DIR)\*.annot
	@if exist $(SRC_DIR)\*.o    $(RM) $(SRC_DIR)\*.o
	@if exist $(SRC_DIR)\*.obj  $(RM) $(SRC_DIR)\*.obj

fclean: clean
	@if exist $(BIN_DIR)\$(PROJECT)$(EXEEXT) $(RM) $(BIN_DIR)\$(PROJECT)$(EXEEXT)
	@if exist .depend $(RM) .depend
	@if exist $(BIN_DIR)\NUL $(RMR) $(BIN_DIR)
else
clean:
	-$(RM) $(SRC_DIR)/*.cmo $(SRC_DIR)/*.cmi $(SRC_DIR)/*.cmt $(SRC_DIR)/*.cmti $(SRC_DIR)/*.annot $(SRC_DIR)/*.o $(SRC_DIR)/*.obj

fclean: clean
	-$(RM) $(BIN_DIR)/$(PROJECT)$(EXEEXT) .depend
	-$(RMR) $(BIN_DIR)
endif

re: fclean all
