DIR := $(dir $(lastword $(MAKEFILE_LIST)))

NUMS :=

OBJS := $(NUMS) \
  $(DIR)/abbr0s.o \
  $(DIR)/abbr0a.o \
  $(DIR)/abbr0h.o \
  $(DIR)/born.o \
  $(DIR)/born_Sfe6.o \
  $(DIR)/SquaredME.o

$(LIB): $(LIB)($(OBJS))

$(LIB)($(OBJS)): $(DIR)/vars.h Fdecl.d Finline.d

$(LIB)($(NUMS)): Fnum.d

