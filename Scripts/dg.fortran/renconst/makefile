DIR := $(dir $(lastword $(MAKEFILE_LIST)))

OBJS := \
  $(DIR)/CalcRenConst.o

$(LIB): $(LIB)($(OBJS))

$(LIB)($(OBJS)): Fdecl.d Finline.d

