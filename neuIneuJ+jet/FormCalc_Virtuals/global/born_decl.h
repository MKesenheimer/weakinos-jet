* born_decl.h
* these declarations are included "everywhere"
* this file is part of FormCalc
* last modified 23 Dec 15 th


#ifndef DECL_H
#define DECL_H

* declarations for the whole file (e.g. preprocessor defs)

#include "born_types.h"

#else

* declarations for every subroutine

#include "born_const.h"
#include "lt_collier.h"

#endif

#include "born_user.h"
#include "born_util.h"

#ifdef SQUAREDME
#include "RenConst.h"
#endif

