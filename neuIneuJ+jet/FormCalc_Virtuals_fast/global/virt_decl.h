* virt_decl.h
* these declarations are included "everywhere"
* this file is part of FormCalc
* last modified 23 Dec 15 th


#ifndef DECL_H
#define DECL_H

* declarations for the whole file (e.g. preprocessor defs)

#include "virt_types.h"

#else

* declarations for every subroutine

#include "virt_const.h"
#ifdef collier
#include "lt_collier.h"
#else
#include "looptools.h"
#endif

#endif

#include "virt_user.h"
#include "virt_util.h"

#ifdef SQUAREDME
#include "RenConst.h"
#endif

