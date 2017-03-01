      ! from ff.h:
      ! regularization parameters (modified)
      RealType lambda
      common /ltregul/ lambda

      ! from lt.h: (modified)
      integer epsi
      common /ltvars/ epsi

      ! variables for interfacing the cache system of collier
      ! global variables to store the numbers of 
      ! Nmax   = maximal N up to which N-point integrals are cached
      ! Rmax   = maximal rank of loop integrals (usually = Nmax)
      integer NCacheSave, NmaxSave, RmaxSave
      common /collierinterface/ NCacheSave, NmaxSave, RmaxSave
