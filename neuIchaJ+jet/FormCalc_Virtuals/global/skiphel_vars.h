#if NSKIP > 0
        ! skip calculation of hel-amplitudes that are 0 more than 'NSKIP'-times
        logical skiphel(hmax)
        data skiphel /hmax*.false./
        integer mskip(hmax)
        data mskip /hmax*0/
        save mskip
        logical initskip
        data initskip /.true./
        integer ninit,minit
        parameter (ninit=NSKIP*hmax)
        save minit
#endif
