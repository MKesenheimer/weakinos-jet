        integer nmax
        parameter (nmax=100)
        character*(100) histlist(nmax)
        integer nhists, unitlist(nmax)
        double precision xm(nmax), xp(nmax)
        integer nbins(nmax)
        
        common /c_hist/ xm, xp
        common /c_hist/ nbins
        common /c_hist/ nhists,unitlist,histlist