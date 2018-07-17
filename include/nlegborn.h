c############### nlegborn header #######################################
c last modified by MK, 11.07.2018
c weakino pair + jet production

c The user must set nlegborn to the appropriate value for his process.
c for neutralino pair production
        integer nlegborn,nlegreal

        ! remember to take the resonants particle into account
        parameter (nlegborn=5)
        parameter (nlegreal=nlegborn+1)

c ndiminteg is the dimensionality of the full real integral
c if there are undecayed resonances, we need extra variables to pilot
c the resonance's masses
c -4: (p,E) conservation 
c +2: x1 x2 
c -1: azimuthal-integration

        integer ndiminteg
        parameter (ndiminteg=(nlegreal-2)*3-4+2-1)
        ! if we want to integrate additionally over phi
        !parameter (ndiminteg=7)

        
        integer maxprocborn,maxprocreal,maxflow
        parameter (maxprocborn=999,maxprocreal=999,maxflow=10)

        integer maxalr
        parameter (maxalr=maxprocreal*nlegreal*(nlegreal-1)/2)

        integer maxreshists
        parameter (maxreshists=500)

        ! integer maxresgroups
        ! parameter (maxresgroups=2)

        ! these are the external (i.e. not including intermediate
        ! resonances) particles in the born process
        integer nlegbornexternal
        parameter (nlegbornexternal=5)

c############### nlegborn header #######################################
