      program topintegrator
        implicit none
        integer iun, cle, i, j, ac, maxlines
        parameter (iun=20,cle=500,maxlines=15000)
        character*(cle) name
        character*(cle) arg, str
        double precision binstart, binend, x, xerr
        double precision int, interr

        if(iargc().eq.0) name = 'pwg-NLO.top'
        do i=1,iargc()
          call getarg(i, arg)
          if(i.eq.1.and.len(trim(arg)).lt.cle) name = arg
        enddo
        print*,'Integrated cross section obtained from distributions in'
        print*,trim(name)
        print*

        open(iun,file=trim(name),status='old',action='read')

        int = 0D0
        interr = 0D0
        do i=1,maxlines+1
          if(i.eq.maxlines+1) then
            print*,'too many lines in file, increase maxlines'
            stop
          endif
          read(iun,fmt='(a)',end=111) str

          ! start of data set
          if(str(1:1).eq.'#'.and.len(trim(str)).ne.0) then
            if(i.ne.1) then
              print*,int,'+-',interr
              print*
            endif
            int = 0D0
            interr = 0D0
            print*,trim(str)
          endif

          ! read data set and sum
          if(str(1:1).ne.'#'.and.len(trim(str)).ne.0) then
            read(str,*,end=110) binstart, binend, x, xerr
 110        continue
            int = int + (binend-binstart)*x
            interr = interr + (binend-binstart)*xerr
           endif

        enddo
 111    continue

        ! last data set
        print*,int,'+-',interr
        
        close(iun)
      end
