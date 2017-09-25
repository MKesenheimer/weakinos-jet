      program rescaler
        implicit none
        integer iun1,iun2, cle, i, j, ac, maxlines
        parameter (iun1=20,iun2=21,cle=500,maxlines=15000)
        character*(cle) name, newname
        character*(cle) arg, str
        double precision binstart, binend, x, xerr
        double precision int, interr
        double precision rescale

        if(iargc().lt.2) then
          print*,"Usage: ./rescaler <pwg-NLO.top> <factor>"
          stop
        endif
        do i=1,iargc()
          call getarg(i, arg)
          if(i.eq.1.and.len(trim(arg)).lt.cle) name = arg
          if(i.eq.2) read(arg,*) rescale
        enddo
        print*,'Rescaling the distributions in'
        print*,trim(name)
        print*,"by factor: ",rescale
        print*


        open(iun1,file=trim(name),status='old',action='read')
        newname = name(1:len_trim(name)-4)//'_res'//
     &            name(len_trim(name)-3:len_trim(name))
        open(iun2,file=trim(newname),status='replace',action='write')

        int = 0D0
        interr = 0D0
        do i=1,maxlines+1
          if(i.eq.maxlines+1) then
            print*,'too many lines in file, increase maxlines'
            stop
          endif
          read(iun1,fmt='(a)',end=111) str

          ! start of data set
          if(str(1:1).eq.'#'.and.len(trim(str)).ne.0) then
            if(i.ne.1) print*
            if(i.ne.1) print*
            print*,trim(str)
            if(i.ne.1) write(iun2,*)
            if(i.ne.1) write(iun2,*)
            write(iun2,*) trim(str)
          endif

          ! read data set and sum
          if(str(1:1).ne.'#'.and.len(trim(str)).ne.0) then
            read(str,*,end=110) binstart, binend, x, xerr
 110        continue
            write(*,201) binstart, binend, rescale*x, rescale*xerr
            write(iun2,201) binstart, binend, rescale*x, rescale*xerr
 201        format(D14.8,' ',D14.8,' ',D14.8,' ',D14.8)
           endif

        enddo
 111    continue
        
        close(iun1)
        close(iun2)
      end
