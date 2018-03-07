c Copyright (C) Matthias Kesenheimer - All Rights Reserved
c Written by Matthias Kesenheimer <m.kesenheimer@gmx.net>, 2017

      program filter
        implicit none
        integer iun1, iun2, iun3, cle, nmax, i, j
        parameter (iun1=20,iun2=21,cle=500,nmax=20)
        character*(cle) name, newname
        character*(cle) arg, str
        character*(cle) strevnt(0:nmax)
        integer nstrevnt, nweights
        double precision lower, upper
        double precision weight(nmax)
        logical badevent

        if(iargc().lt.3) then
          print*,"Usage: ./filter <pwgevents.lhe> <lower> <upper>"
          stop
        endif
        do i=1,iargc()
          call getarg(i, arg)
          if(i.eq.1.and.len(trim(arg)).lt.cle) name = arg
          if(i.eq.2) read(arg,*) lower
          if(i.eq.3) read(arg,*) upper
        enddo
        print*,'Remove unphysical weights from event file'
        print*,trim(name)
        print*,"Limits: ",lower, upper
        print*

        open(iun1,file=trim(name),status='old',action='read')
        newname = name(1:len_trim(name)-4)//'_flt'//
     &            name(len_trim(name)-3:len_trim(name))
        open(iun2,file=trim(newname),status='replace',action='write')
        newname = name(1:len_trim(name)-4)//'_bad'//
     &            name(len_trim(name)-3:len_trim(name))
        open(iun3,file=trim(newname),status='replace',action='write')

        do
          read(iun1,fmt='(a)',end=111) str
          if(trim(str).ne.'<event>') then
            write(iun2,fmt='(a)') trim(str)
          endif

          ! start of event
          badevent = .false.
          if(trim(str).eq.'<event>') then
            ! store event in string array strevnt
            strevnt(0) = '<event>'
            nstrevnt = 0
            do i=1,nmax
              read(iun1,fmt='(a)',end=111) strevnt(i)
              nstrevnt = nstrevnt + 1
              if(trim(strevnt(i)).eq.'</event>') exit
            enddo

            ! extract the weights from event
            do i=0,nstrevnt
              if(trim(strevnt(i)).eq.'<weights>') then
              nweights = 0
                do j=i+1,nstrevnt
                  if(trim(strevnt(j)).eq.'</weights>') goto 109
                  nweights = nweights + 1
                  read(strevnt(j),*) weight(nweights)
                enddo
              endif
            enddo
 109        continue
 
            ! if weight violates the limits, delete the event
            do i=1,nweights
              if(dabs(weight(i)).gt.upper .or.
     &           dabs(weight(i)).lt.lower .or. isnan(weight(i))) then
                print*,"Bound violation: ",weight(i),
     &                  "-> removing event."
                badevent = .true.
              endif
            enddo

            ! write out the event
            if(.not.badevent) then
              do i=0,nstrevnt
                write(iun2,fmt='(a)') trim(strevnt(i))
              enddo
            else
              do i=0,nstrevnt
                write(iun3,fmt='(a)') trim(strevnt(i))
              enddo
            endif

          endif !(trim(str).eq.'<event>')

        enddo
 111    continue
        
        close(iun1)
        close(iun2)
        close(iun3)
      end
