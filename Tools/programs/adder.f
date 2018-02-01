c Copyright (C) Matthias Kesenheimer - All Rights Reserved
c Written by Matthias Kesenheimer <m.kesenheimer@gmx.net>, 2017

      program adder
        implicit none
        integer iun1, iun2, cle, nmax, i, j
        parameter (iun1=20,iun2=21,cle=500,nmax=20)
        character*(cle) name, newname
        character*(cle) arg, str
        character*(cle) strevnt(0:nmax)
        integer nstrevnt, nweights
        double precision aweight
        integer pos, nstart, nend
        double precision weight(nmax)

        if(iargc().lt.3) then
          print*,"Usage: ./adder <pwgevents.lhe> <weight> <pos>"
          stop
        endif
        do i=1,iargc()
          call getarg(i, arg)
          if(i.eq.1.and.len(trim(arg)).lt.cle) name = arg
          if(i.eq.2) read(arg,*) aweight
          if(i.eq.3) read(arg,*) pos
        enddo
        print*,'Adds the weight ',aweight,' at position ',pos,' to file'
        print*,trim(name)
        print*



        open(iun1,file=trim(name),status='old',action='read')
        newname = name(1:len_trim(name)-4)//'_add'//
     &            name(len_trim(name)-3:len_trim(name))
        open(iun2,file=trim(newname),status='replace',action='write')

        do
          read(iun1,fmt='(a)',end=111) str
          if(trim(str).ne.'<event>') then
            write(iun2,fmt='(a)') trim(str)
          endif

          ! start of event
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
              nstart = i+1
              nweights = 0
                do j=i+1,nstrevnt
                  if(trim(strevnt(j)).eq.'</weights>') goto 109
                  nweights = nweights + 1
                  read(strevnt(j),*) weight(nweights)
                enddo
              endif
            enddo
 109        continue
            nend = i+1

            ! check
            if(pos.gt.(nend-nstart+2) .or. pos.lt.1) then
              print*,'pos should be between ',1,' and ',nend-nstart+2
              stop
            endif

            ! write out the event
            do i=0,nstrevnt
              if(i.eq.(pos-1+nstart)) then
                write(iun2,fmt='(D11.5)') aweight
              endif
              write(iun2,fmt='(a)') trim(strevnt(i))
            enddo

          endif !(trim(str).eq.'<event>')

        enddo
 111    continue
        
        close(iun1)
        close(iun2)
      end
