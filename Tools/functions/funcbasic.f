c############### funcbasic.f ###########################################
c some simple auxiliary functions for matrix elements
c last modified by MK, 18.12.2015
c adapted from dislepton and disquark, plus additional helpful functions

c############### functions #############################################
c Kronecker delta
      double precision function kronecker(i,j)
        implicit none
        integer i,j
        kronecker = 0D0
        if(i.eq.j) kronecker = 1d0
      end

c metric tensor
      double precision function gmunu(mu,nu)
        implicit none
        integer mu,nu
        gmunu = 0D0
        if(mu.eq.0.and.mu.eq.nu) gmunu = 1d0
        if(mu.ne.0.and.mu.eq.nu) gmunu = -1d0
      end
      
c swap two integers
      subroutine swapi(i1,i2)
        implicit none
        integer i1,i2,itmp
        itmp=i1
        i1=i2
        i2=itmp
      end

c squared of a four vector
      double precision function momsq(p)
        implicit none
        double precision p(0:3), dotp
        external dotp
        momsq = dotp(p,p)
      end

c check odd or even integer
      logical function iseven(n)
        implicit none
        integer n
        if(mod(abs(n),2).eq.1) then
          iseven = .false.
        else
          iseven = .true.
        endif
      end
      logical function isodd(n)
        implicit none
        integer n
        if(mod(abs(n),2).eq.1) then
          isodd = .true.
        else
          isodd = .false.
        endif
      end
      
c check quark types
      logical function isg(n)
        implicit none
        integer n
        if(n.eq.0) then 
          isg = .true.
        else
          isg = .false.
        endif
      end

      logical function isd(n)
        implicit none
        integer n
        logical isodd
        external isodd
        if(isodd(n).and.n>0) then
          isd = .true.
        else
          isd = .false.
        endif
      end

      logical function isu(n)
        implicit none
        integer n
        logical iseven
        external iseven
        if(iseven(n).and.n>0) then
          isu = .true.
        else
          isu = .false.
        endif
      end

      logical function isdbar(n)
        implicit none
        integer n
        logical isodd
        external isodd
        if(isodd(n).and.n<0) then
          isdbar = .true.
        else
          isdbar = .false.
        endif
      end

      logical function isubar(n)
        implicit none
        integer n
        logical iseven
        external iseven
        if(iseven(n).and.n<0) then
          isubar = .true.
        else
          isubar = .false.
        endif
      end
      
c quark generation
      integer function qgen(n)
        implicit none
        integer n
        qgen = -999
        if(abs(n).eq.1.or.abs(n).eq.2) qgen = 1
        if(abs(n).eq.3.or.abs(n).eq.4) qgen = 2
        if(abs(n).eq.5.or.abs(n).eq.6) qgen = 3
        !if(n.lt.0) qgen = -1*qgen
      end  
      
c electric charge
      double precision function elc(n)
        implicit none
        integer n
        logical isd, isdbar, isu, isubar
        external isd, isdbar, isu, isubar
        elc = 0
        if(isd(n)) elc = -1/3D0
        if(isdbar(n)) elc = 1/3D0
        if(isu(n)) elc = 2/3D0
        if(isubar(n)) elc = -2/3D0
        if(n.eq.1000024.or.n.eq.1000037) elc = 1D0
        if(n.eq.-1000024.or.n.eq.-1000037) elc = -1D0
      end
      
c removes duplicate entries of a two dimensional list(m,n)
c m and n are the dimensions of the list. Example:
c input: m=2, n=3
c   list(:,1) = (1, 0)
c   list(:,2) = (1, 2)
c   list(:,3) = (1, 0)
c output: m=2, n=2
c   list(:,1) = (1, 0)
c   list(:,2) = (1, 2)
      subroutine remove_duplicates(list,m,n)
        implicit none
        integer list, m, n, i, j, k, l
        integer nstart, nend
        dimension list(m,n)
        logical check
        nstart = n
   10   do i=1,n ! go through all the entries
          do j=i+1,n ! don't compare entries with same index
            check = .true.
            do k=1,m ! compare every entry
              if(list(k,i).ne.list(k,j)) check = check .and. .false.
            enddo
            if(check) then
              do l=j,n-1 ! if an entry is a duplicate, delete it, and 
                do k=1,m ! move all following entries one position up
                  list(k,l) = list(k,l+1)
                enddo
              enddo
              do k=1,m ! delete the last entry
                list(k,n) = 0
              enddo
              n = n-1 ! the dimension n is decreased
            endif
          enddo
        enddo
        nend = n
        if(nend.ne.nstart) then ! again until n does not change anymore
          nstart = nend
          goto 10
        endif
      end

c squared of sum of 2 four vectors
      double precision function momsum2sq(p1,p2)
        implicit none
        double precision p1(0:3),p2(0:3),psum(0:3)
        double precision dotp
        external dotp
        integer j
        do j=0,3
          psum(j) = p1(j) + p2(j)
        enddo
        momsum2sq = dotp(psum,psum)
      end

c squared of sum of 3 four vectors
      double precision function momsum3sq(p1,p2,p3)
        implicit none
        double precision p1(0:3),p2(0:3),p3(0:3),psum(0:3)
        double precision dotp
        external dotp
        integer j
        do j=0,3
          psum(j) = p1(j) + p2(j) + p3(j)
        enddo
        momsum3sq = dotp(psum,psum)
      end
      
c levi-civita symbol
      integer function levi_civita(i,j,k)
        implicit none
        integer i,j,k
        ! unit vectors
        integer x(3,3)
        if( (i .le. 0 .or. i .ge. 4) .or. 
     &      (j .le. 0 .or. j .ge. 4) .or.
     &      (k .le. 0 .or. k .ge. 4)) then
          print*, "error in function levi_civita"
          print*, "i,j,k = ", i,j,k
          stop
        endif
        x(1,1) = 1
        x(2,1) = 0
        x(3,1) = 0
        x(1,2) = 0
        x(2,2) = 1
        x(3,2) = 0
        x(1,3) = 0
        x(2,3) = 0
        x(3,3) = 1
        ! calculate levi_civita as determinant of (x1,x2,x3)
        levi_civita = - x(i,3)*x(j,2)*x(k,1) + x(i,2)*x(j,3)*x(k,1)
     &                + x(i,3)*x(j,1)*x(k,2) - x(i,1)*x(j,3)*x(k,2)
     &                - x(i,2)*x(j,1)*x(k,3) + x(i,1)*x(j,2)*x(k,3)
      end

c denominator function needed by FormCalc
      double precision function Den(x,y)
        implicit none
        double precision x,y
        Den = 1/(x-y)
      end
      
c the epsilon tensor fully contracted with four-momenta k1..k4
      double precision function epsilonk(k1,k2,k3,k4)
        implicit none
        double precision k1(0:3),k2(0:3),k3(0:3),k4(0:3)
        epsilonk = k1(3)*k2(2)*k3(1)*k4(0) - k1(2)*k2(3)*k3(1)*k4(0) - 
     &            k1(3)*k2(1)*k3(2)*k4(0) + k1(1)*k2(3)*k3(2)*k4(0) + 
     &            k1(2)*k2(1)*k3(3)*k4(0) - k1(1)*k2(2)*k3(3)*k4(0) - 
     &            k1(3)*k2(2)*k3(0)*k4(1) + k1(2)*k2(3)*k3(0)*k4(1) + 
     &            k1(3)*k2(0)*k3(2)*k4(1) - k1(0)*k2(3)*k3(2)*k4(1) - 
     &            k1(2)*k2(0)*k3(3)*k4(1) + k1(0)*k2(2)*k3(3)*k4(1) + 
     &            k1(3)*k2(1)*k3(0)*k4(2) - k1(1)*k2(3)*k3(0)*k4(2) - 
     &            k1(3)*k2(0)*k3(1)*k4(2) + k1(0)*k2(3)*k3(1)*k4(2) + 
     &            k1(1)*k2(0)*k3(3)*k4(2) - k1(0)*k2(1)*k3(3)*k4(2) - 
     &            k1(2)*k2(1)*k3(0)*k4(3) + k1(1)*k2(2)*k3(0)*k4(3) + 
     &            k1(2)*k2(0)*k3(1)*k4(3) - k1(0)*k2(2)*k3(1)*k4(3) - 
     &            k1(1)*k2(0)*k3(2)*k4(3) + k1(0)*k2(1)*k3(2)*k4(3)
      end
      
c takes an array with n entries, if entry is negative: 
c mult. with -1 and save sign in the 2nd array
        subroutine transfersign(arr,signarr,n)
          implicit none
          integer n
          real*8 arr(n)
          integer signarr(n)
          integer i
          do i=1,n
            if(arr(i).lt.0) then
              arr(i) = -arr(i)
              signarr(i) = -1
            else
              signarr(i) = 1
            endif
          enddo
        end

c error-determination (taken from btilde-routines)
      double precision function calc_error(tot,etot2,n)
        implicit none
        double precision tot,etot2
        integer n
        calc_error = dsqrt((etot2/n-(tot/n)**2)/n)
      end
      
c factorial
      integer function factorial(n)
        implicit none
        integer n,m,i
        factorial=1
        m=1
        do i=1,n
          m=m*i
        enddo
        factorial=m
      end

c gibt die i-te Permutation der 4-Impulsvektoren p1, p2, ... p_DIMEN
c zurück. DIMEN und FAKUL müssen in Abhänbgigkeit der zu permutierenden
c Impulse von Hand gesetzt werden.
c Im common Block /ind/ ist außerdem die aktuelle Permutation zu
c Statuszwecken zu finden.
#define DIMEN 5
#define FAKUL 120
      subroutine permute(p,i)
        integer n,i,j,k,a
        logical nextp
        double precision r(0:3,1:FAKUL,1:DIMEN)
        integer indices(1:DIMEN),pindices(1:FAKUL,1:DIMEN)  
        double precision p(0:3,1:DIMEN)
        double precision q(0:3,1:DIMEN)  
        external nextp
        common /ind/ indices  
        parameter(n=DIMEN)
        dimension a(1:n)
        if(i.gt.FAKUL) then
          print*,"error: i > DIMEN"    
          stop
        endif    
        do j=1,n
          a(j)=j  
        enddo
        k = 1  
        q(:,:) = p(:,:)
   10   do j=1,n
          pindices(k,j) = a(j)  
          r(:,k,j) = q(:,a(j))
        enddo 
        k = k+1  
        if(nextp(n,a)) goto 10
        p(:,:) = r(:,i,:)
        indices(:) = pindices(i,:)  
        !print*,indices(i,:)
      end subroutine permute
 
      function nextp(n,a)
        integer n,a,i,j,k,t
        logical nextp
        dimension a(n)
        i=n-1
   10   if(a(i).lt.a(i+1)) goto 20
        i=i-1
        if(i.eq.0) goto 20
        goto 10
   20   j=i+1
        k=n
   30   t=a(j)
        a(j)=a(k)
        a(k)=t
        j=j+1
        k=k-1
        if(j.lt.k) goto 30
        j=i
        if(j.ne.0) goto 40
        nextp=.false.
        return
   40   j=j+1
        if(a(j).lt.a(i)) goto 40
        t=a(i)
        a(i)=a(j)
        a(j)=t
        nextp=.true.
      end

c berechnet die Größenordnung einer Zahl
      integer function magnitude(x)
        implicit none
        double precision x,z,p
        integer i
        i = 0
        z = x
        if(dabs(x).eq.0D0) goto 20
        if(dabs(x).lt.1d0) p = 1D1
        if(dabs(x).gt.1d0) p = 1D-1
   10   z = z*p
        if(dabs(x).gt.1d0.and.dabs(z).ge.1d0) then
          i = i+1
          goto 10
        endif
        if(dabs(x).lt.1d0.and.dabs(z).le.1d0) then
          i = i-1
          goto 10
        endif
   20   magnitude = i
      end 
      
c übersetzt eine dezimalzahl in ein anderes Zahlsystem zur 
c Basis "BASE" und gibt die Zahl an der Stelle k zurück
#define LENGTH 9
#define BASE 3
      integer function cdec(d,k)
        implicit none
        integer d,di,df,r,i,k
        integer b(0:LENGTH)
        do i=0,LENGTH
        b(i) = 0
        enddo
        di = d  
        i = LENGTH
   10   df = int(di/(BASE**(i)))
        r = mod(di,BASE**(i))
        b(i) = df
        di = r    
        i = i - 1    
        if(r.ne.0) goto 10
        cdec = b(k)  
      end

c gibt das Vorzeichen einer reellen Zahl zurück (+-1)
      double precision function signum(r)
        implicit none
        double precision r
        !signum = r/dabs(r)
        if(r.ge.0D0) then
          signum = 1D0
        else
          signum = -1D0
        endif
      end

c transforms a lower case character string to an upper case
      subroutine to_uppercase(str1,upper1)
        implicit none
        character*(*) str1, upper1
        integer j
        upper1 = str1
        do j=1,len(trim(str1))
          ! convert both strings to uppercase
          if(str1(j:j) .ge. "a" .and. str1(j:j) .le. "z") then
            upper1(j:j) = achar(iachar(str1(j:j)) - 32)
          else
            upper1(j:j) = str1(j:j)
          endif
        enddo
      end

c sorts the entries of an integer list in decreasing order
      subroutine sorti(list,lgth)
        implicit none
        integer i,j,n,t,lgth
        parameter (n=100)
        integer b(n), list(n)
        if(lgth.gt.n) then
          print*,"error in sorti: increase n = ",n
          print*,"lgth = ",lgth
          stop
        endif  
        do i=1,lgth
          b(i) = list(i)
        enddo
        do i=1,lgth
          do j=i,lgth
            if(b(i)<b(j)) then
              t = b(j)
              b(j) = b(i)
              b(i) = t
            endif
          enddo
        enddo
        do i=1,lgth
          list(i) = b(i)
        enddo
      end 
      
c sorts the entries of an integer list in decreasing order and keep track
c of sorting in ilist
c bevor sorting:
c list(1) = 5
c list(2) = 6
c list(3) = 4
c after sorting:
c list(1) = 6, ilist(1) = 2
c list(2) = 5, ilist(2) = 1
c list(3) = 4, ilist(3) = 3
      subroutine sortit(list,ilist,lgth)
        implicit none
        integer i,j,n,t,it,lgth
        parameter (n=100)
        integer b(n), list(n),ilist(n)
        if(lgth.gt.n) then
          print*,"error in sorti: increase n = ",n
          print*,"lgth = ",lgth
          stop
        endif  
        do i=1,lgth
          b(i) = list(i)
          ilist(i) = i
        enddo
        do i=1,lgth
          do j=i,lgth
            if(b(i)<b(j)) then
              t = b(j)
              it = ilist(j)
              b(j) = b(i)
              ilist(j) = ilist(i)
              b(i) = t
              ilist(i) = it
            endif
          enddo
        enddo
        do i=1,lgth
          list(i) = b(i)
        enddo
      end

c stops the program if gets called for n times (useful for debugging)
      subroutine nstop(n)
        integer n,i
        save i
        i = i + 1
        if(i.ge.n) then
          i = 0
          stop
        endif
      end
      
c calculates the kaellen function and the sqrt of kaellen function
      double precision function kaellen(x, y, z)
        implicit none
        double precision x, y, z
        kaellen = x**2+y**2+z**2-2*(x*y+x*z+y*z)
      end

      double precision function kaellenSqrt(x, y, z)
        implicit none
        double precision x, y, z
        kaellenSqrt = dsqrt(dabs(x**2+y**2+z**2-2*(x*y+x*z+y*z)))
      end
      
c calculate the angles in spherical coordinates of a four vector
      subroutine angles(p, phi, theta)
        implicit none
        double precision p(0:3), phi, theta
        double precision pi
        parameter (pi = 4.D0*datan(1.D0))
        theta = dacos(p(3)/dsqrt(p(1)**2+p(2)**2+p(3)**2))
        if(p(1).gt.0d0) then
          phi = datan(p(2)/p(1))
        elseif(p(1).eq.0d0) then
          phi = sign(pi/2D0,p(2))
        elseif(p(1).lt.0D0.and.p(2).ge.0D0) then
          phi = datan(p(2)/p(1))+pi
        elseif(p(1).lt.0D0.and.p(2).lt.0D0) then
          phi = datan(p(2)/p(1))-pi
        endif
      end
      
c calculates the polarization vector eps(k,hel) of a massles vector particle
c hel can be -1, 1 for a massles vector particle
      subroutine polvector(p, hel, eps)
        implicit none
        double precision p(0:3), phi, theta
        integer hel
        double complex eps(0:3),ii
        double precision pi,sqrt12
        parameter (pi = 4.D0*datan(1.D0))
        parameter (sqrt12 = 1D0/dsqrt(2D0))
        parameter (ii = (0D0,1D0))
        if(hel.ne.1 .and. hel.ne.-1) then
          print*,"wrong helicity: ",hel
          print*,"hel requested should be -1 or 1."
          stop
        endif  
        call angles(p, phi, theta)
        eps(0) = 0D0
        eps(1) = sqrt12*(-hel*dcos(theta)*dcos(phi)+ii*dsin(phi))
        eps(2) = sqrt12*(-hel*dcos(theta)*dsin(phi)-ii*dcos(phi))
        eps(3) = sqrt12*hel*dsin(theta)
      end
      
c polarization sum to a certain momenta and arbitrary vector n
      subroutine polsum(p, n, mat)
        implicit none
        double precision p(0:3), n(0:3), mat(0:3,0:3)
        double precision gmunu, dotp
        external gmunu, dotp
        integer mu,nu
        do mu=0,3
        do nu=0,3
          mat(mu,nu) = -gmunu(mu,nu)-(p(mu)*p(nu)-dotp(p,n)*
     &                  (p(mu)*n(nu)+p(nu)*n(mu))+dotp(p,p)*n(mu)*n(nu))
     &                  /(dotp(n,p)**2-dotp(p,p)**2)
        enddo
        enddo
      end  
c############### end functions #########################################
