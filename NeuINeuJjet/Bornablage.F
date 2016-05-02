       !dd -> neu neu jet 
        if(     ((bflav(1).eq.1 .and. bflav(2).eq.-1) .or.
     &           (bflav(1).eq.-1 .and. bflav(2).eq.1)) .and. 
     &           (bflav(3).eq.1000022 .and. bflav(4).eq.1000022) ) then
	  call ddbar_n1n1j_squaredME(fc_result, helicities, flags)
          goto 20
       !uu -> neu neu jet
        elseif(((bflav(1).eq.-2 .and. bflav(2).eq.2) .or.
     &           (bflav(1).eq.2 .and. bflav(2).eq.-2)) .and. 
     &           (bflav(3).eq.1000022 .and. bflav(4).eq.1000022) ) then
          call uubar_n1n1j_squaredME(fc_result, helicities, flags)
          goto 20
        !ss -> neu neu jet
        elseif(((bflav(1).eq.3 .and. bflav(2).eq.-3) .or.
     &           (bflav(1).eq.-3 .and. bflav(2).eq.3)) .and. 
     &           (bflav(3).eq.1000022 .and. bflav(4).eq.1000022) ) then
          call ddbar_n1n1j_squaredME(fc_result, helicities, flags)
          goto 20
        !cc  -> neu neu jet
        elseif(((bflav(1).eq.-4 .and. bflav(2).eq.4) .or.
     &           (bflav(1).eq.4 .and. bflav(2).eq.-4)) .and. 
     &           (bflav(3).eq.1000022 .and. bflav(4).eq.1000022) ) then
          call uubar_n1n1j_squaredME(fc_result, helicities, flags)
          goto 20
        !bb  -> neu neu jet
        elseif(((bflav(1).eq.-5 .and. bflav(2).eq.5) .or.
     &           (bflav(1).eq.5 .and. bflav(2).eq.-5)) .and. 
     &           (bflav(3).eq.1000022 .and. bflav(4).eq.1000022) ) then
          call ddbar_n1n1j_squaredME(fc_result, helicities, flags)
          goto 20
        !dg -> neu neu jet
        elseif(((bflav(1).eq.1 .and. bflav(2).eq.0) .or.
     &           (bflav(1).eq.0 .and. bflav(2).eq.1)) .and. 
     &           (bflav(3).eq.1000022 .and. bflav(4).eq.1000022) ) then
          call dg_n1n1j_squaredME(fc_result, helicities, flags)
          goto 20
        !ug -> neu neu jet
        elseif(((bflav(1).eq.2 .and. bflav(2).eq.0) .or.
     &           (bflav(1).eq.0 .and. bflav(2).eq.2)) .and. 
     &           (bflav(3).eq.1000022 .and. bflav(4).eq.1000022) ) then
          call ug_n1n1j_squaredME(fc_result, helicities, flags)
          goto 20
        !sg -> neu neu jet
        elseif(((bflav(1).eq.3 .and. bflav(2).eq.0) .or.
     &           (bflav(1).eq.0 .and. bflav(2).eq.3)) .and. 
     &           (bflav(3).eq.1000022 .and. bflav(4).eq.1000022) ) then
          call dg_n1n1j_squaredME(fc_result, helicities, flags)
          goto 20
        !cg -> neu neu jet
        elseif(((bflav(1).eq.4 .and. bflav(2).eq.0) .or.
     &           (bflav(1).eq.0 .and. bflav(2).eq.4)) .and. 
     &           (bflav(3).eq.1000022 .and. bflav(4).eq.1000022) ) then
          call ug_n1n1j_squaredME(fc_result, helicities, flags)
          goto 20
        !bg -> neu neu jet
        elseif(((bflav(1).eq.5 .and. bflav(2).eq.0) .or.
     &           (bflav(1).eq.0 .and. bflav(2).eq.5)) .and. 
     &           (bflav(3).eq.1000022 .and. bflav(4).eq.1000022) ) then
          call dg_n1n1j_squaredME(fc_result, helicities, flags)
          goto 20
        !dbar g -> neu neu jet
       elseif(((bflav(1).eq.-1 .and. bflav(2).eq.0) .or.
     &           (bflav(1).eq.0 .and. bflav(2).eq.-1)) .and. 
     &           (bflav(3).eq.1000022 .and. bflav(4).eq.1000022) ) then
          call dbarg_n1n1j_squaredME(fc_result, helicities, flags)
          goto 20
        !ubar g -> neu neu jet
        elseif(((bflav(1).eq.-2 .and. bflav(2).eq.0) .or.
     &           (bflav(1).eq.0 .and. bflav(2).eq.-2)) .and. 
     &           (bflav(3).eq.1000022 .and. bflav(4).eq.1000022) ) then
          call ubarg_n1n1j_squaredME(fc_result, helicities, flags)
          goto 20
        !sbar g -> neu neu jet
        elseif(((bflav(1).eq.-3 .and. bflav(2).eq.0) .or.
     &           (bflav(1).eq.0 .and. bflav(2).eq.-3)) .and. 
     &           (bflav(3).eq.1000022 .and. bflav(4).eq.1000022) ) then
          call dbarg_n1n1j_squaredME(fc_result, helicities, flags)
          goto 20
        !cbar g -> neu neu jet
        elseif(((bflav(1).eq.-4 .and. bflav(2).eq.0) .or.
     &           (bflav(1).eq.0 .and. bflav(2).eq.-4)) .and. 
     &           (bflav(3).eq.1000022 .and. bflav(4).eq.1000022) ) then
          call ubarg_n1n1j_squaredME(fc_result, helicities, flags)
          goto 20
        !bbar g -> neu neu jet
        elseif(((bflav(1).eq.-5 .and. bflav(2).eq.0) .or.
     &           (bflav(1).eq.0 .and. bflav(2).eq.-5)) .and. 
     &           (bflav(3).eq.1000022 .and. bflav(4).eq.1000022) ) then
          call dbarg_n1n1j_squaredME(fc_result, helicities, flags)
          goto 20