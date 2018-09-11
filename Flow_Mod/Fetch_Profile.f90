!==============================================================================!
  subroutine Flow_Mod_Fetch_Profile(flw, prf)
!------------------------------------------------------------------------------!
!   Copy data read by profile "prf" into the flow "flw"                        !
!------------------------------------------------------------------------------!
!----------------------------------[Modules]-----------------------------------!
  use Mesh_Mod, only: Mesh_Type, STRUCTURED, UNSTRUCTURED
  use Prof_Mod, only: Prof_Type
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  type(Flow_Type)  :: flw
  type(Prof_Type)  :: prf
!-----------------------------------[Locals]-----------------------------------!
  integer                  :: jm, jp, c
  real                     :: wm, wp
  type(Mesh_Type), pointer :: msh
!==============================================================================!

  ! Take mesh pointer
  msh => flw % pnt_mesh

  !----------------------------------------------------------------!
  !   Browse through all cells, find interpolation points on the   !
  !     profile and fetch the values prescribed in the profile     !
  !----------------------------------------------------------------!
  do c = 1, msh % n_cells

    ! Find closest profile
    do jm = 1, prf % n_points - 1
      jp = jm + 1
      if( prf % y(jm) <= msh % yc(c) .and.  &
          prf % y(jp) >= msh % yc(c)) then
        wp = (msh % yc(c) - prf % y(jm)) / (prf % y(jp) - prf % y(jm))
        wm = (prf % y(jp) - msh % yc(c)) / (prf % y(jp) - prf % y(jm))
        goto 1
      end if
    end do

    print *, '# PANIC!  Didn''t find the point!'

    ! At this point, you will have jm, jp, wm and wp
1   continue

    ! Interpolate values
    flw % u_avg  % dns(c) = wm * prf % u(jm) +      &
                            wp * prf % u(jp)        !  2  u
    flw % v_avg  % dns(c) = wm * prf % v(jm) +      &
                            wp * prf % v(jp)        !  3  v ~ zero
    flw % w_avg  % dns(c) = wm * prf % w(jm) +      &
                            wp * prf % w(jp)        !  4  w ~ zero
    flw % t_avg  % dns(c) = wm * prf % t(jm) +      &
                            wp * prf % t(jp)        !  5  t

    flw % uu_avg  % dns(c) = wm * prf % rs(1,jm) +  &
                             wp * prf % rs(1,jp)    !  6  uu
    flw % vv_avg  % dns(c) = wm * prf % rs(2,jm) +  &
                             wp * prf % rs(2,jp)    !  7  vv
    flw % ww_avg  % dns(c) = wm * prf % rs(3,jm) +  &
                             wp * prf % rs(3,jp)    !  8  ww
    flw % uv_avg  % dns(c) = wm * prf % rs(4,jm) +  &
                             wp * prf % rs(4,jp)    !  9  uv
    flw % uw_avg  % dns(c) = wm * prf % rs(5,jm) +  &
                             wp * prf % rs(5,jp)    ! 10  uw ~ zero
    flw % vw_avg  % dns(c) = wm * prf % rs(6,jm) +  &
                             wp * prf % rs(6,jp)    ! 11  vw ~ zero

    flw % tt_avg  % dns(c) = wm * prf % ts(1,jm) +  &
                             wp * prf % ts(1,jp)    ! 12  tt
    flw % ut_avg  % dns(c) = wm * prf % ts(2,jm) +  &
                             wp * prf % ts(2,jp)    ! 13  ut
    flw % vt_avg  % dns(c) = wm * prf % ts(3,jm) +  &
                             wp * prf % ts(3,jp)    ! 14  vt
    flw % wt_avg  % dns(c) = wm * prf % ts(4,jm) +  &
                             wp * prf % ts(4,jp)    ! 15  wt ~ zero
  end do

  end subroutine
