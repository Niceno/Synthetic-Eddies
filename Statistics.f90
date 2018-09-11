!==============================================================================!
  subroutine Statistics(flw, ts)
!------------------------------------------------------------------------------!
!   Make a statistics about flows including mean and rms data                  !
!------------------------------------------------------------------------------!
!----------------------------------[Modules]-----------------------------------!
  use Flow_Mod, only: Flow_Type
  use Prof_Mod, only: Prof_Type
  use Mesh_Mod, only: Mesh_Type
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  type(Flow_Type) :: flw
  integer         :: ts
!-----------------------------------[Locals]-----------------------------------!
  integer                 :: c
  type(Mesh_Type), target :: msh
!==============================================================================!

  ! Take mesh pointer
  msh = flw % pnt_mesh

  !------------------------------!
  !   Time average over domain   !
  !------------------------------!
  do c = 1, msh % n_cells

    ! Mean: u, v, w, t
    flw % u_avg % com(c) = (  flw % u_avg % com(c)*(ts-1)    &
                            + flw % u     % com(c)) / ts
    flw % v_avg % com(c) = (  flw % v_avg % com(c)*(ts-1)    &
                            + flw % v     % com(c)) / ts
    flw % w_avg % com(c) = (  flw % w_avg % com(c)*(ts-1)    &
                            + flw % w     % com(c)) / ts
    flw % t_avg % com(c) = (  flw % t_avg % com(c)*(ts-1)    &
                            + flw % t     % com(c)) / ts

    ! Mean: uu, vv, ww, tt
    flw % uu_avg % com(c) = (   flw % uu_avg % com(c) * (ts-1)     &
                             + (flw % u      % com(c)              &
                              - flw % u_avg  % dns(c))**2  ) / ts

    flw % vv_avg % com(c) = (   flw % vv_avg % com(c) * (ts-1)     &
                             + (flw % v      % com(c)              &
                             -  flw % v_avg  % dns(c))**2  ) / ts

    flw % ww_avg % com(c) = (   flw % ww_avg % com(c) * (ts-1)     &
                             + (flw % w      % com(c)              &
                             -  flw % w_avg  % dns(c))**2  ) / ts

    flw % tt_avg % com(c) = (   flw % tt_avg % com(c) * (ts-1)     &
                             + (flw % t      % com(c)              &
                              - flw % t_avg  % dns(c))**2  ) / ts

    ! Mean: uv, uw, vw
    flw % uv_avg % com(c) = (    flw % uv_avg % com(c) * (ts-1)    &
                          + (   (flw % u      % com(c)             &
                               - flw % u_avg  % dns(c))            &
                              * (flw % v      % com(c)             &
                               - flw % v_avg  % dns(c)) ) ) / ts

    flw % uw_avg % com(c) = (    flw % uw_avg % com(c) * (ts-1)    &
                          + (   (flw % u      % com(c)             &
                               - flw % u_avg  % dns(c))            &
                              * (flw % w      % com(c)             &
                               - flw % w_avg  % dns(c)) ) ) / ts

    flw % vw_avg % com(c) = (    flw % vw_avg % com(c) * (ts-1)    &
                          + (   (flw % v      % com(c)             &
                               - flw % v_avg  % dns(c))            &
                              * (flw % w      % com(c)             &
                               - flw % w_avg  % dns(c)) ) ) / ts

    ! Mean: ut, vt, wt
    flw % ut_avg % com(c) = (    flw % ut_avg % com(c) * (ts-1)    &
                          + (   (flw % u      % com(c)             &
                               - flw % u_avg  % dns(c))            &
                              * (flw % t      % com(c)             &
                               - flw % t_avg  % dns(c)) ) ) / ts

    flw % vt_avg % com(c) = (    flw % vt_avg % com(c) * (ts-1)    &
                          + (   (flw % v      % com(c)             &
                               - flw % v_avg  % dns(c))            &
                              * (flw % t      % com(c)             &
                               - flw % t_avg  % dns(c)) ) ) / ts

    flw % wt_avg % com(c) = (    flw % wt_avg % com(c) * (ts-1)    &
                          + (   (flw % w      % com(c)             &
                               - flw % w_avg  % dns(c))            &
                              * (flw % t      % com(c)             &
                               - flw % t_avg  % dns(c)) ) ) / ts
  end do

  end subroutine
