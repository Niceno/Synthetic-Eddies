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
  integer                 :: j, k
  type(Mesh_Type), target :: msh
!==============================================================================!

  ! Take mesh pointer
  msh = flw % pnt_mesh

  !------------------------------!
  !   Time average over domain   !
  !------------------------------!
  do j = 1, msh % ny - 1
    do k = 1, msh % nz - 1

      ! Mean: u, v, w, t
      flw % u_avg % com(At(j,k)) = (  flw % u_avg % com(At(j,k))*(ts-1)  &
                                    + flw % u     % com(At(j,k))) / ts
      flw % v_avg % com(At(j,k)) = (  flw % v_avg % com(At(j,k))*(ts-1)  &
                                    + flw % v     % com(At(j,k))) / ts
      flw % w_avg % com(At(j,k)) = (  flw % w_avg % com(At(j,k))*(ts-1)  &
                                    + flw % w     % com(At(j,k))) / ts
      flw % t_avg % com(At(j,k)) = (  flw % t_avg % com(At(j,k))*(ts-1)  &
                                    + flw % t     % com(At(j,k))) / ts

      ! Mean: uu, vv, ww, tt
      flw % uu_avg % com(At(j,k)) = (   flw % uu_avg % com(At(j,k)) * (ts-1)   &
                                     + (flw % u      % com(At(j,k))            &
                                      - flw % u_avg  % dns(At(j,k)))**2  ) / ts

      flw % vv_avg % com(At(j,k)) = (   flw % vv_avg % com(At(j,k)) * (ts-1)   &
                                     + (flw % v      % com(At(j,k))            &
                                     -  flw % v_avg  % dns(At(j,k)))**2  ) / ts

      flw % ww_avg % com(At(j,k)) = (   flw % ww_avg % com(At(j,k)) * (ts-1)   &
                                     + (flw % w      % com(At(j,k))            &
                                     -  flw % w_avg  % dns(At(j,k)))**2  ) / ts

      flw % tt_avg % com(At(j,k)) = (   flw % tt_avg % com(At(j,k)) * (ts-1)   &
                                     + (flw % t      % com(At(j,k))            &
                                      - flw % t_avg  % dns(At(j,k)))**2  ) / ts

      ! Mean: uv, uw, vw
      flw % uv_avg % com(At(j,k)) = (    flw % uv_avg % com(At(j,k)) * (ts-1)  &
                                  + (   (flw % u      % com(At(j,k))           &
                                       - flw % u_avg  % dns(At(j,k)))          &
                                      * (flw % v      % com(At(j,k))           &
                                       - flw % v_avg  % dns(At(j,k))) ) ) / ts

      flw % uw_avg % com(At(j,k)) = (    flw % uw_avg % com(At(j,k)) * (ts-1)  &
                                  + (   (flw % u      % com(At(j,k))           &
                                       - flw % u_avg  % dns(At(j,k)))          &
                                      * (flw % w      % com(At(j,k))           &
                                       - flw % w_avg  % dns(At(j,k))) ) ) / ts

      flw % vw_avg % com(At(j,k)) = (    flw % vw_avg % com(At(j,k)) * (ts-1)  &
                                  + (   (flw % v      % com(At(j,k))           &
                                       - flw % v_avg  % dns(At(j,k)))          &
                                      * (flw % w      % com(At(j,k))           &
                                       - flw % w_avg  % dns(At(j,k))) ) ) / ts

      ! Mean: ut, vt, wt
      flw % ut_avg % com(At(j,k)) = (    flw % ut_avg % com(At(j,k)) * (ts-1)  &
                                  + (   (flw % u      % com(At(j,k))           &
                                       - flw % u_avg  % dns(At(j,k)))          &
                                      * (flw % t      % com(At(j,k))           &
                                       - flw % t_avg  % dns(At(j,k))) ) ) / ts

      flw % vt_avg % com(At(j,k)) = (    flw % vt_avg % com(At(j,k)) * (ts-1)  &
                                  + (   (flw % v      % com(At(j,k))           &
                                       - flw % v_avg  % dns(At(j,k)))          &
                                      * (flw % t      % com(At(j,k))           &
                                       - flw % t_avg  % dns(At(j,k))) ) ) / ts

      flw % wt_avg % com(At(j,k)) = (    flw % wt_avg % com(At(j,k)) * (ts-1)  &
                                  + (   (flw % w      % com(At(j,k))           &
                                       - flw % w_avg  % dns(At(j,k)))          &
                                      * (flw % t      % com(At(j,k))           &
                                       - flw % t_avg  % dns(At(j,k))) ) ) / ts
    end do
  end do

  contains

  include 'At.f90'

  end subroutine
