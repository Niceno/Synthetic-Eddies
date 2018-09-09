!==============================================================================!
  subroutine Statistics(flw, prf, ts)
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
  type(Prof_Type) :: prf
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
  do j = 1, msh % ny
    do k = 1, msh % nz

      ! Mean: u, v, w, t
      flw % u_avg % com(j,k) = (  flw % u_avg % com(j,k)*(ts-1)  &
                                + flw % u     % com(j,k)) / ts
      flw % v_avg % com(j,k) = (  flw % v_avg % com(j,k)*(ts-1)  &
                                + flw % v     % com(j,k)) / ts
      flw % w_avg % com(j,k) = (  flw % w_avg % com(j,k)*(ts-1)  &
                                + flw % w     % com(j,k)) / ts
      flw % t_avg % com(j,k) = (  flw % t_avg % com(j,k)*(ts-1)  &
                                + flw % t     % com(j,k)) / ts

      ! Mean: uu, vv, ww, tt
      flw % uu_avg % com(j,k) = (   flw % uu_avg % com(j,k) * (ts-1)           &
                                 + (flw % u % com(j,k) - prf % u(j))**2  ) / ts
      flw % vv_avg % com(j,k) = (   flw % vv_avg % com(j,k) * (ts-1)           &
                                 + (flw % v % com(j,k) - prf % v(j))**2  ) / ts
      flw % ww_avg % com(j,k) = (   flw % ww_avg % com(j,k) * (ts-1)           &
                                 + (flw % w % com(j,k) - prf % w(j))**2  ) / ts
      flw % tt_avg % com(j,k) = (   flw % tt_avg % com(j,k) * (ts-1)           &
                                 + (flw % t % com(j,k) - prf % t(j))**2  ) / ts

      ! Mean: uv, uw, vw
      flw % uv_avg % com(j,k) = (    flw % uv_avg % com(j,k) * (ts-1)          &
                              + (   (flw % u % com(j,k) - prf % u(j))          &
                                  * (flw % v % com(j,k) - prf % v(j)) ) ) / ts
      flw % uw_avg % com(j,k) = (    flw % uw_avg % com(j,k) * (ts-1)          &
                              + (   (flw % u % com(j,k) - prf % u(j))          &
                                  * (flw % w % com(j,k) - prf % w(j)) ) ) / ts
      flw % vw_avg % com(j,k) = (    flw % vw_avg % com(j,k) * (ts-1)          &
                              + (   (flw % v % com(j,k) - prf % v(j))          &
                                  * (flw % w % com(j,k) - prf % w(j)) ) ) / ts

      ! Mean: ut, vt, wt
      flw % ut_avg % com(j,k) = (    flw % ut_avg % com(j,k) * (ts-1)          &
                              + (   (flw % u % com(j,k) - prf % u(j))          &
                                  * (flw % t % com(j,k) - prf % t(j)) ) ) / ts
      flw % vt_avg % com(j,k) = (    flw % vt_avg % com(j,k) * (ts-1)          &
                              + (   (flw % v % com(j,k) - prf % v(j))          &
                                  * (flw % t % com(j,k) - prf % t(j)) ) ) / ts
      flw % wt_avg % com(j,k) = (    flw % wt_avg % com(j,k) * (ts-1)          &
                              + (   (flw % w % com(j,k) - prf % w(j))          &
                                  * (flw % t % com(j,k) - prf % t(j)) ) ) / ts
    end do


  end do

  end subroutine
