!==============================================================================!
  subroutine Statistics(ts)
!------------------------------------------------------------------------------!
!   Make a statistics about flows including mean and rms data                  !
!------------------------------------------------------------------------------!
!----------------------------------[Modules]-----------------------------------!
  use Sem_Mod
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  integer :: ts
!-----------------------------------[Locals]-----------------------------------!
  integer :: j, k
!==============================================================================!

  !------------------------------!
  !   Time average over domain   !
  !------------------------------!
  do j = 1, mesh % ny
    do k = 1, mesh % nz

      ! Mean: u, v, w, t
      u_avg % com(j,k) = (u_avg % com(j,k)*(ts-1) + u % com(j,k)) / ts
      v_avg % com(j,k) = (v_avg % com(j,k)*(ts-1) + v % com(j,k)) / ts
      w_avg % com(j,k) = (w_avg % com(j,k)*(ts-1) + w % com(j,k)) / ts
      t_avg % com(j,k) = (t_avg % com(j,k)*(ts-1) + t % com(j,k)) / ts

      ! Mean: uu, vv, ww, tt
      uu_avg % com(j,k) = (   uu_avg % com(j,k) * (ts-1)               &
                            + (u % com(j,k) - prof % u(j))**2  ) / ts
      vv_avg % com(j,k) = (   vv_avg % com(j,k) * (ts-1)               &
                            + (v % com(j,k) - prof % v(j))**2  ) / ts
      ww_avg % com(j,k) = (   ww_avg % com(j,k) * (ts-1)               &
                            + (w % com(j,k) - prof % w(j))**2  ) / ts
      tt_avg % com(j,k) = (   tt_avg % com(j,k) * (ts-1)               &
                            + (t % com(j,k) - prof % t(j))**2  ) / ts

      ! Mean: uv, uw, vw
      uv_avg % com(j,k) = (   uv_avg % com(j,k) * (ts-1)                 &
                            + (   (u % com(j,k) - prof % u(j))           &
                                * (v % com(j,k) - prof % v(j)) ) ) / ts
      uw_avg % com(j,k) = (   uw_avg % com(j,k) * (ts-1)                 &
                            + (   (u % com(j,k) - prof % u(j))           &
                                * (w % com(j,k) - prof % w(j)) ) ) / ts
      vw_avg % com(j,k) = (   vw_avg % com(j,k) * (ts-1)                 &
                            + (   (v % com(j,k) - prof % v(j))           &
                                * (w % com(j,k) - prof % w(j)) ) ) / ts

      ! Mean: ut, vt, wt
      ut_avg % com(j,k) = (   ut_avg % com(j,k) * (ts-1)                 &
                            + (   (u % com(j,k) - prof % u(j))           &
                                * (t % com(j,k) - prof % t(j)) ) ) / ts
      vt_avg % com(j,k) = (   vt_avg % com(j,k) * (ts-1)                 &
                            + (   (v % com(j,k) - prof % v(j))           &
                                * (t % com(j,k) - prof % t(j)) ) ) / ts
      wt_avg % com(j,k) = (   wt_avg % com(j,k) * (ts-1)                 &
                            + (   (w % com(j,k) - prof % w(j))           &
                                * (t % com(j,k) - prof % t(j)) ) ) / ts

    end do


  end do

  end subroutine
