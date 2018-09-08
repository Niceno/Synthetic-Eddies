!==============================================================================!
  subroutine Statistics
!------------------------------------------------------------------------------!
!   Make a statistics about flows including mean and rms data                  !
!------------------------------------------------------------------------------!
!----------------------------------[Modules]-----------------------------------!
  use Sem_Mod
!------------------------------------------------------------------------------!
  implicit none
!-----------------------------------[Locals]-----------------------------------!
  integer :: it,j,k,tt
  real    :: u_tmp(4,1:ny), rms_tmp(8,1:ny)
!==============================================================================!

  tt = int(time / dt)
  u_tmp(1:4,1:ny)   = 0.0
  rms_tmp(1:8,1:ny) = 0.0

  do j = 1,ny
    do k = 1,nz
      u_tmp(1,j) = u_tmp(1,j) + u_comb(j,k)
      u_tmp(2,j) = u_tmp(2,j) + v_comb(j,k)
      u_tmp(3,j) = u_tmp(3,j) + w_comb(j,k)
      u_tmp(4,j) = u_tmp(4,j) + t_comb(j,k)

      rms_tmp(1,j) = rms_tmp(1,j) + (u_comb(j,k) - u(j,k))**2
      rms_tmp(2,j) = rms_tmp(2,j) + (v_comb(j,k) - v(j,k))**2
      rms_tmp(3,j) = rms_tmp(3,j) + (w_comb(j,k) - w(j,k))**2
      rms_tmp(4,j) = rms_tmp(4,j) + (t_comb(j,k) - t(j,k))**2
      rms_tmp(5,j) = rms_tmp(5,j) +                                 &
                     (u_comb(j,k) - u(j,k))*(v_comb(j,k) - v(j,k))
      rms_tmp(6,j) = rms_tmp(6,j) +                                 &
                     (u_comb(j,k) - u(j,k))*(t_comb(j,k) - t(j,k))
      rms_tmp(7,j) = rms_tmp(7,j) +                                 &
                     (v_comb(j,k) - v(j,k))*(t_comb(j,k) - t(j,k))
      rms_tmp(8,j) = rms_tmp(8,j) +                                 &
                     (w_comb(j,k) - w(j,k))*(t_comb(j,k) - t(j,k))
    end do

    u_tmp(1:4,j) = u_tmp(1:4,j)/nz
    u_pr(1:4,j)  = ( u_pr(1:4,j) * (tt - 1) + u_tmp(1:4,j) )/tt

    rms_tmp(1:8,j) = rms_tmp(1:8,j)/nz
    rms_pr(1:8,j) = ( rms_pr(1:8,j) * (tt - 1) + rms_tmp(1:8,j) )/tt

  end do

  end subroutine
