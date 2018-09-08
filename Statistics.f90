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
  integer :: it,j,k
  real    :: u_tmp(4,1:mesh % ny), rms_tmp(8,1:mesh % ny)
!==============================================================================!

  u_tmp(1:4,:)   = 0.0
  rms_tmp(1:8,:) = 0.0

  do j = 1, mesh % ny
    do k = 1, mesh % nz
      u_tmp(1,j) = u_tmp(1,j) + u % com(j,k)
      u_tmp(2,j) = u_tmp(2,j) + v % com(j,k)
      u_tmp(3,j) = u_tmp(3,j) + w % com(j,k)
      u_tmp(4,j) = u_tmp(4,j) + t % com(j,k)

      rms_tmp(1,j) = rms_tmp(1,j) + (u % com(j,k) - u % pro(j,k))**2
      rms_tmp(2,j) = rms_tmp(2,j) + (v % com(j,k) - v % pro(j,k))**2
      rms_tmp(3,j) = rms_tmp(3,j) + (w % com(j,k) - w % pro(j,k))**2
      rms_tmp(4,j) = rms_tmp(4,j) + (t % com(j,k) - t % pro(j,k))**2

      rms_tmp(5,j) = rms_tmp(5,j)                   &
                   + (u % com(j,k) - u % pro(j,k))  &
                   * (v % com(j,k) - v % pro(j,k))
      rms_tmp(6,j) = rms_tmp(6,j)                   &
                   + (u % com(j,k) - u % pro(j,k))  &
                   * (t % com(j,k) - t % pro(j,k))
      rms_tmp(7,j) = rms_tmp(7,j)                   &
                   + (v % com(j,k) - v % pro(j,k))  &
                   * (t % com(j,k) - t % pro(j,k))
      rms_tmp(8,j) = rms_tmp(8,j)                   &
                   + (w % com(j,k) - w % pro(j,k))  &
                   * (t % com(j,k) - t % pro(j,k))
    end do

    u_tmp(1:4,j) = u_tmp(1:4,j)/mesh % nz
    u_pr(1:4,j)  = ( u_pr(1:4,j) * (ts - 1) + u_tmp(1:4,j) )/ts

    rms_tmp(1:8,j) = rms_tmp(1:8,j)/mesh % nz
    rms_pr(1:8,j) = ( rms_pr(1:8,j) * (ts - 1) + rms_tmp(1:8,j) )/ts

  end do

  end subroutine
