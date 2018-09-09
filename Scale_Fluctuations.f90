!==============================================================================!
  subroutine Scale_Fluctuations(ts)
!------------------------------------------------------------------------------!
!   Combine slice mean,rms data with generated fluctuation variables           !
!------------------------------------------------------------------------------!
!----------------------------------[Modules]-----------------------------------!
  use Sem_Mod
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  integer :: ts
!-----------------------------------[Locals]-----------------------------------!
  integer           :: i, j, k
  real              :: r_loc(4,4), a(4,4),                               &
                       u_ins(4,1), u_mean(4,1), u_fluc(4,1), u_tmp(4,1)
  character(len=80) :: file_name
!==============================================================================!

  do k = 1, mesh % nz
    do j = 1, mesh % ny
      a(1:4,1:4)     = 0.0
      r_loc(1:4,1:4) = 0.0
      u_ins(1:4,1)   = 0.0
      u_fluc(1:4,1)  = 0.0
      u_mean(1:4,1)  = 0.0
      u_tmp(1:4,1)   = 0.0

      u_mean(1:4,1) = (/ prof % u(j),  prof % v(j),  prof % w(j),  prof % t(j)  /)
      u_tmp(1:4,1)  = (/ u % raw(j,k), v % raw(j,k), w % raw(j,k), t % raw(j,k) /)

      r_loc(1,1:4)  = (/ prof % rs(1,j), prof % rs(4,j), prof % rs(5,j), prof % ts(2,j) /)
      r_loc(2,1:4)  = (/ prof % rs(4,j), prof % rs(2,j), prof % rs(6,j), prof % ts(3,j) /)
      r_loc(3,1:4)  = (/ prof % rs(5,j), prof % rs(6,j), prof % rs(3,j), prof % ts(4,j) /)
      r_loc(4,1:4)  = (/ prof % ts(2,j), prof % ts(3,j), prof % ts(4,j), prof % ts(1,j) /)

      call Cholesky(a,r_loc,4)
      call Mat_Mul(a,u_tmp,u_fluc,4,1,4)

      u_ins(1:4,1) = u_mean(1:4,1) + u_fluc(1:4,1)

      u % com(j,k) = u_ins(1,1)
      v % com(j,k) = u_ins(2,1)
      w % com(j,k) = u_ins(3,1)
      t % com(j,k) = u_ins(4,1)

    end do
  end do

  if( mod(ts,10) .eq. 0) then
    call Save_Vtk_4_Arrays('scaled-velocities', u % com, v % com, w % com,  &
                                                t % com, ts)
  end if

  end subroutine
