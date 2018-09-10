!==============================================================================!
  subroutine Scale_Fluctuations(flw, ts)
!------------------------------------------------------------------------------!
!   Combine slice mean,rms data with generated fluctuation variables           !
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
  integer                 :: i, j, k
  real                    :: r_loc(4,4), a(4,4),        &
                             u_ins(4,1), u_mean(4,1),   &
                             u_fluc(4,1), u_tmp(4,1)
  character(len=80)       :: file_name
  type(Mesh_Type), target :: msh
!==============================================================================!

  ! Take mesh pointer
  msh = flw % pnt_mesh

  ! Browse through all cells
  do k = 1, msh % nz - 1
    do j = 1, msh % ny - 1
      a(1:4,1:4)     = 0.0
      r_loc(1:4,1:4) = 0.0
      u_ins(1:4,1)   = 0.0
      u_fluc(1:4,1)  = 0.0
      u_mean(1:4,1)  = 0.0
      u_tmp(1:4,1)   = 0.0

      u_mean(1:4,1) = (/ flw % u_avg % dns(At(j,k)),   &
                         flw % v_avg % dns(At(j,k)),   &
                         flw % w_avg % dns(At(j,k)),   &
                         flw % t_avg % dns(At(j,k))  /)
      u_tmp (1:4,1) = (/ flw % u % raw(At(j,k)),  &
                         flw % v % raw(At(j,k)),  &
                         flw % w % raw(At(j,k)),  &
                         flw % t % raw(At(j,k)) /)

      r_loc(1,1:4)  = (/ flw % uu_avg % dns(At(j,k)),  &
                         flw % uv_avg % dns(At(j,k)),  &
                         flw % uw_avg % dns(At(j,k)),  &
                         flw % ut_avg % dns(At(j,k)) /)
      r_loc(2,1:4)  = (/ flw % uv_avg % dns(At(j,k)),  &
                         flw % vv_avg % dns(At(j,k)),  &
                         flw % vw_avg % dns(At(j,k)),  &
                         flw % vt_avg % dns(At(j,k)) /)
      r_loc(3,1:4)  = (/ flw % uw_avg % dns(At(j,k)),  &
                         flw % vw_avg % dns(At(j,k)),  &
                         flw % ww_avg % dns(At(j,k)),  &
                         flw % wt_avg % dns(At(j,k)) /)
      r_loc(4,1:4)  = (/ flw % ut_avg % dns(At(j,k)),  &
                         flw % vt_avg % dns(At(j,k)),  &
                         flw % wt_avg % dns(At(j,k)),  &
                         flw % tt_avg % dns(At(j,k)) /)

      call Cholesky(a,r_loc,4)
      call Mat_Mul(a,u_tmp,u_fluc,4,1,4)

      u_ins(1:4,1) = u_mean(1:4,1) + u_fluc(1:4,1)

      flw % u % com(At(j,k)) = u_ins(1,1)
      flw % v % com(At(j,k)) = u_ins(2,1)
      flw % w % com(At(j,k)) = u_ins(3,1)
      flw % t % com(At(j,k)) = u_ins(4,1)

    end do
  end do

  contains

  include 'At.f90'

  end subroutine
