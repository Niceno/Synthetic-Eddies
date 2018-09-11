!==============================================================================!
  subroutine Scale_Fluctuations(flw)
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
!-----------------------------------[Locals]-----------------------------------!
  integer                 :: c
  real                    :: r_loc(4,4), a(4,4),        &
                             u_ins(4), u_mean(4),   &
                             u_fluc(4), u_tmp(4)
  type(Mesh_Type), target :: msh
!==============================================================================!

  ! Take mesh pointer
  msh = flw % pnt_mesh

  ! Browse through all cells
  do c = 1, msh % n_cells

    a     (:,:) = 0.0
    r_loc (:,:) = 0.0
    u_ins (:)   = 0.0
    u_fluc(:)   = 0.0
    u_mean(:)   = 0.0
    u_tmp (:)   = 0.0

    u_mean(1:4) = (/ flw % u_avg % dns(c),     &
                     flw % v_avg % dns(c),     &
                     flw % w_avg % dns(c),     &
                     flw % t_avg % dns(c)  /)
    u_tmp (1:4) = (/ flw % u % raw(c),         &
                     flw % v % raw(c),         &
                     flw % w % raw(c),         &
                     flw % t % raw(c) /)

    r_loc(1,1:4)  = (/ flw % uu_avg % dns(c),    &
                       flw % uv_avg % dns(c),    &
                       flw % uw_avg % dns(c),    &
                       flw % ut_avg % dns(c) /)
    r_loc(2,1:4)  = (/ flw % uv_avg % dns(c),    &
                       flw % vv_avg % dns(c),    &
                       flw % vw_avg % dns(c),    &
                       flw % vt_avg % dns(c) /)
    r_loc(3,1:4)  = (/ flw % uw_avg % dns(c),    &
                       flw % vw_avg % dns(c),    &
                       flw % ww_avg % dns(c),    &
                       flw % wt_avg % dns(c) /)
    r_loc(4,1:4)  = (/ flw % ut_avg % dns(c),    &
                       flw % vt_avg % dns(c),    &
                       flw % wt_avg % dns(c),    &
                       flw % tt_avg % dns(c) /)

    call Cholesky(a, r_loc, 4)
    call Mat_Mul(a, u_tmp, u_fluc, 4, 1, 4)

    u_ins(1:4) = u_mean(1:4) + u_fluc(1:4)

    flw % u % com(c) = u_ins(1)
    flw % v % com(c) = u_ins(2)
    flw % w % com(c) = u_ins(3)
    flw % t % com(c) = u_ins(4)

  end do

  end subroutine
