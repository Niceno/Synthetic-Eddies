!==============================================================================!
  subroutine Flow_Mod_Create(flw, msh)
!------------------------------------------------------------------------------!
!----------------------------------[Modules]-----------------------------------!
  use Mesh_Mod
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  type(Flow_Type)         :: flw
  type(Mesh_Type), target :: msh
!==============================================================================!

  write(*,*) '#==================================='
  write(*,*) '# Flow creation process started ... '

  ! Store mesh for which the variable is defined
  flw % pnt_mesh => msh

  !---------------------------------------!
  !   Variables holding unsteady values   !
  !---------------------------------------!
  call Var_Mod_Create(flw % u, msh)
  call Var_Mod_Create(flw % v, msh)
  call Var_Mod_Create(flw % w, msh)
  call Var_Mod_Create(flw % t, msh)

  !--------------------------------------------!
  !   Variables holding time-averaged values   !
  !--------------------------------------------!
  call Var_Mod_Create(flw % u_avg, msh)
  call Var_Mod_Create(flw % v_avg, msh)
  call Var_Mod_Create(flw % w_avg, msh)
  call Var_Mod_Create(flw % t_avg, msh)

  call Var_Mod_Create(flw % uu_avg, msh)
  call Var_Mod_Create(flw % vv_avg, msh)
  call Var_Mod_Create(flw % ww_avg, msh)
  call Var_Mod_Create(flw % uv_avg, msh)
  call Var_Mod_Create(flw % uw_avg, msh)
  call Var_Mod_Create(flw % vw_avg, msh)

  call Var_Mod_Create(flw % tt_avg, msh)
  call Var_Mod_Create(flw % ut_avg, msh)
  call Var_Mod_Create(flw % vt_avg, msh)
  call Var_Mod_Create(flw % wt_avg, msh)

  write(*,*) '# ... eddy creation process ended.'
  write(*,*) '#-----------------------------------'
  write(*,*) ''

  end subroutine
