!==============================================================================!
  module Flow_Mod
!------------------------------------------------------------------------------!
!   Module for Synthetic Eddy Method inflow generator                                            !
!------------------------------------------------------------------------------!
!----------------------------------[Modules]-----------------------------------!
  use Var_Mod
!------------------------------------------------------------------------------!
  implicit none
!==============================================================================!

  !---------------!
  !               !
  !   Flow type   !
  !               !
  !---------------!
  type Flow_Type

    type(Mesh_Type), pointer :: pnt_mesh  ! grid for which it is defined

    ! Unsteady computed quantities
    type(Var_Type)  :: u, v, w, t

    ! Time averaged quantities
    type(Var_Type)  :: u_avg,  v_avg,  w_avg,  t_avg
    type(Var_Type)  :: uu_avg, vv_avg, ww_avg
    type(Var_Type)  :: uv_avg, uw_avg, vw_avg
    type(Var_Type)  :: tt_avg, ut_avg, vt_avg, wt_avg

  end type

  contains

  include 'Flow_Mod/Create.f90'

  end module
