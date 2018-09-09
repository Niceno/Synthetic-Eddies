!==============================================================================!
  module Var_Mod
!------------------------------------------------------------------------------!
  use Mesh_Mod
!------------------------------------------------------------------------------!
  implicit none
!------------------------------------------------------------------------------!

  !--------------!
  !   Var type   !
  !--------------!
  type Var_Type

    type(Mesh_Type), pointer :: pnt_mesh  ! grid for which it is defined

    real, allocatable :: raw(:,:)         ! raw
    real, allocatable :: com(:,:)         ! combined
  end type

  contains

  include 'Var_Mod/Create.f90'

  end module
