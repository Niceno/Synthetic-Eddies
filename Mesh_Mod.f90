!==============================================================================!
  module Mesh_Mod
!------------------------------------------------------------------------------!
!   Module to define mesh type                                                 !
!                                                                              !
!   For the time being it is Cartesian, but could easily be generalized        !
!------------------------------------------------------------------------------!
  implicit none
!==============================================================================!

  !---------------!
  !               !
  !   Mesh type   !
  !               !
  !---------------!
  type Mesh_Type
    integer           :: ny       ! number of nodes in "y" direction
    integer           :: nz       ! number of nodes in "z" direction
    integer           :: n_nodes  ! number of nodes
    integer           :: n_cells  ! number of cells
    real, allocatable :: y(:)     ! "y" coordinates
    real, allocatable :: z(:)     ! "z" coordinates
  end type

  end module
