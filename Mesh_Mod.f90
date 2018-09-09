!==============================================================================!
  module Mesh_Mod
!------------------------------------------------------------------------------!
!   Module to define mesh type                                                 !
!                                                                              !
!   For the time being it is Cartesian, but could easily be generalized        !
!------------------------------------------------------------------------------!
  implicit none
!==============================================================================!

  ! Mesh mode
  integer, parameter :: STRUCTURED   = 70001
  integer, parameter :: UNSTRUCTURED = 70003

  !---------------!
  !               !
  !   Mesh type   !
  !               !
  !---------------!
  type Mesh_Type
    integer              :: mode
    integer              :: ny                ! number of nodes in "y" direction
    integer              :: nz                ! number of nodes in "z" direction
    integer              :: n_nodes           ! number of nodes
    integer              :: n_cells           ! number of cells
    integer, allocatable :: cells_nodes(:,:)  ! cells' nodes
    real,    allocatable :: yn(:)             ! nodes' "y" coordinates
    real,    allocatable :: zn(:)             ! nodes' "z" coordinates
    real,    allocatable :: yc(:)             ! cells' "y" coordinates
    real,    allocatable :: zc(:)             ! cells' "z" coordinates
  end type

  contains

  include 'Mesh_Mod/Create_Cartesian.f90'
  include 'Mesh_Mod/Create_From_File.f90'

  end module
