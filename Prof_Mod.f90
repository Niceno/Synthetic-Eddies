!==============================================================================!
  module Prof_Mod
!------------------------------------------------------------------------------!
  use Mesh_Mod
!------------------------------------------------------------------------------!
  implicit none
!------------------------------------------------------------------------------!

  !---------------!
  !   Prof type   !
  !---------------!
  type Prof_Type
    integer :: n_points

    real, allocatable :: y(:)     ! should be distance from the wall

    real, allocatable :: u(:)
    real, allocatable :: v(:)
    real, allocatable :: w(:)
    real, allocatable :: t(:)

    real, allocatable :: rs(:,:)  ! reynolds stresses
    real, allocatable :: ts(:,:)  ! thermal stresses
  end type

  contains

  include 'Prof_Mod/Allocate.f90'

  end module
