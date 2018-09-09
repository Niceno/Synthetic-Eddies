!==============================================================================!
  module Prof_Mod
!------------------------------------------------------------------------------!
!   Variables read from the input profile                                      !
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

  include 'Prof_Mod/Create.f90'

  end module
