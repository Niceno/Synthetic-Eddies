!==============================================================================!
  subroutine Prof_Mod_Create(prf, n)
!------------------------------------------------------------------------------!
!   Create memory for a variable defined on the specific mesh                !
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  type(Prof_Type) :: prf
  integer         :: n
!==============================================================================!

  ! Number of points in prescribed prffile
  prf % n_points = n

  allocate (prf % y(n));  prf % y(:) = 0.0

  allocate (prf % u(n));  prf % u(:) = 0.0
  allocate (prf % v(n));  prf % v(:) = 0.0
  allocate (prf % w(n));  prf % w(:) = 0.0
  allocate (prf % t(n));  prf % t(:) = 0.0

  allocate (prf % rs(6, n));  prf % rs(:,:) = 0.0
  allocate (prf % ts(4, n));  prf % ts(:,:) = 0.0

  end subroutine
