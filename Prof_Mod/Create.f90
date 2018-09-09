!==============================================================================!
  subroutine Prof_Mod_Create(pro, n)
!------------------------------------------------------------------------------!
!   Create memory for a variable defined on the specific mesh                !
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  type(Prof_Type) :: pro
  integer         :: n
!==============================================================================!

  ! Number of points in prescribed profile
  pro % n_points = n

  allocate (pro % y(n));  pro % y(:) = 0.0

  allocate (pro % u(n));  pro % u(:) = 0.0
  allocate (pro % v(n));  pro % v(:) = 0.0
  allocate (pro % w(n));  pro % w(:) = 0.0
  allocate (pro % t(n));  pro % t(:) = 0.0

  allocate (pro % rs(6, n));  pro % rs(:,:) = 0.0
  allocate (pro % ts(4, n));  pro % ts(:,:) = 0.0

  end subroutine
