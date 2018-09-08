!==============================================================================!
  subroutine Prof_Mod_Allocate(pro, n)
!------------------------------------------------------------------------------!
!   Allocate memory for a variable defined on the specific mesh                !
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  type(Prof_Type) :: pro
  integer         :: n
!==============================================================================!

  pro % n_points = n

  allocate (pro % y(n))

  allocate (pro % u(n))
  allocate (pro % v(n))
  allocate (pro % w(n))
  allocate (pro % t(n))

  allocate (pro % rs(6, n))
  allocate (pro % ts(4, n))

  pro % y(:) = 0.0

  pro % u(:) = 0.0
  pro % v(:) = 0.0
  pro % w(:) = 0.0
  pro % t(:) = 0.0

  pro % rs(:,:) = 0.0
  pro % ts(:,:) = 0.0

  end subroutine
