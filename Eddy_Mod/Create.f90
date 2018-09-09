!==============================================================================!
  subroutine Eddy_Mod_Create(ed, n, sig)
!------------------------------------------------------------------------------!
!   Create memory for eddies                                                 !
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  type(Eddy_Type) :: ed
  integer         :: n
  real            :: sig
!==============================================================================!

  ! Store mesh for which the variable is defined
  ed % n_eddies = n
  ed % sigma    = sig

  allocate(ed % num  (n));  ed % num  (:) = 0    ! is this ever used?
  allocate(ed % len  (n));  ed % len  (:) = 0.0
  allocate(ed % x    (n));  ed % x    (:) = 0.0
  allocate(ed % y    (n));  ed % y    (:) = 0.0
  allocate(ed % z    (n));  ed % z    (:) = 0.0
  allocate(ed % x_int(n));  ed % x_int(:) = 0.0
  allocate(ed % y_int(n));  ed % y_int(:) = 0.0
  allocate(ed % z_int(n));  ed % z_int(:) = 0.0
  allocate(ed % t_int(n));  ed % t_int(:) = 0.0


  end subroutine
