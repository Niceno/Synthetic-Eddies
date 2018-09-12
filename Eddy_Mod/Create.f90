!==============================================================================!
  subroutine Eddy_Mod_Create(ed, msh, n, sig)
!------------------------------------------------------------------------------!
!   Create memory for eddies                                                 !
!------------------------------------------------------------------------------!
!----------------------------------[Modules]-----------------------------------!
  use Mesh_Mod, only: Mesh_Type
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  type(Eddy_Type) :: ed
  type(Mesh_Type) :: msh
  integer         :: n
  real            :: sig
!-----------------------------------[Locals]-----------------------------------!
  integer           :: e, random_cell
  real              :: y_s, y_e, z_s, z_e
  real, allocatable :: int_x(:), int_y(:),  &
                       int_z(:), int_t(:)
  real              :: tmp(1:3)
!==============================================================================!

  write(*,*) '#==================================='
  write(*,*) '# Eddy creation process started ... '

  allocate(int_x(n)); int_x(:) = 0.0
  allocate(int_y(n)); int_y(:) = 0.0
  allocate(int_z(n)); int_z(:) = 0.0
  allocate(int_t(n)); int_t(:) = 0.0

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

  ! Determine the bounding box for the eddies
  y_s = minval(msh % yn(:)) - ed % sigma
  y_e = maxval(msh % yn(:)) + ed % sigma
  z_s = minval(msh % zn(:)) - ed % sigma
  z_e = maxval(msh % zn(:)) + ed % sigma

  call random_number(int_x)
  call random_number(int_y)
  call random_number(int_z)
  call random_number(int_t)

  do e = 1, ed % n_eddies
    ed % num(e) = e
    ed % len(e) = ed % sigma

    call random_number(tmp)
    ed % x(e) = -ed % sigma + 2 * ed % sigma * tmp(1)

    random_cell = int( tmp(2) * real(msh % n_cells) )
    ed % y(e) = msh % yc(random_cell)
    ed % z(e) = msh % zc(random_cell)

    ed % x_int(e) = sign(1.0,int_x(e)-0.5)
    ed % y_int(e) = sign(1.0,int_y(e)-0.5)
    ed % z_int(e) = sign(1.0,int_z(e)-0.5)
    ed % t_int(e) = sign(1.0,int_t(e)-0.5)
  end do

  write(*,*) '# ... eddy creation process ended.'
  write(*,*) '#-----------------------------------'
  write(*,*) ''

  end subroutine
