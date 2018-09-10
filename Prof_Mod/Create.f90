!==============================================================================!
  subroutine Prof_Mod_Create(prf, profile_file_name)
!------------------------------------------------------------------------------!
!   Create memory for a variable defined on the specific mesh                !
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  type(Prof_Type)  :: prf
  character(len=*) :: profile_file_name
!-----------------------------------[Locals]-----------------------------------!
  integer            :: j
  character(len=256) :: header
!------------------------------[Local parameters]------------------------------!
  integer, parameter :: PFU = 100
!==============================================================================!

  write(*,*) '#=================================='
  write(*,*) '# Profile reading started ...'

  !-----------------!
  !   Skip header   !
  !-----------------!
  open(PFU, file=profile_file_name, form='formatted', status='old')
  do j = 1, 7
    read(PFU,*) header
  end do

  !-------------------------------------!
  !   Allocate memory for the profile   !
  !-------------------------------------!

  ! Number of points in the profile
  read(PFU, *) prf % n_points

  ! Coordinate (should be distance from the wall one day
  allocate (prf % y(prf % n_points));  prf % y(:) = 0.0

  ! First moments
  allocate (prf % u(prf % n_points));  prf % u(:) = 0.0
  allocate (prf % v(prf % n_points));  prf % v(:) = 0.0
  allocate (prf % w(prf % n_points));  prf % w(:) = 0.0
  allocate (prf % t(prf % n_points));  prf % t(:) = 0.0

  ! Second moments
  allocate (prf % rs(6, prf % n_points));  prf % rs(:,:) = 0.0
  allocate (prf % ts(4, prf % n_points));  prf % ts(:,:) = 0.0

  !---------------------!
  !   Read slice data   !
  !---------------------!
  do j = 1, prf % n_points
    read(PFU,*) prf % y(j),     &    !  1  y 
                prf % u(j),     &    !  2  u
                prf % v(j),     &    !  3  v  ~ zero
                prf % w(j),     &    !  4  w  ~ zero
                prf % t(j),     &    !  5  t
                prf % rs(1,j),  &    !  6  uu
                prf % rs(2,j),  &    !  7  vv
                prf % rs(3,j),  &    !  8  ww
                prf % rs(4,j),  &    !  9  uv
                prf % rs(5,j),  &    ! 10  uw ~ zero
                prf % rs(6,j),  &    ! 11  vw ~ zero
                prf % ts(1,j),  &    ! 12  tt
                prf % ts(2,j),  &    ! 13  ut
                prf % ts(3,j),  &    ! 14  vt
                prf % ts(4,j)        ! 15  wt ~ zero
  end do

  write(*,*) '# ... profile reading is completed'
  write(*,*) '#----------------------------------'
  write(*,*) ''

  close(PFU)

  end subroutine
