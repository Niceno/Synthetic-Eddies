!==============================================================================!
  subroutine Prof_Mod_Read(prf, msh, file_name)
!------------------------------------------------------------------------------!
!   Reading data from another simulations including mean flow and reynolds     !
!------------------------------------------------------------------------------!
!----------------------------------[Modules]-----------------------------------!
  use Mesh_Mod, only: Mesh_Type
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  type(Prof_Type)  :: prf
  type(Mesh_Type)  :: msh
  character(len=*) :: file_name
!-----------------------------------[Locals]-----------------------------------!
  integer            :: i, j, k
  character(len=256) :: header
!==============================================================================!

  write(*,*) '#=================================='
  write(*,*) '# Reading process started ...'

  open(100, file=file_name, form='formatted', status='old')
  do i = 1, 7
    read(100,*) header
  end do

  !-------------------------------------!
  !   Main loop of reading slice data   !
  !-------------------------------------!
  do j = 1, prf % n_points
    read(100,*) prf % y(j),     &    !  1  y 
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

  close(100)

  write(*,*) '# ... reading process is completed'
  write(*,*) '#----------------------------------'
  write(*,*) ''

  ! Finish defining node coordinates; very dirty
  msh % yn(1)        = 0.0
  msh % yn(msh % ny) = 2.0

  ! Just copy cell coordinates from the profile
  if(msh % mode == STRUCTURED) then
    do j = 1, msh % ny - 1
      msh % yc(j) = prf % y(j)  ! very dirty
    end do
  end if

  ! Interpolate node coordinates from cell coordinates
  do j = 2, msh % ny - 1
    msh % yn(j) = ( msh % yc(j-1) + msh % yc(j) ) * 0.5
  end do

  end subroutine
