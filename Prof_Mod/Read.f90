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
  integer           :: j, k
  character(len=80) :: header
!==============================================================================!

  write(*,*) '#=================================='
  write(*,*) '# Reading process started ...'

  open(100, file=file_name, form='formatted', status='old')
  read(100,*) header
  read(100,*) header

  !-------------------------------------!
  !   Main loop of reading slice data   !
  !-------------------------------------!
  do j = 1, prf % n_points
    read(100,*) prf % y(j),     &
                prf % u(j),     &
                prf % v(j),     &
                prf % w(j),     &
                prf % t(j),     &
                prf % rs(1,j),  &
                prf % rs(2,j),  &
                prf % rs(3,j),  &
                prf % rs(4,j),  &
                prf % rs(5,j),  &
                prf % rs(6,j),  &
                prf % ts(1,j),  &
                prf % ts(2,j),  &
                prf % ts(3,j),  &
                prf % ts(4,j)
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