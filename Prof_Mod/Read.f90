!==============================================================================!
  subroutine Prof_Mod_Read(prf, flw, file_name)
!------------------------------------------------------------------------------!
!   Reading data from another simulations including mean flow and reynolds     !
!------------------------------------------------------------------------------!
!----------------------------------[Modules]-----------------------------------!
  use Flow_Mod, only: Flow_Type
  use Mesh_Mod, only: Mesh_Type
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  type(Prof_Type)  :: prf
  type(Flow_Type)  :: flw
  character(len=*) :: file_name
!-----------------------------------[Locals]-----------------------------------!
  integer                  :: i, j, k
  character(len=256)       :: header
  type(Mesh_Type), pointer :: msh
!==============================================================================!

  ! Take mesh pointer
  msh => flw % pnt_mesh

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

  do k = 1, msh % nz - 1
    do j = 1, msh % ny - 1
      flw % u_avg  % dns(j,k) = prf % u(j)
      flw % v_avg  % dns(j,k) = prf % v(j)
      flw % w_avg  % dns(j,k) = prf % w(j)
      flw % t_avg  % dns(j,k) = prf % t(j)

      flw % uu_avg % dns(j,k) = prf % rs(1,j)
      flw % vv_avg % dns(j,k) = prf % rs(2,j)
      flw % ww_avg % dns(j,k) = prf % rs(3,j)
      flw % uv_avg % dns(j,k) = prf % rs(4,j)
      flw % uw_avg % dns(j,k) = prf % rs(5,j)    ! ~ zero
      flw % vw_avg % dns(j,k) = prf % rs(6,j)    ! ~ zero

      flw % tt_avg % dns(j,k) = prf % ts(1,j)
      flw % ut_avg % dns(j,k) = prf % ts(2,j)
      flw % vt_avg % dns(j,k) = prf % ts(3,j)
      flw % wt_avg % dns(j,k) = prf % ts(4,j)    ! ~ zero
    end do
  end do

  end subroutine
