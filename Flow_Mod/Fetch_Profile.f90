!==============================================================================!
  subroutine Flow_Mod_Fetch_Profile(flw, prf)
!------------------------------------------------------------------------------!
!   Copy data read by profile "prf" into the flow "flw"                        !
!------------------------------------------------------------------------------!
!----------------------------------[Modules]-----------------------------------!
  use Mesh_Mod, only: Mesh_Type, STRUCTURED, UNSTRUCTURED
  use Prof_Mod, only: Prof_Type
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  type(Flow_Type)  :: flw
  type(Prof_Type)  :: prf
!-----------------------------------[Locals]-----------------------------------!
  integer                  :: j, k
  type(Mesh_Type), pointer :: msh
!==============================================================================!

  ! Take mesh pointer
  msh => flw % pnt_mesh

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
      flw % u_avg  % dns(At(j,k)) = prf % u(j)       !  2  u
      flw % v_avg  % dns(At(j,k)) = prf % v(j)       !  3  v  ~ zero
      flw % w_avg  % dns(At(j,k)) = prf % w(j)       !  4  w  ~ zero
      flw % t_avg  % dns(At(j,k)) = prf % t(j)       !  5  t

      flw % uu_avg % dns(At(j,k)) = prf % rs(1,j)    !  6  uu
      flw % vv_avg % dns(At(j,k)) = prf % rs(2,j)    !  7  vv
      flw % ww_avg % dns(At(j,k)) = prf % rs(3,j)    !  8  ww
      flw % uv_avg % dns(At(j,k)) = prf % rs(4,j)    !  9  uv
      flw % uw_avg % dns(At(j,k)) = prf % rs(5,j)    ! 10  uw ~ zero
      flw % vw_avg % dns(At(j,k)) = prf % rs(6,j)    ! 11  vw ~ zero

      flw % tt_avg % dns(At(j,k)) = prf % ts(1,j)    ! 12  tt
      flw % ut_avg % dns(At(j,k)) = prf % ts(2,j)    ! 13  ut
      flw % vt_avg % dns(At(j,k)) = prf % ts(3,j)    ! 14  vt
      flw % wt_avg % dns(At(j,k)) = prf % ts(4,j)    ! 15  wt ~ zero
    end do
  end do

  contains

  include '../At.f90'

  end subroutine
