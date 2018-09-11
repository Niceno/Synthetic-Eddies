!==============================================================================!
  subroutine Mesh_Mod_Create_Cartesian(msh, ny, nz)
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  type(Mesh_Type) :: msh
  integer         :: ny, nz
!-----------------------------------[Locals]-----------------------------------!
  integer :: j, k
!==============================================================================!

  msh % mode     =  STRUCTURED

  ! Store number of nodes and cells
  msh % ny       =  ny
  msh % nz       =  nz
  msh % n_nodes  =  msh % ny * msh % nz
  msh % n_cells  = (msh % ny - 1)  &
                 * (msh % nz - 1)

  ! Allocate memory
  allocate(msh % yn(msh % ny))
  allocate(msh % zn(msh % nz))
  allocate(msh % yc(msh % ny - 1))
  allocate(msh % zc(msh % nz - 1))

  ! Initialize "y" coordinates, they range from zero to two
  do j = 1, msh % ny
    msh % yn(j) = (j-1) * 2.00 / (msh % ny-1)
  end do

  do j = 1, msh % ny - 1
    msh % yc(j) = ( msh % yn(j) + msh % yn(j+1) ) * 0.5
  end do

  ! Compute "z" cooridnates for nodes and ...
  ! ... cells, assuming domain width is 2 PI
  do k = 1, msh % nz
    msh % zn(k) = (k-1) * 6.28 / (msh % nz-1)
  end do

  do k = 1, msh % nz - 1
    msh % zc(k) = ( msh % zn(k) + msh % zn(k+1) ) * 0.5
  end do

  end subroutine

