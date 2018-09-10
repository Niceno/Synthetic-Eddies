!==============================================================================!
  subroutine Mesh_Mod_Create_Cartesian(msh, ny, nz)
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  type(Mesh_Type) :: msh
  integer         :: ny, nz
!-----------------------------------[Locals]-----------------------------------!
  integer :: i,j,k
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

  ! Initialize "y" coordinates, you will compute them later from profile
  ! Very bad practice, should be improved soon
  msh % yn(:) = 0.0

  ! Compute "z" cooridnates for nodes and ...
  ! ... cells, assuming domain width is 2 PI
  do k = 1, msh % nz
    msh % zn(k) = (k-1) * 6.28 / (msh % nz-1)
  end do

  do k = 1, msh % nz - 1
    msh % zc(k) = ( msh % zn(k) + msh % zn(k+1) ) * 0.5
  end do

  end subroutine

