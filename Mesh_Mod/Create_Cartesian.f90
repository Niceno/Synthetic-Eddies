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

  msh % ny       =  ny
  msh % nz       =  nz
  msh % n_nodes  =  msh % ny * msh % nz
  msh % n_cells  = (msh % ny - 1)  &
                 * (msh % nz - 1)
  allocate(msh % yn(msh % ny))
  allocate(msh % zn(msh % nz))
  allocate(msh % yc(msh % ny - 1))
  allocate(msh % zc(msh % nz - 1))
  msh % yn(:) = 0.0
  do k = 1, msh % nz
    msh % zn(k) =   0.5 * 6.0 / msh % nz  &
                + (k-1) * 6.0 / msh % nz
  end do

  end subroutine

