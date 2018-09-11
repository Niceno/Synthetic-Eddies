!==============================================================================!
  subroutine Mesh_Mod_Create_Cartesian(msh, ny, nz)
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  type(Mesh_Type) :: msh
  integer         :: ny, nz
!-----------------------------------[Locals]-----------------------------------!
  integer :: j, k, n, c
  real, allocatable :: yn(:), zn(:), yc(:), zc(:), dy(:), dz(:)
!==============================================================================!

  write(*,*) '#==================================='
  write(*,*) '# Mesh creation process started ... '

  msh % mode     =  STRUCTURED

  ! Allocate local memory
  allocate(yn(ny  ));  yn(:) = 0.0
  allocate(zn(nz  ));  zn(:) = 0.0
  allocate(yc(ny-1));  yc(:) = 0.0
  allocate(zc(nz-1));  zc(:) = 0.0
  allocate(dy(ny-1));  dy(:) = 0.0
  allocate(dz(nz-1));  dz(:) = 0.0

  ! Store number of nodes and cells
  msh % ny       =  ny
  msh % nz       =  nz
  msh % n_nodes  =  msh % ny * msh % nz
  msh % n_cells  = (msh % ny - 1)  &
                 * (msh % nz - 1)

  ! Allocate memory
  allocate(msh % yn  (msh % n_nodes))
  allocate(msh % zn  (msh % n_nodes))
  allocate(msh % yc  (msh % n_cells))
  allocate(msh % zc  (msh % n_cells))
  allocate(msh % area(msh % n_cells))

  ! Initialize "y" coordinates, they range from zero to two
  do j = 1, ny
    yn(j) = (j-1) * 2.00 / real(ny-1)
  end do

  do j = 1, ny - 1
    yc(j) = ( yn(j) + yn(j+1) ) * 0.5
    dy(j) = ( yn(j+1) - yn(j) )
  end do

  ! Compute "z" cooridnates for nodes and ...
  ! ... cells, assuming domain width is 2 PI
  do k = 1, nz
    zn(k) = (k-1) * 6.28 / real(nz-1)
  end do

  do k = 1, nz - 1
    zc(k) = ( zn(k) + zn(k+1) ) * 0.5
    dz(k) = ( zn(k+1) - zn(k) )
  end do

  ! Copy linear node coordinates to full mesh coordinates
  do k = 1, msh % nz
    do j = 1, msh % ny
      n = (k - 1) * msh % ny + j        ! find linear address for the node
      msh % yn(n) = yn(j)
      msh % zn(n) = zn(k)
    end do
  end do

  ! Copy linear cell coordinates to full mesh coordinates
  do k = 1, msh % nz - 1
    do j = 1, msh % ny - 1
      c = (k - 1) * (msh % ny - 1) + j  ! find linear address for the cell
      msh % yc(c) = yc(j)
      msh % zc(c) = zc(k)
      msh % area(c) = dy(j) * dz(k)
    end do
  end do

  write(*,*) '# ... mesh creation process ended.'
  write(*,*) '#-----------------------------------'
  write(*,*) ''

  end subroutine

