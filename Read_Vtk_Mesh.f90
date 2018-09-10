!==============================================================================!
  subroutine Read_Vtk_Mesh(msh, full_name)
!------------------------------------------------------------------------------!
!   Reads vtk files created and allocates memory for the mesh along the way    !
!                                                                              !
!   It was designed to read meshes obtained from "Neu_2_Vtu" utility           !
!   (in folder Meshes) and closely follows its formats (a7,i6,a6) etc.         !
!------------------------------------------------------------------------------!
!----------------------------------[Modules]-----------------------------------!
  use Mesh_Mod, only: Mesh_Type
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  type(Mesh_Type)  :: msh
  character(len=*) :: full_name
!-----------------------------------[Locals]-----------------------------------!
  integer           :: k, c, n
  real              :: tmp
  character(len=80) :: line
!------------------------------[Local parameters]------------------------------!
  integer, parameter :: VTK = 9
!==============================================================================!

  open(VTK, file=trim(full_name), form='formatted')

  !-----------------!
  !   Skip header   !
  !-----------------!
  read(VTK,*) line;  read(VTK,*) line;  read(VTK,*) line;  read(VTK,*) line; 

  !-------------------!
  !   Nodes section   !
  !-------------------!
  read(VTK, '(a7,i6,a6)')  line(1:7), msh % n_nodes, line(41:47)

  allocate(msh % yn(msh % n_nodes))
  allocate(msh % zn(msh % n_nodes))

  do n = 1, msh % n_nodes
    read(VTK, *)  tmp, msh % yn(n), msh % zn(n)
  end do

  !-------------------!
  !   Cells section   !
  !-------------------!
  read(VTK, '(a6,i6,a6)')  line(1:6), msh % n_cells, line(41:47)

  allocate(msh % yc(msh % n_cells))
  allocate(msh % zc(msh % n_cells))
  allocate(msh % cells_nodes(4, msh % n_cells))
  allocate(msh % area(msh % n_cells))

  do c = 1, msh % n_cells
    read(VTK, *)  k, msh % cells_nodes(1:4, c)
  end do
  msh % cells_nodes(:,:) = msh % cells_nodes(:,:) + 1

  close(VTK)

  end subroutine

