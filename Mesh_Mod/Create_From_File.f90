!==============================================================================!
  subroutine Mesh_Mod_Create_From_File(msh, full_name)
!------------------------------------------------------------------------------!
!   Reads vtk files created by "Neu_2_Vtu" utility and follows its formats.    !
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

  msh % mode =  UNSTRUCTURED

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

  do c = 1, msh % n_cells
    read(VTK, *)  k, msh % cells_nodes(1:4, c)
  end do

  close(VTK)

  !----------------------------------!
  !   Test if you read it properly   !
  !----------------------------------!
  call Save_Vtk_Mesh(msh, "test-mesh.vtk")

  end subroutine

