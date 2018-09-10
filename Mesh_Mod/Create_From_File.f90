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

  !-----------------------------------------------------!
  !   Set mode; if read from file, it is unstructured   !
  !-----------------------------------------------------!
  msh % mode =  UNSTRUCTURED

  !-------------------------------------------------!
  !   Do the actual reading and memory allocation   !
  !-------------------------------------------------!
  call Read_Vtk_Mesh(msh, full_name)

  ! Test if you read it properly
  call Save_Vtk_Mesh(msh, "test-mesh.vtk")

  !---------------------------------------------!
  !   Calculate grid's geometrical quantities   !
  !---------------------------------------------!
  call Mesh_Mod_Calculate_Geometry(msh)

  end subroutine

