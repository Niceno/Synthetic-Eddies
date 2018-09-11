!==============================================================================!
  subroutine Mesh_Mod_Create_From_File(msh, full_name)
!------------------------------------------------------------------------------!
!   Reads vtk files created by "Neu_2_Vtu" utility and follows its formats.    !
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  type(Mesh_Type)  :: msh
  character(len=*) :: full_name
!==============================================================================!

  write(*,*) '#==================================='
  write(*,*) '# Mesh creation process started ... '

  !-----------------------------------------------------!
  !   Set mode; if read from file, it is unstructured   !
  !-----------------------------------------------------!
  msh % mode =  UNSTRUCTURED

  !-------------------------------------------------!
  !   Do the actual reading and memory allocation   !
  !-------------------------------------------------!
  call Read_Vtk_Mesh(msh, full_name)

  !---------------------------------------------!
  !   Calculate grid's geometrical quantities   !
  !---------------------------------------------!
  call Mesh_Mod_Calculate_Geometry(msh)

  write(*,*) '# ... mesh creation process ended.'
  write(*,*) '#-----------------------------------'
  write(*,*) ''

  end subroutine

