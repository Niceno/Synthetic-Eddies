!==============================================================================!
  subroutine Var_Mod_Create(phi, mesh)
!------------------------------------------------------------------------------!
!   Create memory for a variable defined on the specific mesh                !
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  type(Var_Type)          :: phi
  type(Mesh_Type), target :: mesh
!==============================================================================!

  ! Store mesh for which the variable is defined
  phi % pnt_mesh => mesh

  allocate (phi % com(mesh % ny, mesh % nz))
  allocate (phi % raw(mesh % ny, mesh % nz))

  phi % com(:,:) = 0.0
  phi % raw(:,:) = 0.0

  end subroutine
