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

  ! Allocate unknowns in cell centres
  allocate (phi % com(mesh % ny-1, mesh % nz-1))
  allocate (phi % raw(mesh % ny-1, mesh % nz-1))

  phi % com(:,:) = 0.0
  phi % raw(:,:) = 0.0

  end subroutine
