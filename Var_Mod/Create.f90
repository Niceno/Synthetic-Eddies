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
  allocate( phi % com(mesh % n_cells) )
  allocate( phi % raw(mesh % n_cells) )
  allocate( phi % dns(mesh % n_cells) )

  phi % com(:) = 0.0
  phi % raw(:) = 0.0
  phi % dns(:) = 0.0

  end subroutine
