!==============================================================================!
  subroutine Mesh_Mod_Calculate_Geometry(msh)
!------------------------------------------------------------------------------!
!   Calculates geometrical quantities of the mesh.                             !
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  type(Mesh_Type) :: msh
!-----------------------------------[Locals]-----------------------------------!
  integer         :: c, node(4)
!==============================================================================!

  print *, '# Calculating geometrical quantities of the mesh'

  !------------------!
  !   Cell centres   !
  !------------------!
  do c = 1, msh % n_cells
    msh % yc(c) = sum(msh % yn(msh % cells_nodes(1:4, c))) * 0.25
    msh % zc(c) = sum(msh % zn(msh % cells_nodes(1:4, c))) * 0.25
  end do

  !----------------!
  !   Cell areas   !
  !----------------!
  do c = 1, msh % n_cells

    ! Make an alias for shorter syntax
    node(1:4) = msh % cells_nodes(1:4, c)

    ! Apply shoelace formula
    msh % area(c) = (  msh % yn(node(1)) * msh % zn(node(2))    &
                     + msh % yn(node(2)) * msh % zn(node(3))    &
                     + msh % yn(node(3)) * msh % zn(node(4))    &
                     + msh % yn(node(4)) * msh % zn(node(1))    &
                     - msh % yn(node(2)) * msh % zn(node(1))    &
                     - msh % yn(node(3)) * msh % zn(node(2))    &
                     - msh % yn(node(4)) * msh % zn(node(3))    &
                     - msh % yn(node(1)) * msh % zn(node(4)) ) * 0.5

    ! Take absolute value, don't have ...
    !  ... time to think of orientation
    msh % area(c) = abs(msh % area(c))

  end do

  print *, '# Total mesh surface area is: ', sum(msh % area(:))

  print *, '# Done!'

  end subroutine

