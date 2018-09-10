!==============================================================================!
  subroutine Generate_Fluctuations(flw, ed, ts)
!------------------------------------------------------------------------------!
!   Generate fluctuations without combining mean and rms data                  !
!------------------------------------------------------------------------------!
!----------------------------------[Modules]-----------------------------------!
  use Eddy_Mod, only: Eddy_Type
  use Flow_Mod, only: Flow_Type
  use Mesh_Mod, only: Mesh_Type, STRUCTURED, UNSTRUCTURED
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  type(Flow_Type) :: flw
  type(Eddy_Type) :: ed
  integer         :: ts
!-----------------------------------[Locals]-----------------------------------!
  integer                 :: e, j, k
  real                    :: x0, y0, z0, f, v_b
  real                    :: y_s, y_e, z_s, z_e, s
  character(len=80)       :: file_name
  type(Mesh_Type), target :: msh
!==============================================================================!

  ! Take mesh pointer
  msh = flw % pnt_mesh

  flw % u % raw(:) = 0.0
  flw % v % raw(:) = 0.0
  flw % w % raw(:) = 0.0
  flw % t % raw(:) = 0.0

  ! Surface area ...
  if(msh % mode .eq. STRUCTURED) then
    y_s = minval(msh % yn(:))
    y_e = maxval(msh % yn(:))
    z_s = minval(msh % zn(:))
    z_e = maxval(msh % zn(:))
    s = (y_e - y_s) * (z_e - z_s)
  end if

  ! ... and some kind of volume ... boundary volume?
  v_b = s * 2 * ed % sigma

  if(msh % mode == STRUCTURED) then
    do k = 1, msh % nz - 1                              ! through cells
      do j = 1, msh % ny - 1                            ! through cells

        do e = 1, ed % n_eddies
          x0 = (0           - ed % x(e)) / ed % len(e)
          y0 = (msh % yc(j) - ed % y(e)) / ed % len(e)  ! cell center
          z0 = (msh % zc(k) - ed % z(e)) / ed % len(e)  ! cell center

          !--------------------!
          !   Shape function   !
          !--------------------!
          if ( abs(x0) <=1 .and. abs(y0) <=1 .and. abs(z0) <=1) then
            f = sqrt(1.5) * (1- abs(x0)) *                        &
                sqrt(1.5) * (1- abs(y0)) *                        &
                sqrt(1.5) * (1- abs(z0))

            flw % u % raw(At(j,k)) = flw % u % raw(At(j,k)) +     &
                           sqrt(v_b/ed % len(e)**3) *             &
                           ed % x_int(e)*f

            flw % v % raw(At(j,k)) = flw % v % raw(At(j,k)) +     &
                          sqrt(v_b/ed % len(e)**3) *              &
                          ed % y_int(e)*f

            flw % w % raw(At(j,k)) = flw % w % raw(At(j,k)) +     &
                          sqrt(v_b/ed % len(e)**3) *              &
                          ed % z_int(e)*f

            flw % t % raw(At(j,k)) = flw % t % raw(At(j,k)) +     &
                          sqrt(v_b/ed % len(e)**3) *              &
                          ed % t_int(e)*f
          end if
        end do

      end do
    end do
  end if

  flw % u % raw(:) = flw % u % raw(:) / sqrt(real(ed % n_eddies, 8))
  flw % v % raw(:) = flw % v % raw(:) / sqrt(real(ed % n_eddies, 8))
  flw % w % raw(:) = flw % w % raw(:) / sqrt(real(ed % n_eddies, 8))
  flw % t % raw(:) = flw % t % raw(:) / sqrt(real(ed % n_eddies, 8))

  contains

  include 'At.f90'

  end subroutine
