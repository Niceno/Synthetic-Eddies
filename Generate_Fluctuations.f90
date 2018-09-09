!==============================================================================!
  subroutine Generate_Fluctuations(ts)
!------------------------------------------------------------------------------!
!   Generate fluctuations without combining mean and rms data                  !
!------------------------------------------------------------------------------!
  use Sem_Mod
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  integer :: ts
!-----------------------------------[Locals]-----------------------------------!
  integer           :: e, j, k
  real              :: x0, y0, z0, f
  character(len=80) :: file_name
!==============================================================================!

  u % raw(:,:) = 0.0
  v % raw(:,:) = 0.0
  w % raw(:,:) = 0.0
  t % raw(:,:) = 0.0

  do k = 1, mesh % nz
    do j = 1, mesh % ny

      do e = 1, eddy % n_eddies
        x0 = (0           - eddy % x(e)) / eddy % len(e)
        y0 = (mesh % y(j) - eddy % y(e)) / eddy % len(e)
        z0 = (mesh % z(k) - eddy % z(e)) / eddy % len(e)

        !--------------------!
        !   Shape function   !
        !--------------------!
        if ( abs(x0) <=1 .and. abs(y0) <=1 .and. abs(z0) <=1) then
          f = sqrt(1.5) * (1- abs(x0)) *                        &
              sqrt(1.5) * (1- abs(y0)) *                        &
              sqrt(1.5) * (1- abs(z0))

          u % raw(j,k) = u % raw(j,k) +                         &
                         sqrt(v_b/eddy % len(e)**3) *           &
                         eddy % x_int(e)*f

          v % raw(j,k) = v % raw(j,k) +                         &
                        sqrt(v_b/eddy % len(e)**3) *            &
                        eddy % y_int(e)*f

          w % raw(j,k) = w % raw(j,k) +                         &
                        sqrt(v_b/eddy % len(e)**3) *            &
                        eddy % z_int(e)*f

          t % raw(j,k) = t % raw(j,k) +                         &
                        sqrt(v_b/eddy % len(e)**3) *            &
                        eddy % t_int(e)*f
        end if
      end do

    end do
  end do

  u % raw(:,:) = u % raw(:,:) / sqrt(real(eddy % n_eddies, 8))
  v % raw(:,:) = v % raw(:,:) / sqrt(real(eddy % n_eddies, 8))
  w % raw(:,:) = w % raw(:,:) / sqrt(real(eddy % n_eddies, 8))
  t % raw(:,:) = t % raw(:,:) / sqrt(real(eddy % n_eddies, 8))

  if( mod(ts,10) .eq. 0) then
    call Save_Vtk_4_Arrays('raw-velocities', u % raw, v % raw, w % raw, t % raw, ts)
  end if

  end subroutine
