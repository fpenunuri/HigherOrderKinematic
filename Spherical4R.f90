  !to compile: gfortran -o Spherical4R.exe Spherical4R.f90
  !to execute: Spherical4R.exe
  
  include '../Modulos/dualz1_mod.f90'
  include '../Modulos/dualz2_mod.f90'
  include '../Modulos/dualz3_mod.f90'
  include '../Modulos/dualz4_mod.f90'
  !
  include '../Modulos/mech_mod.f90'
  include '../Modulos/kinematic4_mod.f90' 
  include '../Modulos/tools_mod.f90' 

  module Sph4R_mod
    use dualz4_mod
    use mech_mod

    implicit none
    private
    public :: rcp_sph

  contains
    function rcp_sph(th) result(fr)
      type(dualz4), intent(in) :: th
      type(dualz4), allocatable, dimension(:) :: fr

      !real variables
      real(8), parameter :: pi = 2d0*asin(1d0), rho = 0.1*pi, phi1 = 0,&
           eta1 = 1.57d0, psi = 0.27d0, alpha1 = 1, alpha2 = 0.4d0,    &
           alpha3 = 0.82d0, alpha4 = 0.93d0, beta = 0.23d0,            &
           gamma = 0.47d0

      real(8), parameter, dimension(3) :: e1 = [1,0,0], e2 = [0,1,0],  &
           e3 = [0,0,1]

      real(8), dimension(3) :: x1, x1p, x1T, T1, T14, n14, x4, r1

      !dual variables
      type(dualz4) :: A, B, C, th4
      type(dualz4), dimension(3) :: r2, r3, n23, rb, rbg
      real(8) :: eps, eta1x

      eps = 10*epsilon(eps)
      if(eta1 == 0d0) then
         eta1x = eps
      elseif(eta1 == pi) then
         eta1x = pi - eps
      else
         eta1x = eta1
      end if      
                                                 
      x1 = cos(phi1)*sin(eta1x)*e1 + sin(phi1)*sin(eta1x)*e2 +         &
           cos(eta1x)*e3
            
      x1p = x1 - cos(eta1x)*e3

      x1T = matmul(rot_mat(rho,e3),x1p)
      T1  = Gtangent(x1p,x1T)
      T14 = matmul(rot_mat(psi,x1),T1)
      n14 = normalize(cross(x1,T14))
      x4  = matmul(rot_mat(alpha1,n14),x1)
      r1  = matmul(rot_mat(alpha2,n14),x1)

      A = sin(alpha2)*sin(alpha4)*sin(th)
      B = cos(alpha2)*sin(alpha4)*sin(alpha1) - sin(alpha2)*sin(alpha4)*&
           cos(alpha1)*cos(th)

      C = cos(alpha2)*cos(alpha4)*cos(alpha1) + sin(alpha2)*cos(alpha4)*&
           sin(alpha1)*cos(th) - cos(alpha3)
      
      th4 = 2*atan((-A - sqrt(A**2 + B**2 - C**2))/(C - B))

      r2 = matmul(rot_mat(th, x1), r1)
      r3 = matmul(rot_mat(th4,-x4),matmul(rot_mat(alpha4,-n14),x4))
      
      n23 = normalize(cross(r2,r3))
      rb  = matmul(rot_mat(beta,n23),r2)
      rbg = matmul(rot_mat(beta + gamma, n23),r2)

      allocate(fr(3))        
      fr = matmul(rot_mat(pi/2,rb),rbg)
    end function rcp_sph
  end module Sph4R_mod
  
  program main
    use dualz4_mod
    use kinematic4_mod
    use mech_mod
    use Sph4R_mod
    use tools_mod
    implicit none

    type(dualz4) :: th
    type(dualz4), dimension(3) :: rcp

    !we consider real variables
    integer, parameter :: Rm = 1
    integer, parameter :: Rn = 3
    real(8), dimension(Rm) :: x0p, x1p, x2p, x3p, x4p
    real(8), dimension(Rn) :: Rpos, Rvel, Racel, Rjerk, Rjounce_snap

    !since there is one degree of freedom, we can proceed as follows
    th = dualz4(0.5d0,1d0,0d0,0d0,0d0)
    rcp = rcp_sph(th)

    print*, 'Kinematic variables: position, ..., jounce/snap'
    call disp_vec(real(rcp%f0),5)
    call disp_vec(real(rcp%f1),5)
    call disp_vec(real(rcp%f2),5)
    call disp_vec(real(rcp%f3),5)
    call disp_vec(real(rcp%f4),5)

    print*,new_line('a')
    !if we want to use the kinematic4 subroutine, we need to define the
    !function accepting a vector of arbitrary dimension. We decided to
    !use the below auxiliar function instead of coding the  rcp_sph
    !function as a function of vector argument
    x0p = [0.5] !vector of dimension 1
    x1p = [1]
    x2p = [0]
    x3p = [0]
    x4p = [0]    

    call kinematic4(rcp_sph_vec,x0p,x1p,x2p,x3p,x4p,Rpos, Rvel, Racel,&
         Rjerk,Rjounce_snap)

    !the warning message is not relevant as the imaginary parts are of
    !order 1e-14
    print*,'Kinematic variables: position, ..., jounce/snap'
    call disp_vec(Rpos,5)
    call disp_vec(Rvel,5)
    call disp_vec(Racel,5)
    call disp_vec(Rjerk,5)
    call disp_vec(Rjounce_snap,5)

  contains
    !the auxiliar function  
    function rcp_sph_vec(r) result(fr)
      type(dualz4), intent(in), dimension(:) :: r
      type(dualz4), allocatable, dimension(:) :: fr
      type(dualz4) :: th

      th = r(1)
      allocate (fr(3))
      fr = rcp_sph(th)
    end function rcp_sph_vec       
end program main
