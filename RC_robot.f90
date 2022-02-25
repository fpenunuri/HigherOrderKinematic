  !to compile: gfortran -o RC_robot.exe RC_robot.f90
  !to execute: RC_robot.exe
  
  !modules for the dual number implementation
  include './Modules/dualz1_mod.f90'
  include './Modules/dualz2_mod.f90'
  include './Modules/dualz3_mod.f90'
  include './Modules/dualz4_mod.f90'

  !module implementing Eqs. 19, 20, 27 and 29 of the paper
  include './Modules/kinematic4_mod.f90'

  !a useful module, it include the disp_vec subroutine
  include './Modules/tools_mod.f90' 


  !module for the RC robot manipulator example
  module RC_mod
    use dualz4_mod
    implicit none
    private

    public ::  RCpos, gtest0
  contains

    !position vector of the end effector for the RC robot
     function RCpos(r) result(f_r)
      type(dualz4), intent(in), dimension(:) :: r    !dimension m
      type(dualz4), allocatable, dimension(:) :: f_r !dimension n
      type(dualz4) :: th, phi, s
      real(8), parameter :: BC = 3d0
      integer, parameter :: Rm = 3, Dn = 3
      type(dualz4), dimension(4,4) :: T01, T12, T23, T03
      type(dualz4) :: zd, oned, BCd
      real(8) :: t0

      zd = 0; oned = 1; BCd = BC
      th = r(1); phi = r(2); s = r(Rm)

      allocate(f_r(Dn))
      T01(1,:) = [cos(th), -sin(th), zd, zd]
      T01(2,:) = [sin(th), cos(th), zd, zd]
      T01(3,:) = [zd, zd, oned, zd]
      T01(4,:) = [zd, zd, zd, oned]
      T12(1,:) = [oned, zd, zd, s]
      T12(2,:) = [zd, cos(phi), -sin(phi), zd]

      T12(3,:) = [zd, sin(phi), cos(phi), zd]
      T12(4,:) = [zd, zd, zd, oned]
      T23(1,:) = [oned, zd, zd, zd]
      T23(2,:) = [zd, oned, zd, zd]
      T23(3,:) = [zd, zd, oned, BCd]
      T23(4,:) = [zd, zd, zd, oned]
      T03 = matmul(matmul(T01, T12), T23)
      f_r = T03(1:3,4)
    end function RCpos

    !the articular variables (or degrees of freedom)
    !th = fr(1); phi = fr(2); s = fr(Rm) are given as function of t
    !the functions th(t), phi(t) and s(t) coded as component of gtest0
    function gtest0(t) result(fr)
      type(dualz4), intent(in) :: t
      type(dualz4), allocatable, dimension(:) :: fr

      allocate(fr(3))

      fr = [sin(t), cos(t), 1 + sin(exp(-t**2))]
    end function gtest0
  end module RC_mod

    
  !main program
  program main
    use dualz4_mod
    use kinematic4_mod
    use RC_mod
    use tools_mod
    implicit none
    
    real(8), parameter :: pi = 2*asin(1d0)
    integer, parameter :: Rm = 3 !degrees of freedom
    integer, parameter :: Rn = 3 !dimension of the space

    !we consider real variables
    real(8), dimension(Rm) :: x0p, x1p, x2p, x3p, x4p
    real(8), dimension(Rn) :: Rpos, Rvel, Racel, Rjerk, Rjounce_snap
    real(8) :: t0
    
    x0p = [pi/2, 0d0, 2d0]
    x1p = [1d0, 5.d0, 1.d0]
    x2p = [1d0, 0d0, 2d0]
    x3p = [10d0, 20d0, 30d0]
    x4p = [40d0, 50d0, 60d0]

    !Eqs. 19, 20, 27 and 29 are coded in the kinematic4 subroutine
     call kinematic4(RCpos,x0p,x1p,x2p,x3p,x4p,Rpos,Rvel,Racel,Rjerk,  &
         Rjounce_snap)

    print*,'Kinematic variables: position, ..., jounce/snap'
    call disp_vec(Rpos,2)
    call disp_vec(Rvel,2)
    call disp_vec(Racel,2)
    call disp_vec(Rjerk,2)
    call disp_vec(Rjounce_snap,2)

    !observe that the kinematic4 subroutine accepts diferent arguments
    !for instance, gtest0 below, is a vector function with componets
    !th(t), phi(t) and s(t); t0 is the evaluation time
    t0 = 1.1d0
    call kinematic4(RCpos,gtest0,t0,Rpos,Rvel,Racel,Rjerk,Rjounce_snap)
    
    print*, new_line('a')
    print*, 'Kinematic variables: position, ..., jounce/snap'
    
    call disp_vec(Rpos,3)
    call disp_vec(Rvel,3)
    call disp_vec(Racel,3)
    call disp_vec(Rjerk,3)
    call disp_vec(Rjounce_snap,3)
  end program main
