  !to compile: gfortran -o example2.exe example2.f90
  !to execute: example2.exe
  
  !modules for the dual number implementation
  include './Modules/dualz1_mod.f90'
  include './Modules/dualz2_mod.f90'
  include './Modules/dualz3_mod.f90'
  include './Modules/dualz4_mod.f90'

  !module implementing Eqs. 19, 20, 27 and 29 of the paper
  include './Modules/kinematic4_mod.f90'

  !a useful module, it include the disp_vec subroutine
  include './Modules/tools_mod.f90' 


  !module with a test functions which deppends explicitly on time
  module ftest_mod
    use dualz4_mod
    implicit none
    private

    public :: gtest_explicit, gtest
  contains

    !an example with explicit dependence o t
    !f deppends explicitly on t
    function  gtest_explicit(r) result(fr)
      type(dualz4), intent(in), dimension(:) :: r   !dimension m
      type(dualz4), allocatable, dimension(:) :: fr !dimension n
      type(dualz4) :: th, phi, s, t
      real(8), parameter :: pi = 4*atan(1d0), BC = 3d0

      !Dm = 4 (dim r)
      th  = r(1)
      phi = r(2)
      s   = r(3)
      t   = r(4) 

      !Dn = 3 (dimf)
      allocate (fr(3))
      fr = [cos(th)*s + BC*sin(phi)*sin(th), -BC*cos(th)*sin(phi) +   &
           s*sin(th), BC*cos(phi)]*exp(-t**2)
    end function gtest_explicit

    function gtest(t) result(fr)
      type(dualz4), intent(in) :: t
      type(dualz4), allocatable, dimension(:) :: fr

      allocate(fr(4))

      !    [th(t),  phi(t),      s(t),  t]
      !       |       |            |    | 
      fr = [5*t**2, sin(2*t**3), cos(t),t]
    end function gtest
  end module ftest_mod

    
  !main program
  program main
    use dualz4_mod
    use kinematic4_mod
    use ftest_mod
    use tools_mod
    implicit none
    
    real(8), parameter :: pi = 2*asin(1d0)
    integer, parameter :: Rm = 3 !degrees of freedom
    integer, parameter :: Rn = 3 !dimension of the space

    !we consider real variables
    real(8), dimension(Rm) :: x0p, x1p, x2p, x3p, x4p
    real(8), dimension(Rn) :: Rpos, Rvel, Racel, Rjerk, Rjounce_snap
    real(8) :: t0
    
    !f depends explicitly on t, f=f(x1(t),x2(t),...,xm(t)=t)
    t0 = 1.1d0
    call kinematic4(gtest_explicit,gtest,t0,Rpos,Rvel,Racel,Rjerk,     &
         Rjounce_snap)

    print*,'f depends explicitly on t'
    print*, 'Kinematic variables: position, ..., jounce/snap'
    
    call disp_vec(Rpos,5)
    call disp_vec(Rvel,5)
    call disp_vec(Racel,5)
    call disp_vec(Rjerk,5)
    call disp_vec(Rjounce_snap,5)
  end program main
