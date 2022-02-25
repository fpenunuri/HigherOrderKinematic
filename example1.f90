  !this is the code of the file example1.f90
  !to compile using the gfortran compiler, open a terminal and type:
  !gfortran -o example1.exe example1.f90
  !type example1.exe to execute
 
  include "./Modules/dualz1_mod.f90"
  include "./Modules/dualz2_mod.f90"
  include "./Modules/dualz3_mod.f90"
  include "./Modules/dualz4_mod.f90"
  
  program main
    use dualz4_mod
    implicit none
    
    type(dualz4) :: f, x
    integer :: k

    !observe x%f1 = 1, x is the independent variable
    x = dualz4(1.1d0,1d0,0d0,0d0,0d0)

    f = sin(x)*cos(x) + x**3

    write(*,*) "the derivatives 0th, 1th, 2th, 3th and 4th of f "// &
         "in 1.1 are:"

    !since we use complex components, we use real to get the real part
    write(*,"(f0.5)") real(f%f0) 
    write(*,"(f0.5)") real(f%f1)
    write(*,"(f0.5)") real(f%f2)
    write(*,"(f0.5)") real(f%f3)
    write(*,"(f0.5)") real(f%f4)

    write(*,*) "-----"

    !this computes the 4th derivative of sin(sin(...sin(x))), nested 20
    !times in x0 = 1.1 + i
    x = dualz4((1.1d0,1d0),1,0,0,0) !(1.1d0,1d0) is the complex 1.1 + i

    f = sin(x)
    do k=1,20
       f = sin(f)
    end do

    write(*,*) f%f4    
end program main

