module tools_mod
  implicit none
  private

  public :: disp_vec, IntToString, linspace

  !displays a vector, real or complex
  interface disp_vec
     module procedure disp_Rvec
     module procedure disp_Cvec
  end interface disp_vec

contains
  !Generate linearly spaced vector
  !a: initial point
  !b: final point
  !n: mumber of points
  function linspace(a,b,n) result(fr)
    real(8), intent(in) :: a, b
    integer, intent(in) :: n
    real(8), dimension(n) :: fr
    real(8) :: dx
    integer :: k

    if(n==1) then
       fr(1) = a
       return
    end if

    dx = (b-a)/(n-1)    

    do k=1, n
       fr(k) = a + (k-1)*dx
    end do
  end function linspace
  
  !displays a real vector
  !x: the vector to display
  !ndec: the number of decimal places to print
  subroutine disp_Rvec(x,ndec)
    real(8), intent(in), dimension(:) :: x
    integer, intent(in) :: ndec
    character(50) ::  str0, str1, fmtString      
    integer :: dimm1

    dimm1 = size(x)-1      
    str0 = IntToString(dimm1)
    str1 = IntToString(ndec)

    fmtString = "(a,"//trim(str0)//"(f0."//trim(str1)//&
         ",',',1x),f0."//trim(str1)//",a)"

    write(*,fmt = fmtString) '[',x,']'
  end subroutine disp_Rvec

  !displays a complex vector
  subroutine disp_Cvec(x,ndec)
    complex(8), intent(in), dimension(:) :: x
    integer, intent(in) :: ndec
    character(100) ::  str0, str1, fmtString      
    integer :: dimm1

    dimm1 = size(x)-1      
    str0 = IntToString(dimm1)
    str1 = IntToString(ndec)     

    fmtString = "(A,"//trim(str0)//"( '('f0."//trim(str1)//" ',' f0."&
         //trim(str1)//"')' ,',',1x),'('f0."//trim(str1)//&
         " ',' f0."//trim(str1)//"')',A)"

    write(*,fmt = fmtString) '[',x,']'
  end subroutine disp_Cvec

  !auxiliar function for changing an integer n to string
  function IntToString(n) result(f_result)
    character(len = :), allocatable :: f_result
    integer, intent(in) :: n
    character(len = 100) :: auxf_r

    write(auxf_r,*) n
    f_result = trim(adjustl(auxf_r))
  end function IntToString
end module tools_mod
