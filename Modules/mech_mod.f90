!module with some useful functions in mechanics
!
!F. Peñuñuri
!Yucatan Mexico 2022
!UADY
module mech_mod
    use dualz4_mod
    implicit none

    private
    public :: rot_mat, cross, Gtangent, norm, normalize
    
    interface rot_mat
       module procedure rot_mat_dz4
       module procedure rot_mat_dz4C
       module procedure rot_mat_dz4R
       module procedure rot_mat_Cdz4
       module procedure rot_mat_Rdz4
       module procedure f0_rot_matC
       module procedure f0_rot_matCR
       module procedure f0_rot_matRC
       module procedure f0_rot_matR
    end interface rot_mat

    interface cross
       module procedure cross_dz4
       module procedure cross_dz4C
       module procedure cross_dz4R
       module procedure cross_Cdz4
       module procedure cross_Rdz4
       module procedure f0_crossC
       module procedure f0_crossCR
       module procedure f0_crossRC
       module procedure f0_crossR
    end interface cross

    interface Gtangent
       module procedure Gtangent_dz4
       module procedure Gtangent_dz4C
       module procedure Gtangent_dz4R
       module procedure Gtangent_Cdz4
       module procedure Gtangent_Rdz4
       module procedure f0_GtangentC
       module procedure f0_GtangentCR
       module procedure f0_GtangentRC
       module procedure f0_GtangentR
    end interface Gtangent

    interface norm
       module procedure norm_dz4
       module procedure f0_normC
       module procedure f0_normR
    end interface norm

    interface normalize
       module procedure normalize_dz4
       module procedure f0_normalizeC
       module procedure f0_normalizeR
    end interface normalize

  contains

    !Since we want to use dual numbers to compute derivatives, all
    !quantities are computed as if we were working with reals, even
    !when we are using complex components for the dual numbers.
    !Nevertheless, at the end we evaluate in a dual number. For
    !instance, the norm_dz4 function (not a public function) is coded as
    !sqrt(x1*x1+x2*x2+...xn*xn) and not as sqrt(<x|x>) which is the
    !norm of a complex vector.
    !eje not need to be a unit vector

    !it is not required to have a unit vector as the rotation axis
    function rot_mat_dz4(th, eje) result(fr)
      type(dualz4), intent(in) :: th
      type(dualz4), intent(in), dimension(3) :: eje
      type(dualz4), dimension(3,3) :: fr
      type(dualz4), dimension(3) :: ejeu      
      type(dualz4) :: n1, n2, n3

      !making 'eje' a unit vector
      ejeu = eje/sqrt(sum(eje*eje))

      n1 = ejeu(1); n2 = ejeu(2); n3 = ejeu(3)
      
      fr(1,:) = [1 + (n2**2 + n3**2)*(cos(th) - 1), n1*n2 - n1*n2*     &
           cos(th) - n3*sin(th), n1*n3 - n1*n3*cos(th) + n2*sin(th)]

      fr(2,:) = [n1*n2 - n1*n2*cos(th) + n3*sin(th), 1 + (n1**2 +      &
           n3**2)*(cos(th) - 1), n2*n3 - n2*n3*cos(th) - n1*sin(th)]

      fr(3,:) = [n1*n3 - n1*n3*cos(th) - n2*sin(th), n2*n3 - n2*n3*    &
           cos(th) + n1*sin(th), 1 + (n1**2 + n2**2)*(cos(th) - 1)]
    end function rot_mat_dz4
    
    function rot_mat_dz4C(th, eje) result(fr)
      type(dualz4), intent(in) :: th
      complex(8), intent(in), dimension(3) :: eje
      type(dualz4), dimension(3,3) :: fr
      type(dualz4), dimension(3) :: ejed

      ejed = eje
      fr = rot_mat_dz4(th, ejed)
    end function rot_mat_dz4C
    
    function rot_mat_dz4R(th, eje) result(fr)
      type(dualz4), intent(in) :: th
      real(8), intent(in), dimension(3) :: eje
      type(dualz4), dimension(3,3) :: fr
      type(dualz4), dimension(3) :: ejed

      ejed = eje
      fr = rot_mat_dz4(th, ejed)
    end function rot_mat_dz4R
    
    function rot_mat_Cdz4(th, eje) result(fr)
      complex(8), intent(in) :: th
      type(dualz4), intent(in), dimension(3) :: eje
      type(dualz4), dimension(3,3) :: fr
      type(dualz4) :: thd

      thd = th
      fr = rot_mat_dz4(thd, eje)
    end function rot_mat_Cdz4
    
    function rot_mat_Rdz4(th, eje) result(fr)
      real(8), intent(in) :: th
      type(dualz4), intent(in), dimension(3) :: eje
      type(dualz4), dimension(3,3) :: fr
      type(dualz4) :: thd

      thd = th
      fr = rot_mat_dz4(thd, eje)
    end function rot_mat_Rdz4    

    function f0_rot_matC(th, eje) result(fr)
      complex(8), intent(in) :: th
      complex(8), intent(in), dimension(3) :: eje
      complex(8), dimension(3,3) :: fr
      complex(8), dimension(3) :: ejeu     
      complex(8) :: n1, n2, n3

      !making 'eje' a unit vector
      ejeu = eje/sqrt(sum(eje*eje))

      n1 = ejeu(1); n2 = ejeu(2); n3 = ejeu(3)
      
      fr(1,:) = [1 + (n2**2 + n3**2)*(cos(th) - 1), n1*n2 - n1*n2*     &
           cos(th) - n3*sin(th), n1*n3 - n1*n3*cos(th) + n2*sin(th)]

      fr(2,:) = [n1*n2 - n1*n2*cos(th) + n3*sin(th), 1 + (n1**2 +      &
           n3**2)*(cos(th) - 1), n2*n3 - n2*n3*cos(th) - n1*sin(th)]

      fr(3,:) = [n1*n3 - n1*n3*cos(th) - n2*sin(th), n2*n3 - n2*n3*    &
           cos(th) + n1*sin(th), 1 + (n1**2 + n2**2)*(cos(th) - 1)]
    end function f0_rot_matC    

    function f0_rot_matRC(th, eje) result(fr)
      real(8), intent(in) :: th
      complex(8), intent(in), dimension(3) :: eje
      complex(8), dimension(3,3) :: fr
      complex(8) :: thc

      thc = th
      fr = f0_rot_matC(thc, eje)
    end function f0_rot_matRC
    
    function f0_rot_matCR(th, eje) result(fr)
      complex(8), intent(in) :: th
      real(8), intent(in), dimension(3) :: eje
      complex(8), dimension(3,3) :: fr
      complex(8), dimension(3) :: ejec

      ejec = eje
      fr = f0_rot_matC(th, ejec)
    end function f0_rot_matCR    

    function f0_rot_matR(th, eje) result(fr)
      real(8), intent(in) :: th
      real(8), intent(in), dimension(3) :: eje
      real(8), dimension(3) :: ejeu
      real(8), dimension(3,3) :: fr
      real(8) :: n1, n2, n3

      !making 'eje' a unit vector
      ejeu = eje/sqrt(sum(eje*eje))

      n1 = ejeu(1); n2 = ejeu(2); n3 = ejeu(3)
      
      fr(1,:) = [1 + (n2**2 + n3**2)*(cos(th) - 1), n1*n2 - n1*n2*     &
           cos(th) - n3*sin(th), n1*n3 - n1*n3*cos(th) + n2*sin(th)]

      fr(2,:) = [n1*n2 - n1*n2*cos(th) + n3*sin(th), 1 + (n1**2 +      &
           n3**2)*(cos(th) - 1), n2*n3 - n2*n3*cos(th) - n1*sin(th)]

      fr(3,:) = [n1*n3 - n1*n3*cos(th) - n2*sin(th), n2*n3 - n2*n3*    &
           cos(th) + n1*sin(th), 1 + (n1**2 + n2**2)*(cos(th) - 1)]
    end function f0_rot_matR

    !cross product of two dual vectors
    function cross_dz4(x,y) result(f_r)
      type(dualz4), dimension(3), intent(in) :: x, y
      type(dualz4), dimension(3)             :: f_r

      f_r = [x(2)*y(3) - x(3)*y(2), x(3)*y(1) - x(1)*y(3), x(1)*y(2)-  &
           x(2)*y(1)]
    end function cross_dz4

    function cross_dz4C(x,y) result(fr)
      type(dualz4), dimension(3), intent(in) :: x
      complex(8), dimension(3), intent(in) :: y
      type(dualz4), dimension(3) :: fr
      type(dualz4), dimension(3) :: yd

      yd = y
      fr = cross_dz4(x,yd)
    end function cross_dz4C

    function cross_dz4R(x,y) result(fr)
      type(dualz4), dimension(3), intent(in) :: x
      real(8), dimension(3), intent(in) :: y
      type(dualz4), dimension(3) :: fr
      type(dualz4), dimension(3) :: yd

      yd = y
      fr = cross_dz4(x,yd)
    end function cross_dz4R

    function cross_Cdz4(x,y) result(fr)
      complex(8), dimension(3), intent(in) :: x
      type(dualz4), dimension(3), intent(in) :: y      
      type(dualz4), dimension(3) :: fr
      type(dualz4), dimension(3) :: xd

      xd = x
      fr = cross_dz4(xd,y)
    end function cross_Cdz4
    
    function cross_Rdz4(x,y) result(fr)
      real(8), dimension(3), intent(in) :: x
      type(dualz4), dimension(3), intent(in) :: y      
      type(dualz4), dimension(3) :: fr
      type(dualz4), dimension(3) :: xd

      xd = x
      fr = cross_dz4(xd,y)
    end function cross_Rdz4

    function f0_crossC(x,y) result(f_r)
      complex(8), dimension(3), intent(in) :: x, y
      complex(8), dimension(3) :: f_r

      f_r = [x(2)*y(3) - x(3)*y(2), x(3)*y(1) - x(1)*y(3), x(1)*y(2)-  &
           x(2)*y(1)]
    end function f0_crossC

    function f0_crossCR(x,y) result(f_r)
      complex(8), dimension(3), intent(in) :: x
      real(8), dimension(3), intent(in) :: y
      complex(8), dimension(3) :: f_r      

      f_r = [x(2)*y(3) - x(3)*y(2), x(3)*y(1) - x(1)*y(3), x(1)*y(2)-  &
           x(2)*y(1)]
    end function f0_crossCR

    function f0_crossRC(x,y) result(f_r)
      real(8), dimension(3), intent(in) :: x
      complex(8), dimension(3), intent(in) :: y
      complex(8), dimension(3) :: f_r      

      f_r = [x(2)*y(3) - x(3)*y(2), x(3)*y(1) - x(1)*y(3), x(1)*y(2)-  &
           x(2)*y(1)]
    end function f0_crossRC
    
    function f0_crossR(x,y) result(f_r)
      real(8), dimension(3), intent(in) :: x, y
      real(8), dimension(3) :: f_r

      f_r = [x(2)*y(3) - x(3)*y(2), x(3)*y(1) - x(1)*y(3), x(1)*y(2)-  &
           x(2)*y(1)]
    end function f0_crossR

    !making a dual vector of norm 1
    function normalize_dz4(x) result(fr)
      type(dualz4), intent(in), dimension(:) :: x
      type(dualz4), dimension(size(x)) :: fr

      fr = x/norm_dz4(x)
    end function normalize_dz4

    function f0_normalizeC(x) result(fr)
      complex(8), intent(in), dimension(:) :: x
      complex(8), dimension(size(x)) :: fr

      fr = x/f0_normC(x)
    end function f0_normalizeC

    function f0_normalizeR(x) result(fr)
      real(8), intent(in), dimension(:) :: x
      real(8), dimension(size(x)) :: fr

      fr = x/f0_normR(x)
    end function f0_normalizeR
    
    !norm of a vector
    function norm_dz4(x) result(fr)
      type(dualz4), intent(in), dimension(:) :: x
      type(dualz4) :: fr

      fr = sqrt(sum(x*x))
    end function norm_dz4
    
    !Is not the norm of a complex vector. For this reason we used f0_norm
    !this hallows to use the complex step approximation to compute
    !derivatives
    function f0_normC(x) result(fr)
      complex(8), intent(in), dimension(:) :: x
      complex(8) :: fr

      fr = sqrt(sum(x*x))
    end function f0_normC

    function f0_normR(x) result(fr)
      real(8), intent(in), dimension(:) :: x
      real(8) :: fr

      fr = sqrt(sum(x*x))
    end function f0_normR
    
    function Gtangent_dz4(v,w) result(fr)
      type(dualz4), intent(in), dimension(:) :: v, w
      type(dualz4), dimension(size(v)) :: fr      
      type(dualz4), dimension(size(v)) :: vu

      vu = normalize_dz4(v)
      fr = normalize_dz4(w - sum(w*vu)*vu)
    end function Gtangent_dz4

    function Gtangent_dz4C(v,w) result(fr)
      type(dualz4), intent(in), dimension(:) :: v
      complex(8), intent(in), dimension(:) :: w
      type(dualz4), dimension(size(v)) :: fr      
      type(dualz4), dimension(size(w)) :: wd

      wd = w
      fr = Gtangent_dz4(v,wd)
    end function Gtangent_dz4C

    function Gtangent_dz4R(v,w) result(fr)
      type(dualz4), intent(in), dimension(:) :: v
      real(8), intent(in), dimension(:) :: w
      type(dualz4), dimension(size(v)) :: fr      
      type(dualz4), dimension(size(w)) :: wd

      wd = w
      fr = Gtangent_dz4(v,wd)
    end function Gtangent_dz4R

    function Gtangent_Cdz4(v,w) result(fr)
      complex(8), intent(in), dimension(:) :: v
      type(dualz4), intent(in), dimension(:) :: w
      type(dualz4), dimension(size(w)) :: fr      
      type(dualz4), dimension(size(v)) :: vd

      vd = v
      fr = Gtangent_dz4(vd,w)
    end function Gtangent_Cdz4    

    function Gtangent_Rdz4(v,w) result(fr)
      real(8), intent(in), dimension(:) :: v
      type(dualz4), intent(in), dimension(:) :: w
      type(dualz4), dimension(size(w)) :: fr      
      type(dualz4), dimension(size(v)) :: vd

      vd = v
      fr = Gtangent_dz4(vd,w)
    end function Gtangent_Rdz4 
    
    function f0_GtangentC(v,w) result(fr)
      complex(8), intent(in), dimension(:) :: v, w
      complex(8), dimension(size(v)) :: fr      
      complex(8), dimension(size(v)) :: vu

      vu = f0_normalizeC(v)
      fr = f0_normalizeC(w - sum(w*vu)*vu)
    end function f0_GtangentC

    function f0_GtangentCR(v,w) result(fr)
      complex(8), intent(in), dimension(:) :: v
      real(8), intent(in), dimension(:) :: w
      complex(8), dimension(size(v)) :: fr
      complex(8), dimension(size(w)) :: wc

      wc = w
      fr = f0_GtangentC(v,wc)
    end function f0_GtangentCR

    function f0_GtangentRC(v,w) result(fr)
      real(8), intent(in), dimension(:) :: v
      complex(8), intent(in), dimension(:) :: w
      complex(8), dimension(size(v)) :: fr
      complex(8), dimension(size(v)) :: vc

      vc = v
      fr = f0_GtangentC(vc,w)
    end function f0_GtangentRC
    
    function f0_GtangentR(v,w) result(fr)
      real(8), intent(in), dimension(:) :: v, w
      real(8), dimension(size(v)) :: fr      
      real(8), dimension(size(v)) :: vu

      vu = f0_normalizeR(v)
      fr = f0_normalizeR(w - sum(w*vu)*vu)
    end function f0_GtangentR
  end module mech_mod


