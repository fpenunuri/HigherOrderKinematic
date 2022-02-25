!module for the subroutine which computes position, velocity,
!acceleration, jerk and jounce/snap
module kinematic4_mod
  use dualz4_mod
  implicit none
  private

  public :: kinematic4

  interface kinematic4
     module procedure kinematic4C  !vector arguments are complex
     module procedure kinematic4R  !vector arguments are real      
     module procedure kinematic4RC
     module procedure kinematic4CR
     module procedure kinematic4FFC
     module procedure kinematic4FFR
  end interface kinematic4

contains
  !computes kinematic variables for f(x1(t),x2(t),...xm(t))
  !with x1(t), x2(t)..., xm(t), given by g(t)=[x1(t),...xm(t)]
  !if f deppends explicitly on time, use xm(t) = t
  !t0 is the evaluation time (could be complex)
   !f:Dm-->Dn, g:D1-->Dm
  subroutine kinematic4FFC(f, g, t0, pos, vel, acel, jerk, jounce_snap)
    interface
       !f: Dm-->Dn
       function f(r) result(f_result)
         use dualz4_mod
         type(dualz4), intent(in), dimension(:) :: r
         type(dualz4), allocatable, dimension(:) :: f_result
       end function f

       !g: D1-->Dm-1
       function g(t) result(g_result)
         use dualz4_mod
         type(dualz4), intent(in) :: t
         type(dualz4), allocatable, dimension(:) :: g_result
       end function g
    end interface

    complex(8), intent(in) :: t0
    complex(8), intent(out), dimension(:) :: pos, vel, acel, jerk
    complex(8), intent(out), dimension(:) :: jounce_snap
    type(dualz4) :: td
    type(dualz4), allocatable, dimension(:) :: gval
    integer :: dimg

    td%f0 = t0
    td%f1 = 1
    td%f2 = 0
    td%f3 = 0
    td%f4 = 0

    dimg = size(g(td))
    allocate(gval(dimg))

    gval = g(td)

    call kinematic4C(f, gval%f0, gval%f1, gval%f2, gval%f3, gval%f4,   &
         pos, vel, acel, jerk, jounce_snap)
    deallocate(gval)
  end subroutine kinematic4FFC

  !computes kinematic variables for f(x1(t),x2(t),...xm(t))
  !with x1(t), x2(t)..., xm(t), given by g(t)=[x1(t),...xm(t)]
  !if f deppends explicitly on time, use xm(t) = t
  !t0 is the evaluation time. All the variables are real variables,
  !except the functions f and g which are dualz4 variables;
  !f:Dm-->Dn, g:D1-->Dm
  subroutine kinematic4FFR(f,g,Rt0,Rpos,Rvel,Racel,Rjerk,Rjounce_snap)
    interface
       !f: Dm-->Dn
       function f(r) result(f_result)
         use dualz4_mod
         type(dualz4), intent(in), dimension(:) :: r
         type(dualz4), allocatable, dimension(:) :: f_result
       end function f

       !g: D1-->Dm
       function g(t) result(g_result)
         use dualz4_mod
         type(dualz4), intent(in) :: t
         type(dualz4), allocatable, dimension(:) :: g_result
       end function g
    end interface

    real(8), intent(in) :: Rt0
    real(8), intent(out), dimension(:) :: Rpos, Rvel, Racel, Rjerk
    real(8), intent(out), dimension(:) :: Rjounce_snap
    type(dualz4) :: td
    type(dualz4), allocatable, dimension(:) :: gval
    integer :: dimg

    td%f0 = Rt0
    td%f1 = 1
    td%f2 = 0
    td%f3 = 0
    td%f4 = 0

    dimg = size(g(td))
    allocate(gval(dimg))

    gval = g(td)   
    
    call kinematic4CR(f, gval%f0, gval%f1, gval%f2, gval%f3, gval%f4,  &
         Rpos, Rvel, Racel, Rjerk, Rjounce_snap)
    deallocate(gval)
  end subroutine kinematic4FFR
    
  !input vector arguments real, and output arguments complex
  subroutine kinematic4RC(f, x, x1p, x2p, x3p, x4p, pos, vel, acel,  &
       jerk, jounce_snap)
    interface
       !f: Dm-->Dn
       function f(r) result(f_result)
         use dualz4_mod
         type(dualz4), intent(in), dimension(:) :: r
         type(dualz4), allocatable, dimension(:) :: f_result
       end function f
    end interface

    real(8), intent(in),  dimension(:) :: x, x1p, x2p, x3p, x4p
    complex(8), intent(out), dimension(:) :: pos, vel, acel, jerk
    complex(8), intent(out), dimension(:) :: jounce_snap
    complex(8), parameter :: uc = (1d0,0d0)

    call kinematic4C(f, x*uc, x1p*uc, x2p*uc, x3p*uc, x4p*uc, pos,   &
         vel, acel, jerk, jounce_snap)
  end subroutine kinematic4RC

  !input vector arguments complex, and output arguments real
  subroutine kinematic4CR(f, x, x1p, x2p, x3p, x4p, pos, vel, acel,  &
       jerk, jounce_snap)
    interface
       !f: Dm-->Dn
       function f(r) result(f_result)
         use dualz4_mod
         type(dualz4), intent(in), dimension(:) :: r
         type(dualz4), allocatable, dimension(:) :: f_result
       end function f
    end interface
    complex(8), intent(in),  dimension(:) :: x, x1p, x2p, x3p, x4p
    real(8), intent(out), dimension(:) :: pos, vel, acel, jerk
    real(8), intent(out), dimension(:) :: jounce_snap
    type(dualz4), parameter :: eps1 = dualz4(0,1,0,0,0)
    type(dualz4), allocatable, dimension(:) :: fv1, fv2, fv3, fv4
    type(dualz4), allocatable, dimension(:) :: fv1mas2, fv2menos1
    type(dualz4), allocatable, dimension(:) :: fv1mas3
    integer :: Dn
    type(dualz4), dimension(size(x)) :: xd4
    complex(8), allocatable, dimension(:) :: Cpos, Cvel, Cacel, Cjerk
    complex(8), allocatable, dimension(:) :: Cjounce_snap

    xd4 = x
    Dn = size(f(xd4))

    allocate(fv1(Dn)); allocate(fv2(Dn)); allocate(fv3(Dn))
    allocate(fv4(Dn)); allocate(fv1mas2(Dn)); allocate(fv2menos1(Dn))
    allocate(fv1mas3(Dn))

    fv1 = f(x + eps1*x1p)
    fv2 = f(x + eps1*x2p)
    fv3 = f(x + eps1*x3p)
    fv4 = f(x + eps1*x4p)

    fv1mas2   = f(x + eps1*(x1p + x2p))
    fv2menos1 = f(x + eps1*(x2p - x1p))
    fv1mas3   = f(x + eps1*(x1p + x3p))

    allocate(Cpos(Dn)); allocate(Cvel(Dn)); allocate(Cacel(Dn))
    allocate(Cjerk(Dn)); allocate(Cjounce_snap(Dn))

    Cpos  = fv1%f0
    Cvel  = fv1%f1
    Cacel = fv1%f2 + fv2%f1
    Cjerk = fv1%f3 + 1.5*(fv1mas2%f2 -fv2%f2 - fv1%f2) + fv3%f1
    Cjounce_snap = fv1%f4 + f3part_dz4(fv1mas2 + fv2menos1- 2*fv2) +  &
         2*f2part_dz4((fv1mas3 - fv3 - fv1 + 1.5*fv2)) + fv4%f1

    if(any(aimag([Cpos,Cvel,Cacel,Cjerk,Cjounce_snap])/=0)) then
       print*, 'warning! complex values encountered '// &
            'perhap you want to change to complex the kind '// &
            'of the kinematic variables'
    end if    

    pos  = Cpos
    vel  = Cvel
    acel = Cacel
    jerk = Cjerk
    jounce_snap = Cjounce_snap
    
    deallocate(fv1,fv2,fv3,fv4)
    deallocate(fv1mas2,fv2menos1,fv1mas3)
    deallocate(Cpos,Cvel,Cacel,Cjerk,Cjounce_snap)
  end subroutine kinematic4CR

  !complex case, that is to say, x, x1p, ... jounce_snap are complex
  !vectors. f is a dual function, f:Dm-->Dn, the dual version of
  !f:Rm-->Rn. Dm = size(x), Dn = size(f) 
  subroutine kinematic4C(f, x, x1p, x2p, x3p, x4p, pos, vel, acel, &
       jerk, jounce_snap)
    interface
       !f: Dm-->Dn
       function f(r) result(f_result)
         use dualz4_mod
         type(dualz4), intent(in), dimension(:) :: r
         type(dualz4), allocatable, dimension(:) :: f_result
       end function f
    end interface

    complex(8), intent(in),  dimension(:) :: x, x1p, x2p, x3p, x4p
    complex(8), intent(out), dimension(:) :: pos, vel, acel, jerk
    complex(8), intent(out), dimension(:) :: jounce_snap

    type(dualz4), parameter :: eps1 = dualz4(0,1,0,0,0)
    type(dualz4), allocatable, dimension(:) :: fv1, fv2, fv3, fv4
    type(dualz4), allocatable, dimension(:) :: fv1mas2, fv2menos1
    type(dualz4), allocatable, dimension(:) :: fv1mas3
    integer :: Dn
    type(dualz4), dimension(size(x)) :: xd4

    xd4 = x
    Dn = size(f(xd4))

    allocate(fv1(Dn)); allocate(fv2(Dn)); allocate(fv3(Dn))
    allocate(fv4(Dn)); allocate(fv1mas2(Dn)); allocate(fv2menos1(Dn))
    allocate(fv1mas3(Dn))

    fv1 = f(x + eps1*x1p)
    fv2 = f(x + eps1*x2p)
    fv3 = f(x + eps1*x3p)
    fv4 = f(x + eps1*x4p)

    fv1mas2   = f(x + eps1*(x1p + x2p))
    fv2menos1 = f(x + eps1*(x2p - x1p))
    fv1mas3   = f(x + eps1*(x1p + x3p))

    pos  = fv1%f0
    vel  = fv1%f1
    acel = fv1%f2 + fv2%f1
    jerk = fv1%f3 + 1.5*(fv1mas2%f2 -fv2%f2 - fv1%f2) + fv3%f1
    jounce_snap = fv1%f4 + f3part_dz4(fv1mas2 + fv2menos1- 2*fv2) +  &
         2*f2part_dz4((fv1mas3 - fv3 - fv1 + 1.5*fv2)) + fv4%f1

    deallocate(fv1,fv2,fv3,fv4)
    deallocate(fv1mas2,fv2menos1,fv1mas3)
  end subroutine kinematic4C

  !Real case, that is to say, x, x1p, ... jounce_snap are real vectors
  !f is a dual function, f:Dm-->Dn, the dual version of f:Rm-->Rn
  !Dm = size(x), Dn = size(f) 
  subroutine kinematic4R(f, x, x1p, x2p, x3p, x4p, pos, vel, acel, &
       jerk, jounce_snap)
    interface
       !f: Dm-->Dn
       function f(r) result(f_result)
         use dualz4_mod
         type(dualz4), intent(in), dimension(:) :: r
         type(dualz4), allocatable, dimension(:) :: f_result
       end function f
    end interface

    real(8), intent(in),  dimension(:) :: x, x1p, x2p, x3p, x4p
    real(8), intent(out), dimension(:) :: pos, vel, acel, jerk
    real(8), intent(out), dimension(:) :: jounce_snap

    type(dualz4), parameter :: eps1 = dualz4(0,1,0,0,0)
    type(dualz4), allocatable, dimension(:) :: fv1, fv2, fv3, fv4
    type(dualz4), allocatable, dimension(:) :: fv1mas2, fv2menos1
    type(dualz4), allocatable, dimension(:) :: fv1mas3
    integer :: Dn
    type(dualz4), dimension(size(x)) :: xd4
    complex(8), allocatable, dimension(:) :: Cpos, Cvel, Cacel, Cjerk
    complex(8), allocatable, dimension(:) :: Cjounce_snap

    xd4 = x
    Dn = size(f(xd4))

    allocate(fv1(Dn)); allocate(fv2(Dn)); allocate(fv3(Dn))
    allocate(fv4(Dn)); allocate(fv1mas2(Dn)); allocate(fv2menos1(Dn))
    allocate(fv1mas3(Dn))

    fv1 = f(x + eps1*x1p)
    fv2 = f(x + eps1*x2p)
    fv3 = f(x + eps1*x3p)
    fv4 = f(x + eps1*x4p)

    fv1mas2   = f(x + eps1*(x1p + x2p))
    fv2menos1 = f(x + eps1*(x2p - x1p))
    fv1mas3   = f(x + eps1*(x1p + x3p))

    allocate(Cpos(Dn)); allocate(Cvel(Dn)); allocate(Cacel(Dn))
    allocate(Cjerk(Dn)); allocate(Cjounce_snap(Dn))

    Cpos  = fv1%f0
    Cvel  = fv1%f1
    Cacel = fv1%f2 + fv2%f1
    Cjerk = fv1%f3 + 1.5*(fv1mas2%f2 -fv2%f2 - fv1%f2) + fv3%f1
    Cjounce_snap = fv1%f4 + f3part_dz4(fv1mas2 + fv2menos1- 2*fv2) +  &
         2*f2part_dz4((fv1mas3 - fv3 - fv1 + 1.5*fv2)) + fv4%f1

    if(any(aimag([Cpos,Cvel,Cacel,Cjerk,Cjounce_snap])/=0)) then
       print*, 'warning! complex values encountered '// &
            'perhap you want to change to complex the kind '// &
            'of the kinematic variables'
    end if

    pos  = Cpos
    vel  = Cvel
    acel = Cacel
    jerk = Cjerk
    jounce_snap = Cjounce_snap
    
    deallocate(fv1,fv2,fv3,fv4)
    deallocate(fv1mas2,fv2menos1,fv1mas3)
    deallocate(Cpos,Cvel,Cacel,Cjerk,Cjounce_snap)
  end subroutine kinematic4R
end module kinematic4_mod

