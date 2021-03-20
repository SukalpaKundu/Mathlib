!!Following routines provide wide range of mathematical tools, details can be found in documentation.
!!For reporting errors/bugs, kindly mail to sukalpa.k123@gmail.com.



module mathlib
implicit none
contains

!intlib
double precision function int11(f,a,b,h)
double precision::a,b,p,f,h,h1,xy1,xy2
integer::n,i
integer,parameter::dp=selected_real_kind(8)
double precision,allocatable::g(:)


n=(b-a)/h
n=n+3-mod(n,3)
allocate(g(0:n))
h1=(b-a)/dfloat(n)
do i=0,n
g(i)=f(a+dfloat(i)*h1)
end do

p=0
do i=1,n-1
if(mod(i,3).ne.0) then
p=p+3*g(i)
elseif(mod(i,3).eq.0) then
p=p+2*g(i)
end if
end do

xy1=(p+g(0)+g(n))*3*h1/8

int11=xy1
deallocate(g)

end function int11



double precision function int21(f,x,a,b,h)
double precision::a,b,p,f,h,h1,xy1,xy2,x
integer::n,i
integer,parameter::dp=selected_real_kind(8)
double precision,allocatable::g(:)


n=(b-a)/h
n=n+3-mod(n,3)
allocate(g(0:n))
h1=(b-a)/dfloat(n)
do i=0,n
g(i)=f(x,a+dfloat(i)*h1)
end do

p=0
do i=1,n-1
if(mod(i,3).ne.0) then
p=p+3*g(i)
elseif(mod(i,3).eq.0) then
p=p+2*g(i)
end if
end do

xy1=(p+g(0)+g(n))*3*h1/8

int21=xy1
deallocate(g)

end function int21

double precision function int31(f,x,y,a,b,h)
double precision::a,b,p,f,h,h1,xy1,xy2,x,y
integer::n,i
integer,parameter::dp=selected_real_kind(8)
double precision,allocatable::g(:)


n=(b-a)/h
n=n+3-mod(n,3)
allocate(g(0:n))
h1=(b-a)/dfloat(n)
do i=0,n
g(i)=f(x,y,a+dfloat(i)*h1)
end do

p=0
do i=1,n-1
if(mod(i,3).ne.0) then
p=p+3*g(i)
elseif(mod(i,3).eq.0) then
p=p+2*g(i)
end if
end do

xy1=(p+g(0)+g(n))*3*h1/8

int31=xy1
deallocate(g)

end function int31

double precision function int41(f,x,y,z,a,b,h)
double precision::a,b,p,f,h,h1,xy1,xy2,x,y,z
integer::n,i
integer,parameter::dp=selected_real_kind(8)
double precision,allocatable::g(:)

n=(b-a)/h
n=n+3-mod(n,3)
allocate(g(0:n))
h1=(b-a)/dfloat(n)
do i=0,n
g(i)=f(x,y,z,a+dfloat(i)*h1)
end do

p=0
do i=1,n-1
if(mod(i,3).ne.0) then
p=p+3*g(i)
elseif(mod(i,3).eq.0) then
p=p+2*g(i)
end if
end do

xy1=(p+g(0)+g(n))*3*h1/8

int41=xy1
deallocate(g)

end function int41


double precision function int51(f,x,y,z,w,a,b,h)
double precision::a,b,p,f,h,h1,xy1,xy2,x,y,z,w
integer::n,i
integer,parameter::dp=selected_real_kind(8)
double precision,allocatable::g(:)


n=(b-a)/h
n=n+3-mod(n,3)
allocate(g(0:n))
h1=(b-a)/dfloat(n)
do i=0,n
g(i)=f(x,y,z,w,a+dfloat(i)*h1)
end do

p=0
do i=1,n-1
if(mod(i,3).ne.0) then
p=p+3*g(i)
elseif(mod(i,3).eq.0) then
p=p+2*g(i)
end if
end do

xy1=(p+g(0)+g(n))*3*h1/8

int51=xy1
deallocate(g)
end function int51

double precision function int1r1(f,a,b,error)
double precision::a,b,p1,p2,f,h,h1,xy1,xy2,error,h2
integer::n1,n2,i,n
integer,parameter::dp=selected_real_kind(8)
double precision,allocatable::g1(:),g2(:)
n=(b-a)/0.1

do while(abs(xy2-xy1)/abs(xy1) .gt. error)
h=(b-a)/dfloat(n)
h1=h
h2=h/2
n1=(b-a)/h1
n2=(b-a)/h2
n1=n1+3-mod(n1,3)
n2=n2+3-mod(n2,3)
allocate(g1(0:n1),g2(0:n2))

h1=(b-a)/dfloat(n1)
h2=(b-a)/dfloat(n2)

do i=0,n1
g1(i)=f(a+dfloat(i)*h1)
end do
do i=0,n2
g2(i)=f(a+dfloat(i)*h2)
end do



p1=0
do i=1,n1-1
if(mod(i,3).ne.0) then
p1=p1+3*g1(i)
elseif(mod(i,3).eq.0) then
p1=p1+2*g1(i)
end if
end do

p2=0
do i=1,n2-1
if(mod(i,3).ne.0) then
p2=p2+3*g2(i)
elseif(mod(i,3).eq.0) then
p2=p2+2*g2(i)
end if
end do

xy1=(p1+g1(0)+g1(n1))*3*h1/8
xy2=(p2+g2(0)+g2(n2))*3*h2/8
n=n*5
deallocate(g1,g2)
end do
int1r1=xy2


end function int1r1

double precision function int2r1(f,x,a,b,error)
double precision::a,b,p1,p2,f,h,h1,xy1,xy2,error,h2,x
integer::n1,n2,i,n
integer,parameter::dp=selected_real_kind(8)
double precision,allocatable::g1(:),g2(:)
n=(b-a)/0.1

do while(abs(xy2-xy1)/abs(xy1) .gt. error)
h=(b-a)/dfloat(n)
h1=h
h2=h/2
n1=(b-a)/h1
n2=(b-a)/h2
n1=n1+3-mod(n1,3)
n2=n2+3-mod(n2,3)
allocate(g1(0:n1),g2(0:n2))
h1=(b-a)/dfloat(n1)
h2=(b-a)/dfloat(n2)

do i=0,n1
g1(i)=f(x,a+dfloat(i)*h1)
end do
do i=0,n2
g2(i)=f(x,a+dfloat(i)*h2)
end do



p1=0
do i=1,n1-1
if(mod(i,3).ne.0) then
p1=p1+3*g1(i)
elseif(mod(i,3).eq.0) then
p1=p1+2*g1(i)
end if
end do

p2=0
do i=1,n2-1
if(mod(i,3).ne.0) then
p2=p2+3*g2(i)
elseif(mod(i,3).eq.0) then
p2=p2+2*g2(i)
end if
end do

xy1=(p1+g1(0)+g1(n1))*3*h1/8
xy2=(p2+g2(0)+g2(n2))*3*h2/8
n=n*5
deallocate(g1,g2)
end do
int2r1=xy2
end function int2r1

double precision function int3r1(f,x,y,a,b,error)
double precision::a,b,p1,p2,f,h,h1,xy1,xy2,error,h2,x,y
integer::n1,n2,i,n
integer,parameter::dp=selected_real_kind(8)
double precision,allocatable::g1(:),g2(:)
n=(b-a)/0.1

do while(abs(xy2-xy1)/abs(xy1) .gt. error)
h=(b-a)/dfloat(n)
h1=h
h2=h/2
n1=(b-a)/h1
n2=(b-a)/h2
n1=n1+3-mod(n1,3)
n2=n2+3-mod(n2,3)
allocate(g1(0:n1),g2(0:n2))
h1=(b-a)/dfloat(n1)
h2=(b-a)/dfloat(n2)

do i=0,n1
g1(i)=f(x,y,a+dfloat(i)*h1)
end do
do i=0,n2
g2(i)=f(x,y,a+dfloat(i)*h2)
end do



p1=0
do i=1,n1-1
if(mod(i,3).ne.0) then
p1=p1+3*g1(i)
elseif(mod(i,3).eq.0) then
p1=p1+2*g1(i)
end if
end do

p2=0
do i=1,n2-1
if(mod(i,3).ne.0) then
p2=p2+3*g2(i)
elseif(mod(i,3).eq.0) then
p2=p2+2*g2(i)
end if
end do

xy1=(p1+g1(0)+g1(n1))*3*h1/8
xy2=(p2+g2(0)+g2(n2))*3*h2/8
n=n*5
deallocate(g1,g2)
end do
int3r1=xy2


end function int3r1



double precision function int4r1(f,x,y,z,a,b,error)
double precision::a,b,p1,p2,f,h,h1,xy1,xy2,error,h2,x,y,z
integer::n1,n2,i,n
integer,parameter::dp=selected_real_kind(8)
double precision,allocatable::g1(:),g2(:)
n=(b-a)/0.1

do while(abs(xy2-xy1)/abs(xy1) .gt. error)
h=(b-a)/dfloat(n)
h1=h
h2=h/2
n1=(b-a)/h1
n2=(b-a)/h2
n1=n1+3-mod(n1,3)
n2=n2+3-mod(n2,3)
allocate(g1(0:n1),g2(0:n2))
h1=(b-a)/dfloat(n1)
h2=(b-a)/dfloat(n2)

do i=0,n1
g1(i)=f(x,y,z,a+dfloat(i)*h1)
end do
do i=0,n2
g2(i)=f(x,y,z,a+dfloat(i)*h2)
end do



p1=0
do i=1,n1-1
if(mod(i,3).ne.0) then
p1=p1+3*g1(i)
elseif(mod(i,3).eq.0) then
p1=p1+2*g1(i)
end if
end do

p2=0
do i=1,n2-1
if(mod(i,3).ne.0) then
p2=p2+3*g2(i)
elseif(mod(i,3).eq.0) then
p2=p2+2*g2(i)
end if
end do

xy1=(p1+g1(0)+g1(n1))*3*h1/8
xy2=(p2+g2(0)+g2(n2))*3*h2/8
n=n*5
deallocate(g1,g2)
end do
int4r1=xy2


end function int4r1


double precision function int5r1(f,x,y,z,w,a,b,error)
double precision::a,b,p1,p2,f,h,h1,xy1,xy2,error,h2,x,y,z,w
integer::n1,n2,i,n
integer,parameter::dp=selected_real_kind(8)
double precision,allocatable::g1(:),g2(:)
n=(b-a)/0.1

do while(abs(xy2-xy1)/abs(xy1) .gt. error)
h=(b-a)/dfloat(n)
h1=h
h2=h/2
n1=(b-a)/h1
n2=(b-a)/h2
n1=n1+3-mod(n1,3)
n2=n2+3-mod(n2,3)
allocate(g1(0:n1),g2(0:n2))
h1=(b-a)/dfloat(n1)
h2=(b-a)/dfloat(n2)

do i=0,n1
g1(i)=f(x,y,z,w,a+dfloat(i)*h1)
end do
do i=0,n2
g2(i)=f(x,y,z,w,a+dfloat(i)*h2)
end do



p1=0
do i=1,n1-1
if(mod(i,3).ne.0) then
p1=p1+3*g1(i)
elseif(mod(i,3).eq.0) then
p1=p1+2*g1(i)
end if
end do

p2=0
do i=1,n2-1
if(mod(i,3).ne.0) then
p2=p2+3*g2(i)
elseif(mod(i,3).eq.0) then
p2=p2+2*g2(i)
end if
end do

xy1=(p1+g1(0)+g1(n1))*3*h1/8
xy2=(p2+g2(0)+g2(n2))*3*h2/8
n=n*5
deallocate(g1,g2)
end do
int5r1=xy2
end function int5r1


double precision function int1a1(f,a,b,error)
double precision::a,b,p1,p2,f,h,h1,xy1,xy2,error,h2
integer::n1,n2,i,n
integer,parameter::dp=selected_real_kind(8)
double precision,allocatable::g1(:),g2(:)
n=(b-a)/0.1
xy2=10000
xy1=5

do while(abs(xy2-xy1) .gt. error)
h=(b-a)/dfloat(n)
h1=h
h2=h/2
n1=(b-a)/h1
n2=(b-a)/h2
n1=n1+3-mod(n1,3)
n2=n2+3-mod(n2,3)
allocate(g1(0:n1),g2(0:n2))
h1=(b-a)/dfloat(n1)
h2=(b-a)/dfloat(n2)

do i=0,n1
g1(i)=f(a+dfloat(i)*h1)
end do
do i=0,n2
g2(i)=f(a+dfloat(i)*h2)
end do



p1=0
do i=1,n1-1
if(mod(i,3).ne.0) then
p1=p1+3*g1(i)
elseif(mod(i,3).eq.0) then
p1=p1+2*g1(i)
end if
end do

p2=0
do i=1,n2-1
if(mod(i,3).ne.0) then
p2=p2+3*g2(i)
elseif(mod(i,3).eq.0) then
p2=p2+2*g2(i)
end if
end do

xy1=(p1+g1(0)+g1(n1))*3*h1/8
xy2=(p2+g2(0)+g2(n2))*3*h2/8
n=n*2
deallocate(g1,g2)
end do
int1a1=xy2
end function int1a1

double precision function int2a1(f,x,a,b,error)
double precision::a,b,p1,p2,f,h,h1,xy1,xy2,error,h2,x
integer::n1,n2,i,n
integer,parameter::dp=selected_real_kind(8)
double precision,allocatable::g1(:),g2(:)
n=(b-a)/0.1
xy2=10000
xy1=5

do while(abs(xy2-xy1) .gt. error)
h=(b-a)/dfloat(n)
h1=h
h2=h/2
n1=(b-a)/h1
n2=(b-a)/h2
n1=n1+3-mod(n1,3)
n2=n2+3-mod(n2,3)
allocate(g1(0:n1),g2(0:n2))
h1=(b-a)/dfloat(n1)
h2=(b-a)/dfloat(n2)

do i=0,n1
g1(i)=f(x,a+dfloat(i)*h1)
end do
do i=0,n2
g2(i)=f(x,a+dfloat(i)*h2)
end do



p1=0
do i=1,n1-1
if(mod(i,3).ne.0) then
p1=p1+3*g1(i)
elseif(mod(i,3).eq.0) then
p1=p1+2*g1(i)
end if
end do

p2=0
do i=1,n2-1
if(mod(i,3).ne.0) then
p2=p2+3*g2(i)
elseif(mod(i,3).eq.0) then
p2=p2+2*g2(i)
end if
end do

xy1=(p1+g1(0)+g1(n1))*3*h1/8
xy2=(p2+g2(0)+g2(n2))*3*h2/8
n=n*2
deallocate(g1,g2)
end do
int2a1=xy2
end function int2a1

double precision function int3a1(f,x,y,a,b,error)
double precision::a,b,p1,p2,f,h,h1,xy1,xy2,error,h2,x,y
integer::n1,n2,i,n
integer,parameter::dp=selected_real_kind(8)
double precision,allocatable::g1(:),g2(:)
n=(b-a)/0.1
xy2=10000
xy1=5

do while(abs(xy2-xy1) .gt. error)
h=(b-a)/dfloat(n)
h1=h
h2=h/2
n1=(b-a)/h1
n2=(b-a)/h2
n1=n1+3-mod(n1,3)
n2=n2+3-mod(n2,3)
allocate(g1(0:n1),g2(0:n2))
h1=(b-a)/dfloat(n1)
h2=(b-a)/dfloat(n2)

do i=0,n1
g1(i)=f(x,y,a+dfloat(i)*h1)
end do
do i=0,n2
g2(i)=f(x,y,a+dfloat(i)*h2)
end do



p1=0
do i=1,n1-1
if(mod(i,3).ne.0) then
p1=p1+3*g1(i)
elseif(mod(i,3).eq.0) then
p1=p1+2*g1(i)
end if
end do

p2=0
do i=1,n2-1
if(mod(i,3).ne.0) then
p2=p2+3*g2(i)
elseif(mod(i,3).eq.0) then
p2=p2+2*g2(i)
end if
end do

xy1=(p1+g1(0)+g1(n1))*3*h1/8
xy2=(p2+g2(0)+g2(n2))*3*h2/8
n=n*2
deallocate(g1,g2)
end do
int3a1=xy2

end function int3a1



double precision function int4a1(f,x,y,z,a,b,error)
double precision::a,b,p1,p2,f,h,h1,xy1,xy2,error,h2,x,y,z
integer::n1,n2,i,n
integer,parameter::dp=selected_real_kind(8)
double precision,allocatable::g1(:),g2(:)
n=(b-a)/0.1
xy2=10000
xy1=5

do while(abs(xy2-xy1) .gt. error)
h=(b-a)/dfloat(n)
h1=h
h2=h/2
n1=(b-a)/h1
n2=(b-a)/h2
n1=n1+3-mod(n1,3)
n2=n2+3-mod(n2,3)
allocate(g1(0:n1),g2(0:n2))
h1=(b-a)/dfloat(n1)
h2=(b-a)/dfloat(n2)

do i=0,n1
g1(i)=f(x,y,z,a+dfloat(i)*h1)
end do
do i=0,n2
g2(i)=f(x,y,z,a+dfloat(i)*h2)
end do



p1=0
do i=1,n1-1
if(mod(i,3).ne.0) then
p1=p1+3*g1(i)
elseif(mod(i,3).eq.0) then
p1=p1+2*g1(i)
end if
end do

p2=0
do i=1,n2-1
if(mod(i,3).ne.0) then
p2=p2+3*g2(i)
elseif(mod(i,3).eq.0) then
p2=p2+2*g2(i)
end if
end do

xy1=(p1+g1(0)+g1(n1))*3*h1/8
xy2=(p2+g2(0)+g2(n2))*3*h2/8
n=n*2
deallocate(g1,g2)
end do
int4a1=xy2


end function int4a1


double precision function int5a1(f,x,y,z,w,a,b,error)
double precision::a,b,p1,p2,f,h,h1,xy1,xy2,error,h2,x,y,z,w
integer::n1,n2,i,n
integer,parameter::dp=selected_real_kind(8)
double precision,allocatable::g1(:),g2(:)
n=(b-a)/0.1
xy2=10000
xy1=5

do while(abs(xy2-xy1) .gt. error)
h=(b-a)/dfloat(n)
h1=h
h2=h/2
n1=(b-a)/h1
n2=(b-a)/h2
n1=n1+3-mod(n1,3)
n2=n2+3-mod(n2,3)
allocate(g1(0:n1),g2(0:n2))
h1=(b-a)/dfloat(n1)
h2=(b-a)/dfloat(n2)

do i=0,n1
g1(i)=f(x,y,z,w,a+dfloat(i)*h1)
end do
do i=0,n2
g2(i)=f(x,y,z,w,a+dfloat(i)*h2)
end do



p1=0
do i=1,n1-1
if(mod(i,3).ne.0) then
p1=p1+3*g1(i)
elseif(mod(i,3).eq.0) then
p1=p1+2*g1(i)
end if
end do

p2=0
do i=1,n2-1
if(mod(i,3).ne.0) then
p2=p2+3*g2(i)
elseif(mod(i,3).eq.0) then
p2=p2+2*g2(i)
end if
end do

xy1=(p1+g1(0)+g1(n1))*3*h1/8
xy2=(p2+g2(0)+g2(n2))*3*h2/8
n=n*2
deallocate(g1,g2)
end do
int5a1=xy2


end function int5a1


!2d


double precision function int22(f,xi,xf,yi,yf,h)
double precision::a,b,p,f,hx,hy,hx1,hx2,hy1,hy2,xy1,xy2,xf,xi,yf,yi,h
integer::i,j,nx1,ny1
integer,parameter::dp=selected_real_kind(8)
double precision,allocatable::g(:,:),q(:)


hx=h
hy=h
nx1=(xf-xi)/hx
nx1=nx1+3-mod(nx1,3)
hx1=(xf-xi)/dfloat(nx1)
ny1=(yf-yi)/hy
ny1=ny1+3-mod(ny1,3)
hy1=(yf-yi)/dfloat(ny1)
allocate(g(0:max(nx1,ny1),0:max(nx1,ny1)),q(0:max(nx1,ny1)))
if(nx1.gt.45000 .or. ny1.gt.45000) then
write(*,*) "error, maximum allowed stepsize in this grid is",max((xf-xi),(yf-yi))/20000
stop
end if
do j=0,nx1
do i=0,ny1
g(i,j)=f(xi+dfloat(i)*hx1,yi+dfloat(j)*hy1)
end do
end do

p=0
do i=0,nx1
q(i)=0
do j=1,ny1-1
if(mod(j,3).ne.0) then
q(i)=q(i)+3*g(i,j)
elseif(mod(j,3).eq.0) then
q(i)=q(i)+2*g(i,j)
end if
end do

q(i)=(q(i)+g(i,0)+g(i,ny1))*3*hy1/8
if(mod(i,3).ne.0 .and. i.gt.0 .and. i.lt.nx1) then
p=p+3*q(i)
elseif(mod(i,3).eq.0 .and. i.gt.0 .and. i.lt.nx1) then
p=p+2*q(i)
end if

end do

int22=(p+q(0)+q(nx1))*3*hx1/8
deallocate(g,q)

end function int22



double precision function int32(f,x,xi,xf,yi,yf,h)
double precision::a,b,p,f,hx,hy,hx1,hx2,hy1,hy2,xy1,xy2,xf,xi,yf,yi,h,x
integer::i,j,nx1,ny1
integer,parameter::dp=selected_real_kind(8)
double precision,allocatable::g(:,:),q(:)


hx=h
hy=h
nx1=(xf-xi)/hx
nx1=nx1+3-mod(nx1,3)
hx1=(xf-xi)/dfloat(nx1)
ny1=(yf-yi)/hy
ny1=ny1+3-mod(ny1,3)
hy1=(yf-yi)/dfloat(ny1)
allocate(g(0:max(nx1,ny1),0:max(nx1,ny1)),q(0:max(nx1,ny1)))
if(nx1.gt.45000 .or. ny1.gt.45000) then
write(*,*) "error, maximum allowed stepsize in this grid is",max((xf-xi),(yf-yi))/20000
stop
end if
do j=0,nx1
do i=0,ny1
g(i,j)=f(x,xi+dfloat(i)*hx1,yi+dfloat(j)*hy1)
end do
end do

p=0
do i=0,nx1
q(i)=0
do j=1,ny1-1
if(mod(j,3).ne.0) then
q(i)=q(i)+3*g(i,j)
elseif(mod(j,3).eq.0) then
q(i)=q(i)+2*g(i,j)
end if
end do

q(i)=(q(i)+g(i,0)+g(i,ny1))*3*hy1/8
if(mod(i,3).ne.0 .and. i.gt.0 .and. i.lt.nx1) then
p=p+3*q(i)
elseif(mod(i,3).eq.0 .and. i.gt.0 .and. i.lt.nx1) then
p=p+2*q(i)
end if
end do

int32=(p+q(0)+q(nx1))*3*hx1/8
deallocate(g,q)

end function int32

double precision function int42(f,x,y,xi,xf,yi,yf,h)
double precision::a,b,p,f,hx,hy,hx1,hx2,hy1,hy2,xy1,xy2,xf,xi,yf,yi,h,x,y
integer::i,j,nx1,ny1
integer,parameter::dp=selected_real_kind(8)
double precision,allocatable::g(:,:),q(:)


hx=h
hy=h
nx1=(xf-xi)/hx
nx1=nx1+3-mod(nx1,3)
hx1=(xf-xi)/dfloat(nx1)
ny1=(yf-yi)/hy
ny1=ny1+3-mod(ny1,3)
hy1=(yf-yi)/dfloat(ny1)
allocate(g(0:max(nx1,ny1),0:max(nx1,ny1)),q(0:max(nx1,ny1)))
if(nx1.gt.45000 .or. ny1.gt.45000) then
write(*,*) "error, maximum allowed stepsize in this grid is",max((xf-xi),(yf-yi))/20000
stop
end if
do j=0,nx1
do i=0,ny1
g(i,j)=f(x,y,xi+dfloat(i)*hx1,yi+dfloat(j)*hy1)
end do
end do

p=0
do i=0,nx1
q(i)=0
do j=1,ny1-1
if(mod(j,3).ne.0) then
q(i)=q(i)+3*g(i,j)
elseif(mod(j,3).eq.0) then
q(i)=q(i)+2*g(i,j)
end if
end do

q(i)=(q(i)+g(i,0)+g(i,ny1))*3*hy1/8
if(mod(i,3).ne.0 .and. i.gt.0 .and. i.lt.nx1) then
p=p+3*q(i)
elseif(mod(i,3).eq.0 .and. i.gt.0 .and. i.lt.nx1) then
p=p+2*q(i)
end if
end do

int42=(p+q(0)+q(nx1))*3*hx1/8
deallocate(g,q)

end function int42


double precision function int52(f,x,y,z,xi,xf,yi,yf,h)
double precision::a,b,p,f,hx,hy,hx1,hx2,hy1,hy2,xy1,xy2,xf,xi,yf,yi,h,x,y,z
integer::i,j,nx1,ny1
integer,parameter::dp=selected_real_kind(8)
double precision,allocatable::g(:,:),q(:)


hx=h
hy=h
nx1=(xf-xi)/hx
nx1=nx1+3-mod(nx1,3)
hx1=(xf-xi)/dfloat(nx1)
ny1=(yf-yi)/hy
ny1=ny1+3-mod(ny1,3)
hy1=(yf-yi)/dfloat(ny1)
allocate(g(0:max(nx1,ny1),0:max(nx1,ny1)),q(0:max(nx1,ny1)))
if(nx1.gt.45000 .or. ny1.gt.45000) then
write(*,*) "error, maximum allowed stepsize in this grid is",max((xf-xi),(yf-yi))/20000
stop
end if
do j=0,nx1
do i=0,ny1
g(i,j)=f(x,y,z,xi+dfloat(i)*hx1,yi+dfloat(j)*hy1)
end do
end do

p=0
do i=0,nx1
q(i)=0
do j=1,ny1-1
if(mod(j,3).ne.0) then
q(i)=q(i)+3*g(i,j)
elseif(mod(j,3).eq.0) then
q(i)=q(i)+2*g(i,j)
end if
end do

q(i)=(q(i)+g(i,0)+g(i,ny1))*3*hy1/8
if(mod(i,3).ne.0 .and. i.gt.0 .and. i.lt.nx1) then
p=p+3*q(i)
elseif(mod(i,3).eq.0 .and. i.gt.0 .and. i.lt.nx1) then
p=p+2*q(i)
end if
end do

int52=(p+q(0)+q(nx1))*3*hx1/8
deallocate(g,q)

end function int52







double precision function int2r2(f,xi,xf,yi,yf,error)
double precision::pa,pb,f,hx,hy,hx1a,hx1b,hy1a,hy1b,xf,xi,yf,yi,h,error,hya,hyb,hx1,hy1,hxa,hxb
integer::i,j,nx1a,nx1b,ny1a,ny1b,n
integer,parameter::dp=selected_real_kind(8)
double precision,allocatable::ga(:,:),gb(:,:),qa(:),qb(:)

n=1000

pb=10
pa=5
do while(abs(pb-pa)/abs(pa).gt.error)
h=max((yf-yi),(xf-xi))/dfloat(n)
hxa=h
hxb=h*dfloat(n)/dfloat(n+500)
nx1a=(xf-xi)/hxa
nx1b=(xf-xi)/hxb
nx1a=nx1a+3-mod(nx1a,3)
nx1b=nx1b+3-mod(nx1b,3)
hx1a=(xf-xi)/dfloat(nx1a)
hx1b=(xf-xi)/dfloat(nx1b)

hya=h
hyb=h*dfloat(n)/dfloat(n+500)
ny1a=(yf-yi)/hya
ny1b=(yf-yi)/hyb
ny1a=ny1a+3-mod(ny1a,3)
ny1b=ny1b+3-mod(ny1b,3)
hy1a=(yf-yi)/dfloat(ny1a)
hy1b=(yf-yi)/dfloat(ny1b)
allocate(ga(0:nx1a,0:ny1a),gb(0:nx1b,0:ny1b),&
qa(0:max(nx1a,nx1b,ny1a,ny1b)),qb(0:max(nx1a,nx1b,ny1a,ny1b)))
if(ny1a.gt. 45000 .or.ny1b.gt. 45000 .or. nx1a.gt. 45000 .or.nx1b.gt. 45000) then
write(*,*) "failed to converge, estimated percentage error is",abs(pb-pa)/abs(pb)*100,"%"
exit
end if

do j=0,nx1a
do i=0,ny1a
ga(i,j)=f(xi+dfloat(i)*hx1a,yi+dfloat(j)*hy1a)
end do
end do

pa=0
do i=0,nx1a
qa(i)=0
do j=1,ny1a-1
if(mod(j,3).ne.0) then
qa(i)=qa(i)+3*ga(i,j)
elseif(mod(j,3).eq.0) then
qa(i)=qa(i)+2*ga(i,j)
end if
end do

qa(i)=(qa(i)+ga(i,0)+ga(i,ny1a))*3*hy1a/8
if(mod(i,3).ne.0 .and. i.gt.0 .and. i.lt.nx1a) then
pa=pa+3*qa(i)
elseif(mod(i,3).eq.0 .and. i.gt.0 .and. i.lt.nx1a) then
pa=pa+2*qa(i)
end if
end do

pa=(pa+qa(0)+qa(nx1a))*3*hx1a/8


do j=0,nx1b
do i=0,ny1b
gb(i,j)=f(xi+dfloat(i)*hx1b,yi+dfloat(j)*hy1b)
end do
end do

pb=0
do i=0,nx1b
qb(i)=0
do j=1,ny1b-1
if(mod(j,3).ne.0) then
qb(i)=qb(i)+3*gb(i,j)
elseif(mod(j,3).eq.0) then
qb(i)=qb(i)+2*gb(i,j)
end if
end do

qb(i)=(qb(i)+gb(i,0)+gb(i,ny1b))*3*hy1b/8
if(mod(i,3).ne.0 .and. i.gt.0 .and. i.lt.nx1b) then
pb=pb+3*qb(i)
elseif(mod(i,3).eq.0 .and. i.gt.0 .and. i.lt.nx1b) then
pb=pb+2*qb(i)
end if
end do

pb=(pb+qb(0)+qb(nx1b))*3*hx1b/8
n=n*2
deallocate(ga,gb,qa,qb)
end do

int2r2=pb



end function int2r2




double precision function int3r2(f,x,xi,xf,yi,yf,error)
double precision::pa,pb,f,hx,hy,hx1a,hx1b,hy1a,hy1b,xf,xi,yf,yi,h,error,hya,hyb,hx1,hy1,hxa,hxb,x
integer::i,j,nx1a,nx1b,ny1a,ny1b,n
integer,parameter::dp=selected_real_kind(8)
double precision,allocatable::ga(:,:),gb(:,:),qa(:),qb(:)

n=1000

pb=10
pa=5
do while(abs(pb-pa)/abs(pa).gt.error)
h=max((yf-yi),(xf-xi))/dfloat(n)
hxa=h
hxb=h*dfloat(n)/dfloat(n+500)
nx1a=(xf-xi)/hxa
nx1b=(xf-xi)/hxb
nx1a=nx1a+3-mod(nx1a,3)
nx1b=nx1b+3-mod(nx1b,3)
hx1a=(xf-xi)/dfloat(nx1a)
hx1b=(xf-xi)/dfloat(nx1b)

hya=h
hyb=h*dfloat(n)/dfloat(n+500)
ny1a=(yf-yi)/hya
ny1b=(yf-yi)/hyb
ny1a=ny1a+3-mod(ny1a,3)
ny1b=ny1b+3-mod(ny1b,3)
hy1a=(yf-yi)/dfloat(ny1a)
hy1b=(yf-yi)/dfloat(ny1b)
allocate(ga(0:nx1a,0:ny1a),gb(0:nx1b,0:ny1b),&
qa(0:max(nx1a,nx1b,ny1a,ny1b)),qb(0:max(nx1a,nx1b,ny1a,ny1b)))
if(ny1a.gt. 45000 .or.ny1b.gt. 45000 .or. nx1a.gt. 45000 .or.nx1b.gt. 45000) then
write(*,*) "failed to converge, estimated percentage error is",abs(pb-pa)/abs(pb)*100,"%"
exit
end if

do j=0,nx1a
do i=0,ny1a
ga(i,j)=f(x,xi+dfloat(i)*hx1a,yi+dfloat(j)*hy1a)
end do
end do

pa=0
do i=0,nx1a
qa(i)=0
do j=1,ny1a-1
if(mod(j,3).ne.0) then
qa(i)=qa(i)+3*ga(i,j)
elseif(mod(j,3).eq.0) then
qa(i)=qa(i)+2*ga(i,j)
end if
end do

qa(i)=(qa(i)+ga(i,0)+ga(i,ny1a))*3*hy1a/8
if(mod(i,3).ne.0 .and. i.gt.0 .and. i.lt.nx1a) then
pa=pa+3*qa(i)
elseif(mod(i,3).eq.0 .and. i.gt.0 .and. i.lt.nx1a) then
pa=pa+2*qa(i)
end if
end do

pa=(pa+qa(0)+qa(nx1a))*3*hx1a/8


do j=0,nx1b
do i=0,ny1b
gb(i,j)=f(x,xi+dfloat(i)*hx1b,yi+dfloat(j)*hy1b)
end do
end do

pb=0
do i=0,nx1b
qb(i)=0
do j=1,ny1b-1
if(mod(j,3).ne.0) then
qb(i)=qb(i)+3*gb(i,j)
elseif(mod(j,3).eq.0) then
qb(i)=qb(i)+2*gb(i,j)
end if
end do

qb(i)=(qb(i)+gb(i,0)+gb(i,ny1b))*3*hy1b/8
if(mod(i,3).ne.0 .and. i.gt.0 .and. i.lt.nx1b) then
pb=pb+3*qb(i)
elseif(mod(i,3).eq.0 .and. i.gt.0 .and. i.lt.nx1b) then
pb=pb+2*qb(i)
end if
end do

pb=(pb+qb(0)+qb(nx1b))*3*hx1b/8
n=n*2
deallocate(ga,gb,qa,qb)
end do

int3r2=pb



end function int3r2





double precision function int4r2(f,x,y,xi,xf,yi,yf,error)
double precision::pa,pb,f,hx,hy,hx1a,hx1b,hy1a,hy1b,xf,xi,yf,yi,h,error,hya,hyb,hx1,hy1,hxa,hxb,x,y
integer::i,j,nx1a,nx1b,ny1a,ny1b,n
integer,parameter::dp=selected_real_kind(8)
double precision,allocatable::ga(:,:),gb(:,:),qa(:),qb(:)

n=1000

pb=10
pa=5
do while(abs(pb-pa)/abs(pa).gt.error)
h=max((yf-yi),(xf-xi))/dfloat(n)
hxa=h
hxb=h*dfloat(n)/dfloat(n+500)
nx1a=(xf-xi)/hxa
nx1b=(xf-xi)/hxb
nx1a=nx1a+3-mod(nx1a,3)
nx1b=nx1b+3-mod(nx1b,3)
hx1a=(xf-xi)/dfloat(nx1a)
hx1b=(xf-xi)/dfloat(nx1b)

hya=h
hyb=h*dfloat(n)/dfloat(n+500)
ny1a=(yf-yi)/hya
ny1b=(yf-yi)/hyb
ny1a=ny1a+3-mod(ny1a,3)
ny1b=ny1b+3-mod(ny1b,3)
hy1a=(yf-yi)/dfloat(ny1a)
hy1b=(yf-yi)/dfloat(ny1b)
allocate(ga(0:nx1a,0:ny1a),gb(0:nx1b,0:ny1b),&
qa(0:max(nx1a,nx1b,ny1a,ny1b)),qb(0:max(nx1a,nx1b,ny1a,ny1b)))
if(ny1a.gt. 45000 .or.ny1b.gt. 45000 .or. nx1a.gt. 45000 .or.nx1b.gt. 45000) then
write(*,*) "failed to converge, estimated percentage error is",abs(pb-pa)/abs(pb)*100,"%"
exit
end if

do j=0,nx1a
do i=0,ny1a
ga(i,j)=f(x,y,xi+dfloat(i)*hx1a,yi+dfloat(j)*hy1a)
end do
end do

pa=0
do i=0,nx1a
qa(i)=0
do j=1,ny1a-1
if(mod(j,3).ne.0) then
qa(i)=qa(i)+3*ga(i,j)
elseif(mod(j,3).eq.0) then
qa(i)=qa(i)+2*ga(i,j)
end if
end do

qa(i)=(qa(i)+ga(i,0)+ga(i,ny1a))*3*hy1a/8
if(mod(i,3).ne.0 .and. i.gt.0 .and. i.lt.nx1a) then
pa=pa+3*qa(i)
elseif(mod(i,3).eq.0 .and. i.gt.0 .and. i.lt.nx1a) then
pa=pa+2*qa(i)
end if
end do

pa=(pa+qa(0)+qa(nx1a))*3*hx1a/8


do j=0,nx1b
do i=0,ny1b
gb(i,j)=f(x,y,xi+dfloat(i)*hx1b,yi+dfloat(j)*hy1b)
end do
end do

pb=0
do i=0,nx1b
qb(i)=0
do j=1,ny1b-1
if(mod(j,3).ne.0) then
qb(i)=qb(i)+3*gb(i,j)
elseif(mod(j,3).eq.0) then
qb(i)=qb(i)+2*gb(i,j)
end if
end do

qb(i)=(qb(i)+gb(i,0)+gb(i,ny1b))*3*hy1b/8
if(mod(i,3).ne.0 .and. i.gt.0 .and. i.lt.nx1b) then
pb=pb+3*qb(i)
elseif(mod(i,3).eq.0 .and. i.gt.0 .and. i.lt.nx1b) then
pb=pb+2*qb(i)
end if
end do

pb=(pb+qb(0)+qb(nx1b))*3*hx1b/8
n=n*2
deallocate(ga,gb,qa,qb)
end do

int4r2=pb



end function int4r2



double precision function int5r2(f,x,y,z,xi,xf,yi,yf,error)
double precision::pa,pb,f,hx,hy,hx1a,hx1b,hy1a,hy1b,xf,xi,yf,yi,h,error,hya,hyb,hx1,hy1,hxa,hxb,x,y,z
integer::i,j,nx1a,nx1b,ny1a,ny1b,n
integer,parameter::dp=selected_real_kind(8)
double precision,allocatable::ga(:,:),gb(:,:),qa(:),qb(:)

n=1000

pb=10
pa=5
do while(abs(pb-pa)/abs(pa).gt.error)
h=max((yf-yi),(xf-xi))/dfloat(n)
hxa=h
hxb=h*dfloat(n)/dfloat(n+500)
nx1a=(xf-xi)/hxa
nx1b=(xf-xi)/hxb
nx1a=nx1a+3-mod(nx1a,3)
nx1b=nx1b+3-mod(nx1b,3)
hx1a=(xf-xi)/dfloat(nx1a)
hx1b=(xf-xi)/dfloat(nx1b)

hya=h
hyb=h*dfloat(n)/dfloat(n+500)
ny1a=(yf-yi)/hya
ny1b=(yf-yi)/hyb
ny1a=ny1a+3-mod(ny1a,3)
ny1b=ny1b+3-mod(ny1b,3)
hy1a=(yf-yi)/dfloat(ny1a)
hy1b=(yf-yi)/dfloat(ny1b)
allocate(ga(0:nx1a,0:ny1a),gb(0:nx1b,0:ny1b),&
qa(0:max(nx1a,nx1b,ny1a,ny1b)),qb(0:max(nx1a,nx1b,ny1a,ny1b)))
if(ny1a.gt. 45000 .or.ny1b.gt. 45000 .or. nx1a.gt. 45000 .or.nx1b.gt. 45000) then
write(*,*) "failed to converge, estimated percentage error is",abs(pb-pa)/abs(pb)*100,"%"
exit
end if

do j=0,nx1a
do i=0,ny1a
ga(i,j)=f(x,y,z,xi+dfloat(i)*hx1a,yi+dfloat(j)*hy1a)
end do
end do

pa=0
do i=0,nx1a
qa(i)=0
do j=1,ny1a-1
if(mod(j,3).ne.0) then
qa(i)=qa(i)+3*ga(i,j)
elseif(mod(j,3).eq.0) then
qa(i)=qa(i)+2*ga(i,j)
end if
end do

qa(i)=(qa(i)+ga(i,0)+ga(i,ny1a))*3*hy1a/8
if(mod(i,3).ne.0 .and. i.gt.0 .and. i.lt.nx1a) then
pa=pa+3*qa(i)
elseif(mod(i,3).eq.0 .and. i.gt.0 .and. i.lt.nx1a) then
pa=pa+2*qa(i)
end if
end do

pa=(pa+qa(0)+qa(nx1a))*3*hx1a/8


do j=0,nx1b
do i=0,ny1b
gb(i,j)=f(x,y,z,xi+dfloat(i)*hx1b,yi+dfloat(j)*hy1b)
end do
end do

pb=0
do i=0,nx1b
qb(i)=0
do j=1,ny1b-1
if(mod(j,3).ne.0) then
qb(i)=qb(i)+3*gb(i,j)
elseif(mod(j,3).eq.0) then
qb(i)=qb(i)+2*gb(i,j)
end if
end do

qb(i)=(qb(i)+gb(i,0)+gb(i,ny1b))*3*hy1b/8
if(mod(i,3).ne.0 .and. i.gt.0 .and. i.lt.nx1b) then
pb=pb+3*qb(i)
elseif(mod(i,3).eq.0 .and. i.gt.0 .and. i.lt.nx1b) then
pb=pb+2*qb(i)
end if
end do

pb=(pb+qb(0)+qb(nx1b))*3*hx1b/8
n=n*2
deallocate(ga,gb,qa,qb)
end do

int5r2=pb
end function int5r2


double precision function int2a2(f,xi,xf,yi,yf,error)
double precision::pa,pb,f,hx,hy,hx1a,hx1b,hy1a,hy1b,xf,xi,yf,yi,h,error,hya,hyb,hx1,hy1,hxa,hxb
integer::i,j,nx1a,nx1b,ny1a,ny1b,n
integer,parameter::dp=selected_real_kind(8)
double precision,allocatable::ga(:,:),gb(:,:),qa(:),qb(:)

n=1000

pb=2*error
pa=0
do while(abs(pb-pa).gt.error)
h=max((yf-yi),(xf-xi))/dfloat(n)
hxa=h
hxb=h*dfloat(n)/dfloat(n+500)
nx1a=(xf-xi)/hxa
nx1b=(xf-xi)/hxb
nx1a=nx1a+3-mod(nx1a,3)
nx1b=nx1b+3-mod(nx1b,3)
hx1a=(xf-xi)/dfloat(nx1a)
hx1b=(xf-xi)/dfloat(nx1b)

hya=h
hyb=h*dfloat(n)/dfloat(n+500)
ny1a=(yf-yi)/hya
ny1b=(yf-yi)/hyb
ny1a=ny1a+3-mod(ny1a,3)
ny1b=ny1b+3-mod(ny1b,3)
hy1a=(yf-yi)/dfloat(ny1a)
hy1b=(yf-yi)/dfloat(ny1b)
allocate(ga(0:nx1a,0:ny1a),gb(0:nx1b,0:ny1b),&
qa(0:max(nx1a,nx1b,ny1a,ny1b)),qb(0:max(nx1a,nx1b,ny1a,ny1b)))
if(ny1a.gt. 45000 .or.ny1b.gt. 45000 .or. nx1a.gt. 45000 .or.nx1b.gt. 45000) then
write(*,*) "failed to converge, estimated percentage error is",abs(pb-pa)/abs(pb)*100,"%"
exit
end if

do j=0,nx1a
do i=0,ny1a
ga(i,j)=f(xi+dfloat(i)*hx1a,yi+dfloat(j)*hy1a)
end do
end do

pa=0
do i=0,nx1a
qa(i)=0
do j=1,ny1a-1
if(mod(j,3).ne.0) then
qa(i)=qa(i)+3*ga(i,j)
elseif(mod(j,3).eq.0) then
qa(i)=qa(i)+2*ga(i,j)
end if
end do

qa(i)=(qa(i)+ga(i,0)+ga(i,ny1a))*3*hy1a/8
if(mod(i,3).ne.0 .and. i.gt.0 .and. i.lt.nx1a) then
pa=pa+3*qa(i)
elseif(mod(i,3).eq.0 .and. i.gt.0 .and. i.lt.nx1a) then
pa=pa+2*qa(i)
end if
end do

pa=(pa+qa(0)+qa(nx1a))*3*hx1a/8


do j=0,nx1b
do i=0,ny1b
gb(i,j)=f(xi+dfloat(i)*hx1b,yi+dfloat(j)*hy1b)
end do
end do

pb=0
do i=0,nx1b
qb(i)=0
do j=1,ny1b-1
if(mod(j,3).ne.0) then
qb(i)=qb(i)+3*gb(i,j)
elseif(mod(j,3).eq.0) then
qb(i)=qb(i)+2*gb(i,j)
end if
end do

qb(i)=(qb(i)+gb(i,0)+gb(i,ny1b))*3*hy1b/8
if(mod(i,3).ne.0 .and. i.gt.0 .and. i.lt.nx1b) then
pb=pb+3*qb(i)
elseif(mod(i,3).eq.0 .and. i.gt.0 .and. i.lt.nx1b) then
pb=pb+2*qb(i)
end if
end do

pb=(pb+qb(0)+qb(nx1b))*3*hx1b/8
n=n*2
deallocate(ga,gb,qa,qb)
end do

int2a2=pb

end function int2a2

double precision function int3a2(f,x,xi,xf,yi,yf,error)
double precision::pa,pb,f,hx,hy,hx1a,hx1b,hy1a,hy1b,xf,xi,yf,yi,h,error,hya,hyb,hx1,hy1,hxa,hxb,x
integer::i,j,nx1a,nx1b,ny1a,ny1b,n
integer,parameter::dp=selected_real_kind(8)
double precision,allocatable::ga(:,:),gb(:,:),qa(:),qb(:)

n=1000

pb=2*error
pa=0
do while(abs(pb-pa).gt.error)
h=max((yf-yi),(xf-xi))/dfloat(n)
hxa=h
hxb=h*dfloat(n)/dfloat(n+500)
nx1a=(xf-xi)/hxa
nx1b=(xf-xi)/hxb
nx1a=nx1a+3-mod(nx1a,3)
nx1b=nx1b+3-mod(nx1b,3)
hx1a=(xf-xi)/dfloat(nx1a)
hx1b=(xf-xi)/dfloat(nx1b)

hya=h
hyb=h*dfloat(n)/dfloat(n+500)
ny1a=(yf-yi)/hya
ny1b=(yf-yi)/hyb
ny1a=ny1a+3-mod(ny1a,3)
ny1b=ny1b+3-mod(ny1b,3)
hy1a=(yf-yi)/dfloat(ny1a)
hy1b=(yf-yi)/dfloat(ny1b)
allocate(ga(0:nx1a,0:ny1a),gb(0:nx1b,0:ny1b),&
qa(0:max(nx1a,nx1b,ny1a,ny1b)),qb(0:max(nx1a,nx1b,ny1a,ny1b)))
if(ny1a.gt. 45000 .or.ny1b.gt. 45000 .or. nx1a.gt. 45000 .or.nx1b.gt. 45000) then
write(*,*) "failed to converge, estimated percentage error is",abs(pb-pa)/abs(pb)*100,"%"
exit
end if

do j=0,nx1a
do i=0,ny1a
ga(i,j)=f(x,xi+dfloat(i)*hx1a,yi+dfloat(j)*hy1a)
end do
end do

pa=0
do i=0,nx1a
qa(i)=0
do j=1,ny1a-1
if(mod(j,3).ne.0) then
qa(i)=qa(i)+3*ga(i,j)
elseif(mod(j,3).eq.0) then
qa(i)=qa(i)+2*ga(i,j)
end if
end do

qa(i)=(qa(i)+ga(i,0)+ga(i,ny1a))*3*hy1a/8
if(mod(i,3).ne.0 .and. i.gt.0 .and. i.lt.nx1a) then
pa=pa+3*qa(i)
elseif(mod(i,3).eq.0 .and. i.gt.0 .and. i.lt.nx1a) then
pa=pa+2*qa(i)
end if
end do

pa=(pa+qa(0)+qa(nx1a))*3*hx1a/8


do j=0,nx1b
do i=0,ny1b
gb(i,j)=f(x,xi+dfloat(i)*hx1b,yi+dfloat(j)*hy1b)
end do
end do

pb=0
do i=0,nx1b
qb(i)=0
do j=1,ny1b-1
if(mod(j,3).ne.0) then
qb(i)=qb(i)+3*gb(i,j)
elseif(mod(j,3).eq.0) then
qb(i)=qb(i)+2*gb(i,j)
end if
end do

qb(i)=(qb(i)+gb(i,0)+gb(i,ny1b))*3*hy1b/8
if(mod(i,3).ne.0 .and. i.gt.0 .and. i.lt.nx1b) then
pb=pb+3*qb(i)
elseif(mod(i,3).eq.0 .and. i.gt.0 .and. i.lt.nx1b) then
pb=pb+2*qb(i)
end if
end do

pb=(pb+qb(0)+qb(nx1b))*3*hx1b/8
n=n*2
deallocate(ga,gb,qa,qb)
end do

int3a2=pb
end function int3a2

double precision function int4a2(f,x,y,xi,xf,yi,yf,error)
double precision::pa,pb,f,hx,hy,hx1a,hx1b,hy1a,hy1b,xf,xi,yf,yi,h,error,hya,hyb,hx1,hy1,hxa,hxb,x,y
integer::i,j,nx1a,nx1b,ny1a,ny1b,n
integer,parameter::dp=selected_real_kind(8)
double precision,allocatable::ga(:,:),gb(:,:),qa(:),qb(:)

n=1000

pb=2*error
pa=0
do while(abs(pb-pa).gt.error)
h=max((yf-yi),(xf-xi))/dfloat(n)
hxa=h
hxb=h*dfloat(n)/dfloat(n+500)
nx1a=(xf-xi)/hxa
nx1b=(xf-xi)/hxb
nx1a=nx1a+3-mod(nx1a,3)
nx1b=nx1b+3-mod(nx1b,3)
hx1a=(xf-xi)/dfloat(nx1a)
hx1b=(xf-xi)/dfloat(nx1b)

hya=h
hyb=h*dfloat(n)/dfloat(n+500)
ny1a=(yf-yi)/hya
ny1b=(yf-yi)/hyb
ny1a=ny1a+3-mod(ny1a,3)
ny1b=ny1b+3-mod(ny1b,3)
hy1a=(yf-yi)/dfloat(ny1a)
hy1b=(yf-yi)/dfloat(ny1b)
allocate(ga(0:nx1a,0:ny1a),gb(0:nx1b,0:ny1b),&
qa(0:max(nx1a,nx1b,ny1a,ny1b)),qb(0:max(nx1a,nx1b,ny1a,ny1b)))
if(ny1a.gt. 45000 .or.ny1b.gt. 45000 .or. nx1a.gt. 45000 .or.nx1b.gt. 45000) then
write(*,*) "failed to converge, estimated percentage error is",abs(pb-pa)/abs(pb)*100,"%"
exit
end if

do j=0,nx1a
do i=0,ny1a
ga(i,j)=f(x,y,xi+dfloat(i)*hx1a,yi+dfloat(j)*hy1a)
end do
end do

pa=0
do i=0,nx1a
qa(i)=0
do j=1,ny1a-1
if(mod(j,3).ne.0) then
qa(i)=qa(i)+3*ga(i,j)
elseif(mod(j,3).eq.0) then
qa(i)=qa(i)+2*ga(i,j)
end if
end do

qa(i)=(qa(i)+ga(i,0)+ga(i,ny1a))*3*hy1a/8
if(mod(i,3).ne.0 .and. i.gt.0 .and. i.lt.nx1a) then
pa=pa+3*qa(i)
elseif(mod(i,3).eq.0 .and. i.gt.0 .and. i.lt.nx1a) then
pa=pa+2*qa(i)
end if
end do

pa=(pa+qa(0)+qa(nx1a))*3*hx1a/8


do j=0,nx1b
do i=0,ny1b
gb(i,j)=f(x,y,xi+dfloat(i)*hx1b,yi+dfloat(j)*hy1b)
end do
end do

pb=0
do i=0,nx1b
qb(i)=0
do j=1,ny1b-1
if(mod(j,3).ne.0) then
qb(i)=qb(i)+3*gb(i,j)
elseif(mod(j,3).eq.0) then
qb(i)=qb(i)+2*gb(i,j)
end if
end do

qb(i)=(qb(i)+gb(i,0)+gb(i,ny1b))*3*hy1b/8
if(mod(i,3).ne.0 .and. i.gt.0 .and. i.lt.nx1b) then
pb=pb+3*qb(i)
elseif(mod(i,3).eq.0 .and. i.gt.0 .and. i.lt.nx1b) then
pb=pb+2*qb(i)
end if
end do

pb=(pb+qb(0)+qb(nx1b))*3*hx1b/8
n=n*2
deallocate(ga,gb,qa,qb)
end do

int4a2=pb

end function int4a2

double precision function int5a2(f,x,y,z,xi,xf,yi,yf,error)
double precision::pa,pb,f,hx,hy,hx1a,hx1b,hy1a,hy1b,xf,xi,yf,yi,h,error,hya,hyb,hx1,hy1,hxa,hxb,x,y,z
integer::i,j,nx1a,nx1b,ny1a,ny1b,n
integer,parameter::dp=selected_real_kind(8)
double precision,allocatable::ga(:,:),gb(:,:),qa(:),qb(:)

n=1000

pb=2*error
pa=0
do while(abs(pb-pa).gt.error)
h=max((yf-yi),(xf-xi))/dfloat(n)
hxa=h
hxb=h*dfloat(n)/dfloat(n+100)
nx1a=(xf-xi)/hxa
nx1b=(xf-xi)/hxb
nx1a=nx1a+3-mod(nx1a,3)
nx1b=nx1b+3-mod(nx1b,3)
hx1a=(xf-xi)/dfloat(nx1a)
hx1b=(xf-xi)/dfloat(nx1b)

hya=h
hyb=h*dfloat(n)/dfloat(n+100)
ny1a=(yf-yi)/hya
ny1b=(yf-yi)/hyb
ny1a=ny1a+3-mod(ny1a,3)
ny1b=ny1b+3-mod(ny1b,3)
hy1a=(yf-yi)/dfloat(ny1a)
hy1b=(yf-yi)/dfloat(ny1b)
allocate(ga(0:nx1a,0:ny1a),gb(0:nx1b,0:ny1b),&
qa(0:max(nx1a,nx1b,ny1a,ny1b)),qb(0:max(nx1a,nx1b,ny1a,ny1b)))
if(ny1a.gt. 45000 .or.ny1b.gt. 45000 .or. nx1a.gt. 45000 .or.nx1b.gt. 45000) then
write(*,*) "failed to converge, estimated percentage error is",abs(pb-pa)/abs(pb)*100,"%"
exit
end if

do j=0,nx1a
do i=0,ny1a
ga(i,j)=f(x,y,z,xi+dfloat(i)*hx1a,yi+dfloat(j)*hy1a)
end do
end do

pa=0
do i=0,nx1a
qa(i)=0
do j=1,ny1a-1
if(mod(j,3).ne.0) then
qa(i)=qa(i)+3*ga(i,j)
elseif(mod(j,3).eq.0) then
qa(i)=qa(i)+2*ga(i,j)
end if
end do

qa(i)=(qa(i)+ga(i,0)+ga(i,ny1a))*3*hy1a/8
if(mod(i,3).ne.0 .and. i.gt.0 .and. i.lt.nx1a) then
pa=pa+3*qa(i)
elseif(mod(i,3).eq.0 .and. i.gt.0 .and. i.lt.nx1a) then
pa=pa+2*qa(i)
end if
end do

pa=(pa+qa(0)+qa(nx1a))*3*hx1a/8


do j=0,nx1b
do i=0,ny1b
gb(i,j)=f(x,y,z,xi+dfloat(i)*hx1b,yi+dfloat(j)*hy1b)
end do
end do

pb=0
do i=0,nx1b
qb(i)=0
do j=1,ny1b-1
if(mod(j,3).ne.0) then
qb(i)=qb(i)+3*gb(i,j)
elseif(mod(j,3).eq.0) then
qb(i)=qb(i)+2*gb(i,j)
end if
end do

qb(i)=(qb(i)+gb(i,0)+gb(i,ny1b))*3*hy1b/8
if(mod(i,3).ne.0 .and. i.gt.0 .and. i.lt.nx1b) then
pb=pb+3*qb(i)
elseif(mod(i,3).eq.0 .and. i.gt.0 .and. i.lt.nx1b) then
pb=pb+2*qb(i)
end if
end do

pb=(pb+qb(0)+qb(nx1b))*3*hx1b/8
n=n*2
deallocate(ga,gb,qa,qb)
end do

int5a2=pb
end function int5a2

real*8 function int33(fq,ax,bx,ay,by,az,bz,h)
real*8::ax,bx,ay,by,az,bz,h,x
real*8,external::fq
int33=int11(g,ax,bx,h)

contains
real*8 function g(x)
real*8 x


g=int32(fq,x,ay,by,az,bz,h)
end function g

end function int33


real*8 function int43(fq,x,ax,bx,ay,by,az,bz,h)
real*8::ax,bx,ay,by,az,bz,h,x
real*8,external::fq
int43=int21(g,x,ax,bx,h)

contains
real*8 function g(x,y)
real*8 x,y


g=int42(fq,x,y,ay,by,az,bz,h)
end function g

end function int43

real*8 function int53(fq,x,y,ax,bx,ay,by,az,bz,h)
real*8::ax,bx,ay,by,az,bz,h,x,y
real*8,external::fq
int53=int31(g,x,y,ax,bx,h)

contains
real*8 function g(x,y,z)
real*8 x,y,z


g=int52(fq,x,y,z,ay,by,az,bz,h)
end function g

end function int53

real*8 function int3a3(fq,ax,bx,ay,by,az,bz,error)
real*8::ax,bx,ay,by,az,bz,error,x1,x2,h
real*8,external::fq
x1=10
x2=5
h=0.5
do while(abs(x1-x2).gt.error)
x1=int33(fq,ax,bx,ay,by,az,bz,h)
x2=int33(fq,ax,bx,ay,by,az,bz,h/2)
h=h/5
end do
int3a3=x2



end function int3a3


real*8 function int4a3(fq,x,ax,bx,ay,by,az,bz,error)
real*8::ax,bx,ay,by,az,bz,error,x1,x2,h,x
real*8,external::fq
x1=10
x2=5
h=0.5
do while(abs(x1-x2).gt.error)
x1=int43(fq,x,ax,bx,ay,by,az,bz,h)
x2=int43(fq,x,ax,bx,ay,by,az,bz,h/2)
h=h/5
end do
int4a3=x2



end function int4a3




real*8 function int5a3(fq,x,y,ax,bx,ay,by,az,bz,error)
real*8::ax,bx,ay,by,az,bz,error,x1,x2,h,x,y
real*8,external::fq
x1=10
x2=5
h=0.5
do while(abs(x1-x2).gt.error)
x1=int53(fq,x,y,ax,bx,ay,by,az,bz,h)
x2=int53(fq,x,y,ax,bx,ay,by,az,bz,h/2)
h=h/5
end do
int5a3=x2

end function int5a3



real*8 function int3r3(fq,ax,bx,ay,by,az,bz,error)
real*8::ax,bx,ay,by,az,bz,error,x1,x2,h
real*8,external::fq
x1=10
x2=5
h=0.5
do while(abs(x1-x2)/abs(x2).gt.error)
x1=int33(fq,ax,bx,ay,by,az,bz,h)
x2=int33(fq,ax,bx,ay,by,az,bz,h/2)
h=h/5
end do
int3r3=x2
end function int3r3


real*8 function int4r3(fq,x,ax,bx,ay,by,az,bz,error)
real*8::ax,bx,ay,by,az,bz,error,x1,x2,h,x
real*8,external::fq
x1=10
x2=5
h=0.5
do while(abs(x1-x2)/abs(x2).gt.error)
x1=int43(fq,x,ax,bx,ay,by,az,bz,h)
x2=int43(fq,x,ax,bx,ay,by,az,bz,h/2)
h=h/5
end do
int4r3=x2

end function int4r3
real*8 function int5r3(fq,x,y,ax,bx,ay,by,az,bz,error)
real*8::ax,bx,ay,by,az,bz,error,x1,x2,h,x,y
real*8,external::fq
x1=10
x2=5
h=0.5
do while(abs(x1-x2)/abs(x2).gt.error)
x1=int53(fq,x,y,ax,bx,ay,by,az,bz,h)
x2=int53(fq,x,y,ax,bx,ay,by,az,bz,h/2)
h=h/5
end do
int5r3=x2

end function int5r3


!end intlib



!diflib
real*8 function d11(f,x,h)
real*8,external::f
real*8::x,h
d11=(-f(x+2*h)+8*f(x+h)-8*f(x-h)+f(x-2*h))/(12*h)
end function d11

real*8 function d12(f,x,h)
real*8,external::f
real*8::x,h
d12=(-d11(f,x+2*h,h)+8*d11(f,x+h,h)-8*d11(f,x-h,h)+d11(f,x-2*h,h))/(12*h)
end function d12


real*8 function d210(f,x,y,h)
real*8,external::f
real*8::x,h,y
d210=(-f(x+2*h,y)+8*f(x+h,y)-8*f(x-h,y)+f(x-2*h,y))/(12*h)
end function d210


real*8 function d220(f,x,y,h)
real*8,external::f
real*8::x,h,y
d220=(-d210(f,x+2*h,y,h)+8*d210(f,x+h,y,h)-8*d210(f,x-h,y,h)+d210(f,x-2*h,y,h))/(12*h)
end function d220

real*8 function d201(f,y,x,h)
real*8,external::f
real*8::x,h,y
d201=(-f(y,x+2*h)+8*f(y,x+h)-8*f(y,x-h)+f(y,x-2*h))/(12*h)
end function d201


real*8 function d202(f,y,x,h)
real*8,external::f
real*8::x,h,y
d202=(-d201(f,y,x+2*h,h)+8*d201(f,y,x+h,h)-8*d201(f,y,x-h,h)+d201(f,y,x-2*h,h))/(12*h)
end function d202


real*8 function d211(f,x,y,h)
real*8,external::f
real*8::x,h,y
d211=(-d201(f,x+2*h,y,h)+8*d201(f,x+h,y,h)-8*d201(f,x-h,y,h)+d201(f,x-2*h,y,h))/(12*h)
end function d211
!end diflib

!plotlib
subroutine plotf2(f,xi,xf,h,option)
real*8 xi,xf,h,x
real*8, external::f
character(len=*)::option


open(1,file="p.dat")
open(2,file="p.plt")

x=xi
do while(x.le.xf)
write(1,*) x,f(x)
x=x+h
end do

write(2,*) "set xrange[",xi,":",xf,"]"
write(2,*) "p 'p.dat' u 1:2 w ",option
write(2,*) "pause mouse close"

call execute_command_line('gnuplot p.plt')
close(1,status='delete')
close(2,status='delete')
end subroutine plotf2


subroutine plota2(x,y,option)
real*8::x(:),y(:)
integer l,u,i
character(len=*)::option
l=lbound(x(:),dim=1)
u=ubound(x(:),dim=1)
open(1,file="p.dat")
open(2,file="p.plt")
do i=l,u
write(1,*) x(i),y(i)
end do

write(2,*) "set xrange[",minval(x),":",maxval(x),"]"
write(2,*) "set yrange[",minval(y),":",maxval(y),"]"
write(2,*) "p 'p.dat' u 1:2 w ",option
write(2,*) "pause mouse close"

call execute_command_line('gnuplot p.plt')
close(1,status='delete')
close(2,status='delete')
end subroutine plota2





subroutine plotf3(f,xi,xf,yi,yf,h,option)
real*8 xi,xf,h,x,yi,yf,y
real*8, external::f
character(len=*)::option

open(1,file="p.dat")
open(2,file="p.plt")

x=xi
do while(x.le.xf)
y=yi
do while(y.le.yf)
write(1,*) x,y,f(x,y)
y=y+h
end do
x=x+h
end do

write(2,*) "set xrange[",xi,":",xf,"]"
write(2,*) "set yrange[",yi,":",yf,"]"
write(2,*) "sp 'p.dat' u 1:2:3 w ",option
write(2,*) "pause mouse close"

call execute_command_line('gnuplot p.plt')
close(1,status='delete')
close(2,status='delete')
end subroutine plotf3


subroutine plota3(x,y,z,option)
real*8::x(:),y(:),z(:,:)
integer l,u,i,j
character(len=*)::option
l=lbound(x(:),dim=1)
u=ubound(x(:),dim=1)
open(1,file="p.dat")
open(2,file="p.plt")

do i=l,u
do j=l,u
write(1,*) x(i),y(j),z(i,j)
end do
end do

write(2,*) "set xrange[",minval(x),":",maxval(x),"]"
write(2,*) "set yrange[",minval(y),":",maxval(y),"]"
write(2,*) "set zrange[",minval(z),":",maxval(z),"]"
write(2,*) "sp 'p.dat' u 1:2:3 w ",option
write(2,*) "pause mouse close"

call execute_command_line('gnuplot p.plt')
close(1,status='delete')
close(2,status='delete')
end subroutine plota3



subroutine denplotf2(f,xi,xf,yi,yf,h)
real*8 xi,xf,h,x,yi,yf,y
real*8, external::f


open(1,file="p.dat")
open(2,file="p.plt")

x=xi
do while(x.le.xf)
y=yi
do while(y.le.yf)
write(1,*) x,y,f(x,y)
y=y+h
end do
write(1,*)
x=x+h
end do

write(2,*) "set xrange[",xi,":",xf,"]"
write(2,*) "set yrange[",yi,":",yf,"]"
write(2,*) "set pm3d map"
write(2,*) "sp 'p.dat' u 1:2:3 w pm3d"
write(2,*) "pause mouse close"

call execute_command_line('gnuplot p.plt')
close(1,status='delete')
close(2,status='delete')
end subroutine denplotf2


subroutine denplota2(x,y,z)
real*8::x(:),y(:),z(:,:)
integer l,u,i,j
l=lbound(x(:),dim=1)
u=ubound(x(:),dim=1)
open(1,file="p.dat")
open(2,file="p.plt")

do i=l,u
do j=l,u
write(1,*) x(i),y(j),z(i,j)
end do
write(1,*)
end do

!write(2,*) "set xrange[",xi,":",xf,"]"
write(2,*) "set pm3d map"
write(2,*) "sp 'p.dat' u 1:2:3 w pm3d"
write(2,*) "pause mouse close"

call execute_command_line('gnuplot p.plt')
close(1,status='delete')
close(2,status='delete')
end subroutine denplota2



!datafile

subroutine plotd2(filename,a,b,option)
character(len=*)::filename,option
integer a,b
open(1,file='p.plt')
write(1,*) "p" ,"'",filename,"' u",a,":",b,"w ",option
write(1,*) "pause mouse close"
call execute_command_line('gnuplot p.plt')
close(1,status="delete")
end subroutine plotd2

subroutine plotd3(filename,a,b,c,option)
character(len=*)::filename,option
integer a,b,c
open(1,file='p.plt')
write(1,*) "sp" ,"'",filename,"' u",a,":",b,":",c,"w ",option
write(1,*) "pause mouse close"
call execute_command_line('gnuplot p.plt')
close(1,status="delete")
end subroutine plotd3

subroutine denplotd2(filename,a,b,c)
character(len=*)::filename
integer a,b,c
open(1,file='p.plt')
write(1,*) "set pm3d map"
write(1,*) "sp" ,"'",filename,"' u",a,":",b,":",c,"w pm3d"
write(1,*) "pause mouse close"
call execute_command_line('gnuplot p.plt')
close(1,status="delete")
end subroutine denplotd2


subroutine plot_trajectory2(filename,a1,b1,nl,slow)
character(len=*)::filename
integer a1,b1,i,n,nl,ncount
real*8 a(nl),b(nl),slow
open(1,file=filename)

do i=1,nl
read(1,*) a(i),b(i)
enddo

close(1)


open(3,file="temp_p.plt")



write(3,*) "set xrange[",minval(a),":",maxval(a),"]"
write(3,*) "set yrange[",minval(b),":",maxval(b),"]"

write(3,*) "do for [i=1:",nl,"] {"
write(3,*) "plot '",filename,"' every ::i::i w p pt 7"
write(3,*) "pause",slow
write(3,*) "}"


call execute_command_line("gnuplot -p temp_p.plt")

close(3,status='delete')


end subroutine plot_trajectory2


subroutine plot_trajectory3(filename,a1,b1,c1,nl,slow)
character(len=*)::filename
integer a1,b1,i,n,nl,ncount,c1
real*8 a(nl),b(nl),slow,dc(nl)
open(1,file=filename)

do i=1,nl
read(1,*) a(i),b(i),dc(i)
enddo

close(1)


open(3,file="temp_p.plt")



write(3,*) "set xrange[",minval(a),":",maxval(a),"]"
write(3,*) "set yrange[",minval(b),":",maxval(b),"]"
write(3,*) "set zrange[",minval(dc),":",maxval(dc),"]"
write(3,*) "do for [i=1:",nl,"] {"
write(3,*) "splot '",filename,"' every ::i::i w p pt 7"
write(3,*) "pause",slow
write(3,*) "}"


call execute_command_line("gnuplot -p temp_p.plt")

close(3,status='delete')


end subroutine plot_trajectory3

!end plotlib


!specfun
FUNCTION zeta(x) RESULT(fn_val)
INTEGER, PARAMETER  :: dp = SELECTED_REAL_KIND(12, 60)
REAL (dp), INTENT(IN)  :: x
REAL (dp)              :: fn_val

REAL (dp), PARAMETER :: delta = 1.0D-13, z1 = 1.0_dp, hf = z1/2, th = z1/3,  &
                        pi = 3.14159265358979324_dp, pih = pi/2, pi2 = 2*pi

REAL (dp), PARAMETER :: p1(0:8) = (/ 1.28716812148244639D+10,  &
  1.37539693203702511D+10, 5.10665591836440610D+09, 8.56147100243331486D+08, &
  7.48361812438023298D+07, 4.86010658546188251D+06, 2.73957499022140609D+05, &
  4.63171084318342712D+03, 5.78758100409666066D+01 /)
REAL (dp), PARAMETER :: q1(0:8) = (/ 2.57433624296484624D+10,  &
  5.93816564867959016D+09, 9.00633037326123344D+08, 8.04253663428328989D+07, &
  5.60971175954192006D+06, 2.24743120289913752D+05, 7.57457890934153756D+03, &
 -2.37383578137377262D+01, 1.0_dp /)

REAL (dp), PARAMETER :: p2(0:8) = (/ -6.88197293216348954D+06,  &
  7.48218916305315972D+06, -2.07584504810211014D+06, 3.55302557096214295D+05, &
 -4.06706449551854889D+04, 3.19804864027146911D+03, -1.69820937033722853D+02, &
  5.61485842394289048D+00, -8.93888705926154944D-02 /)
REAL (dp), PARAMETER :: q2(0:8) = (/ -1.29725624934891554D+09,  &
 -9.48715407579907817D+08, -1.05496193474005203D+08, 4.67774488211993048D+06, &
  3.12936040573813534D+06, 4.59581803839305070D+05, 3.88176109610396834D+04,  &
  1.92561544834491423D+03, 5.12578125000000000D+01 /)

REAL (dp), PARAMETER :: p3(0:9) = (/ 1.66156480515774676D-11,  &
 -4.68068827660654529D-09, 5.83519727319147047D-07, -4.17644012643145602D-05, &
  1.85468422843597959D-03, -5.11288800220490241D-02, 8.10450231751100353D-01, &
 -5.69951948768478923D+00, 0.0_dp, 0.0_dp /)
REAL (dp), PARAMETER :: q3(0:9) = (/ -6.99562633519191655D-10,  &
 -1.77757961895149257D-08, -9.82231825734078036D-07, -2.84927282759096488D-05, &
 -5.81727909388048094D-04, -1.15848749169766586D-02, -1.28149124051978196D-01, &
 -1.11913057349097709D+00, -7.67928761604628813D-01,  1.0_dp /)

REAL (dp), PARAMETER :: p4(0:8) = (/ 1.03144877188859712D-15,  &
 -5.12584613964688241D-13, 1.12948794194873548D-10, -1.44234665373130952D-08, &
  1.16824676984458098D-06, -6.14975167990314806D-05, 2.05594677988830328D-03, &
 -3.99339429394668869D-02, 3.45234976736178457D-01 /)
REAL (dp), PARAMETER :: q4(0:8) = (/ 5.93959417288419050D-11,  &
 -6.04755359079991806D-09, 3.64680208668388563D-07, -1.29456905568011812D-05, &
  3.20189498470229250D-04, -5.07801557099994077D-03, 5.49628907881587266D-02, &
 -3.24517611155972419D-01, 1.0_dp /)

REAL (dp)  :: alfa, ap, aq, b0, b1, b2, f, h, t, v
INTEGER    :: ix, j

v = x
f = 1.0_dp
IF (x /= 0 .AND. x < hf) THEN
  ix = x - delta
  IF (ABS(ix-x) <= delta) THEN
    IF (MOD(-ix,2) == 0) THEN
      h = 0.0_dp
      GO TO 70
    ELSE
      v = 1.0_dp - x
      f = 2 * (-z1) ** ((1-ix)/2) * dgamma(v) / pi2 ** v
    END IF
  ELSE
    v = 1.0_dp - x
    f = 2 * SIN(pih*x) * dgamma(v) / pi2 ** v
  END IF
END IF

IF (x == 0) THEN
  h = -3 * hf
ELSE IF (x == 1) THEN
  fn_val = 0.0_dp
  WRITE(*, *) 'Riemanns ZETA(X) HAS POLE AT X = 1'
  RETURN
ELSE IF (v <= 5) THEN
  ap = p1(8)
  aq = q1(8)
  DO  j = 7, 0, -1
    ap = p1(j) + v * ap
    aq = q1(j) + v * aq
  END DO
  h = ap / (aq*(v-1)) - 1
ELSE IF (v <= 11) THEN
  t = th * (v-8)
  alfa = t + t
  b1 = 0
  b2 = 0
  DO  j = 8, 0, -1
    b0 = p2(j) + alfa * b1 - b2
    b2 = b1
    b1 = b0
  END DO
  h = b0 - t * b2
  b1 = 0
  b2 = 0
  DO  j = 8, 0, -1
    b0 = q2(j) + alfa * b1 - b2
    b2 = b1
    b1 = b0
  END DO
  h = h / (b0 - t*b2)
ELSE IF (v <= 25) THEN
  t = 1 / v
  ap = p3(7)
  DO  j = 6, 0, -1
    ap = p3(j) + t * ap
  END DO
  aq = q3(9)
  DO  j = 8, 0, -1
    aq = q3(j) + t * aq
  END DO
  h = hf ** (v - t*ap/aq)
ELSE IF (v <= 55) THEN
  t = 1 / v
  ap = p4(8)
  aq = q4(8)
  DO  j = 7, 0, -1
    ap = p4(j) + t * ap
    aq = q4(j) + t * aq
  END DO
  h = hf ** (v-t*ap/aq)
ELSE IF (v <= 90) THEN
  h = hf ** v + th ** v
ELSE
  h = hf ** v
END IF
IF (x < 1) h = f * (1 + h)

70 IF (x > 1.0_dp) THEN
   fn_val = 1.0_dp + h
ELSE
   fn_val = h
END IF

RETURN
END FUNCTION zeta



real*8 function bose(p,x)
real*8 p,x,error,sum1
integer n,i
if(abs(x).gt.1) then
write(*,*) "error, absolute value of argument x should be less than 1"
stop
end if
if(p.lt.0) then
write(*,*) "error, absolute value of argument n should be less than 1"
stop
end if

error=10.0**(-4)
n=10
do while(int3a1(b,p,x,dfloat(n),dfloat(2*n),error/5).gt.error/5)
n=n*2
end do
sum1=0
do i=1,n
sum1=sum1+x**dfloat(i)/dfloat(i)**p
end do
bose=sum1


contains
real*8 function b(p,x,u)
real*8 x,p,u
b=x**(u)/u**(p)
end function b

end function bose

real*8 function dbose(p,x)
real*8 p,x,error,sum1
integer n,i
if(abs(x).gt.1) then
write(*,*) "error, absolute value of argument x should be less than 1"
stop
end if
if(p.lt.0) then
write(*,*) "error, absolute value of argument n should be less than 1"
stop
end if

error=10.0**(-8)
n=10
do while(int3a1(b,p,x,dfloat(n),dfloat(2*n),error/5).gt.error/5)
n=n*2
end do
sum1=0
do i=1,n
sum1=sum1+x**dfloat(i)/dfloat(i)**p
end do
dbose=sum1


contains
real*8 function b(p,x,u)
real*8 x,p,u
b=x**(u)/u**(p)
end function b

end function dbose

real*8 function fermi(p,x)
real*8 p,x,error,sum1,ra,rb
integer n,i
error=10.0**(-4)
if(x.lt.0 .or. p.lt.1) then
write(*,*) "error:n greater than 1 and x greater than zero required"
stop
end if

if(abs(x).lt.1) then
n=10
do while(x**(n)/n**(p).gt.error/5)
n=n*2
end do
!write(*,*) n
sum1=0
do i=1,n
if(mod(i,2).ne.0) then
sum1=sum1+x**(dfloat(i))/dfloat(i)**p
else
sum1=sum1-x**(dfloat(i))/dfloat(i)**p
end if
end do
fermi=sum1



else
rb=20.0
do while(int3a1(b,p,x,rb,rb*2,error/2).gt.error/2)
rb=rb*2
end do

ra=0.01
do while(x*ra**(p)/(p*(x+1)).gt.error/10)
ra=ra/2
end do


fermi=int3a1(b,p,x,ra,rb,error/2)/dgamma(p)
end if



contains
real*8 function b(p,x,u)
real*8 p,x,u
b=u**(p-1)/((exp(u)/x)+1)
end function b


end function fermi


real*8 function dfermi(p,x)
real*8 p,x,error,sum1,ra,rb
integer n,i
error=10.0**(-8)
if(x.lt.0 .or. p.lt.1) then
write(*,*) "error:n greater than 1 and x greater than zero required"
stop
end if

if(abs(x).lt.1) then
n=10
do while(x**(n)/n**(p).gt.error/5)
n=n*2
end do
!write(*,*) n
sum1=0
do i=1,n
if(mod(i,2).ne.0) then
sum1=sum1+x**(dfloat(i))/dfloat(i)**p
else
sum1=sum1-x**(dfloat(i))/dfloat(i)**p
end if
end do
dfermi=sum1



else
rb=20.0
do while(int3a1(b,p,x,rb,rb*2,error/2).gt.error/2)
rb=rb*2
end do

ra=0.01
do while(x*ra**(p)/(p*(x+1)).gt.error/10)
ra=ra/2
end do

!write(*,*) ra,rb,b(p,x,ra),b(p,x,rb)
dfermi=int3a1(b,p,x,ra,rb,error/2)/dgamma(p)
end if



contains
real*8 function b(p,x,u)
real*8 p,x,u
b=u**(p-1)/((exp(u)/x)+1)
end function b

end function dfermi


real*8 function legendre_p(n,x)
integer n,j
real*8 x
real*8,allocatable::p(:)

allocate(p(0:n))

p(0)=1
p(1)=x
do j=1,n-1
p(j+1)=((2*dfloat(j)+1)*x*p(j)-dfloat(j)*p(j-1))/(dfloat(j)+1)
end do
legendre_p=p(n)
deallocate(p)
end function legendre_p


real*8 function hermite(n,x)
integer n,j
real*8 x
real*8,allocatable::h(:)

allocate(h(0:n))

h(0)=1
h(1)=2*x
do j=1,n-1
h(j+1)=2*x*h(j)-2*dfloat(j)*h(j-1)
end do
hermite=h(n)
deallocate(h)
end function hermite


real*8 function sp_besselj(n,x)
integer j,n
real*8 x,error
error=10.0**(-8)

sp_besselj=int3a1(fq,dfloat(n),x,0.0_8,1.0_8,error)*(x/2)**(dfloat(n))/(dgamma(dfloat(n+1)))

contains
real*8 function fq(m,p,y)
real*8 p,y,m
fq=(1.0-y**2)**(m)*cos(p*y)
end function fq
end function sp_besselj

real*8 function sinc(x)
real*8 x
if(abs(x).lt.0.0001) then
sinc=1
else
sinc=dsin(x)/x
endif
end function sinc
!end specfun



!genlib
subroutine dinterp(xi,yi,x,y)
real*8::x(:),y(:)
real*8,allocatable::l(:)
integer:: n,n1,i,j
real*8:: xi,yi,aq,bq
!patch start
aq=minval(x)
bq=maxval(x)
if(xi.lt.aq .or. xi.gt.bq) then
write(*,*) "error, value lies outside range of interpolation"
stop
endif

!patch end
n=size(x)
allocate(l(n))
do i=1,n-1
if(xi.gt.x(i) .and. xi.lt.x(i+1)) then
n1=i+1
exit
endif
end do
!write(*,*) n1
!case 1
if(n1.lt.n-1 .and. n1.gt.1) then
do j=n1-1,n1+2
l(j)=1.0
do i=n1-1,n1+2
if(i.ne.j) then
l(j)=l(j)*(xi-x(i))/(x(j)-x(i))
endif
end do
l(j)=l(j)*y(j)
end do

yi=sum(l)
l=0
deallocate(l)

!case 2
elseif(n1.eq.1) then
do j=n1,n1+2
l(j)=1.0
do concurrent(i=n1:n1+2,i.ne.j)
l(j)=l(j)*(xi-x(i))/(x(n1)-x(i))
end do
l(j)=l(j)*y(j)
end do

yi=sum(l)
l=0
deallocate(l)

!case 3
elseif(n1.eq.n-1) then
do j=n1-1,n1+1
l(j)=1.0
do concurrent(i=n1-1:n1+1,i.ne.j)
l(j)=l(j)*(xi-x(i))/(x(n1)-x(i))
end do
l(j)=l(j)*y(j)
end do

yi=sum(l)
l=0
deallocate(l)
endif

end subroutine dinterp


real*8 function fun_inverse(f,x,xi,xf,h2)

real*8 x,x1,h2,h,xcount,xi,yi,xf,h1
integer n,m,i
real*8,external::f
real*8,allocatable::xdata(:),ydata(:),xd(:),yd(:)

x1=xi
h1=0.1

n=1
xcount=xi
do while(xcount.lt.xf)
h=h2
if(abs((f(xcount+h1)-f(xcount-h1))/(2*h1)) .lt. 1.0) then
xcount=xcount+h
n=n+1
else
h=h/abs((f(xcount+h1)-f(xcount-h1))/(2*h1))
xcount=xcount+h
n=n+1
endif
enddo
allocate(xdata(n),ydata(n),xd(n),yd(n))
!write(*,*) n
n=1
xdata(1)=xi
ydata(1)=f(xi)
xcount=xi
do while(xcount.lt.xf)
h=h2
if(abs((f(xcount+h1)-f(xcount-h1))/(2*h1)) .lt. 1.0) then
xcount=xcount+h
xdata(n+1)=xcount
ydata(n+1)=f(xcount)
n=n+1
else
h=h/abs((f(xcount+h1)-f(xcount-h1))/(2*h1))
xcount=xcount+h
xdata(n+1)=xcount
ydata(n+1)=f(xcount)
n=n+1
endif
!write(*,*) x,ydata(n),xdata(n)
enddo
!write(*,*) xdata(n),ydata(n)

do i=1,n-1
if(ydata(2).gt.ydata(1)) then
if(ydata(i+1).lt.ydata(i)) then
write(*,*) "error, original function is not monotonic in the given range"
stop
endif
endif
if(ydata(2).lt.ydata(1)) then
if(ydata(i+1).gt.ydata(i)) then
write(*,*) "error, original function is not monotonic in the given range"
stop
endif
endif
enddo
if(ydata(2).lt.ydata(1)) then
xd=xdata
yd=ydata
do i=1,n
xdata(i)=xd(n-i+1)
ydata(i)=yd(n-i+1)
end do
endif
if(x.lt.minval(ydata) .or. x.gt.maxval(ydata)) then
write(*,*) "error, point is outside range"
stop
endif
call dinterp(x,yi,ydata,xdata)
fun_inverse=yi
xdata=0
ydata=0
deallocate(xdata,ydata,xd,yd)
!write(*,*) ydata(n-5),n
end function fun_inverse

recursive subroutine lsort(a)
  real*8 :: a(:)
  real*8 x, t
  integer :: first = 1, last
  integer i, j

  last = size(a, 1)
  x = a( (first+last) / 2 )
  i = first
  j = last
  
  do
     do while (a(i) < x)
        i=i+1
     end do
     do while (x < a(j))
        j=j-1
     end do
     if (i >= j) exit
     t = a(i);  a(i) = a(j);  a(j) = t
     i=i+1
     j=j-1
  end do
  
  if (first < i - 1) call lsort(a(first : i - 1))
  if (j + 1 < last)  call lsort(a(j + 1 : last))
end subroutine lsort


recursive subroutine hsort(a)
  real*8 :: a(:)
  real*8 x, t
  integer :: first = 1, last
  integer i, j

  last = size(a, 1)
  x = a( (first+last) / 2 )
  i = first
  j = last
  
  do
     do while (a(i) > x)
        i=i+1
     end do
     do while (x > a(j))
        j=j-1
     end do
     if (i >= j) exit
     t = a(i);  a(i) = a(j);  a(j) = t
     i=i+1
     j=j-1
  end do
  
  if (first < i - 1) call hsort(a(first : i - 1))
  if (j + 1 < last)  call hsort(a(j + 1 : last))
end subroutine hsort


function linspace(xi,xf,n)
real*8::linspace(n),xi,xf,h,a(n)
integer n,i
h=(xf-xi)/n
do i=1,n
a(i)=xi+dfloat(i)*h
end do
linspace=a
end function linspace

real*8 function trace(a)
real*8,intent(in)::a(:,:)
integer::i,n1,n2
real*8 a1
n1=size(a,dim=1)
n2=size(a,dim=2)

if(n1.ne.n2) then
write(*,*) "not square matrix"
stop
endif
a1=0
do i=1,n1
a1=a1+a(i,i)
enddo
trace=a1
end function trace

subroutine normalise(a)
real*8::a(:)
a=a/sqrt(sum(a(:)**2))
end subroutine normalise

subroutine swap_v(a,b)
real*8 a,b,a1,b1
b1=b
a1=a
a=b1
b=a1
end subroutine swap_v

subroutine swap_a(a,b)
real*8 a(:),b(:)
real*8,allocatable::a1(:),b1(:)
integer na,nb
na=size(a)
nb=size(b)
if(na.ne.nb)  then
write(*,*) "arrays of different size, error"
stop
endif
allocate(a1(na),b1(nb))
b1=b
a1=a
a=b1
b=a1
deallocate(a1,b1)
end subroutine swap_a



subroutine brent (f, x, x1, abserr, kmax)

  real ( kind = 8 ) abserr
  real ( kind = 8 ) d
  real ( kind = 8 ) dx
  real ( kind = 8 ) e
  real ( kind = 8 ) em
  real ( kind = 8 ), external :: f
  real ( kind = 8 ) fx
  real ( kind = 8 ) fx1
  real ( kind = 8 ) fx2
  integer ( kind = 4 ) ierror
  integer ( kind = 4 ) k
  integer ( kind = 4 ) kmax
  real ( kind = 8 ) p
  real ( kind = 8 ) q
  real ( kind = 8 ) r
  real ( kind = 8 ) s
  real ( kind = 8 ) t
  real ( kind = 8 ) tol
  real ( kind = 8 ) x
  real ( kind = 8 ) x1
  real ( kind = 8 ) x2
!
!  Initialization.
!
  ierror = 0
  k = 0
  fx = f ( x )
  fx1 = f ( x1 )
  x2 = x1
  fx2 = fx1
!
!  Iteration loop:
!

if(f(x)*f(x1).gt.0) then
write(*,*) "error, function does not have different sign in the endpoints"
stop
endif

  do

    k = k + 1

    if ( kmax < k ) then
      ierror = 2
      return
    end if

    if ( 0.0D+00 < fx1 .eqv. 0.0D+00 < fx2 ) then
      x2 = x
      fx2 = fx
      d = x1 - x
      e = x1 - x
    end if

    if ( abs ( fx2 ) < abs ( fx1 ) ) then
      t = x1
      x1 = x2
      x2 = t
      t = fx1
      fx1 = fx2
      fx2 = t
    end if

    tol = 2.0D+00 * epsilon ( 1.0D+00 ) * abs ( x1 ) + abserr
    em = 0.5D+00 * ( x2 - x1 )

    if ( abs ( em ) <= tol .or. fx1 == 0.0D+00 ) then
      x = x1
      return
    end if

    if ( abs ( e ) < tol .or. abs ( fx ) <= abs ( fx1 ) ) then

      d = em
      e = em

    else

      s = fx1 / fx

      if ( x == x2 ) then
        p = 2.0D+00 * em * s
        q = 1.0D+00 - s
      else
        q = fx / fx2
        r = fx1 / fx2
        p = s * ( 2.0D+00 * em * q * ( q - r ) - ( x1 - x ) * ( r - 1.0D+00 ) )
        q = ( q - 1.0D+00 ) * ( r - 1.0D+00 ) * ( s - 1.0D+00 )
      end if

      if ( 0 < p ) then
        q = - q
      else
        p = - p
      end if

      s = e
      e = d

      if ( 2.0D+00 * p < 3.0D+00 * em * q - abs ( tol * q ) .or. &
        p < abs ( 0.5D+00 * s * q ) ) then
        d = p / q
      else
        d = em
        e = em
      end if

    end if
!
!  Set the increment.
!
    if ( tol < abs ( d ) ) then
      dx = + d
    else if ( 0.0D+00 < em ) then
      dx = + tol
    else
      dx = - tol
    end if
!
!  Remember current data for next step.
!
    x = x1
    fx = fx1
!
!  Update the iterate and function values.
!
    x1 = x1 + dx
    fx1 = f ( x1 )
  end do

  return
end





subroutine busy(time)
real*8 time,s1,s2,p

call cpu_time(s1)
s2=-10
do while(s2.lt.time)
p=int1a1(f,100.0_8,120.0_8,0.00001_8)
call cpu_time(s2)
end do

contains
real*8 function f(x)
real*8 x
f=sin(x**2)
end function f
end subroutine busy



subroutine append(a,b)
real*8,intent(in)::b(:)
real*8,allocatable,intent(inout)::a(:)
real*8,allocatable::aux(:)
integer::na,nb,i
logical::xx
nb=size(b)

xx=allocated(a)

	if(xx.eqv..false.) then
		allocate(a(nb))
		a=b
	else
		na=size(a)
		allocate(aux(na))
		aux=a
		deallocate(a)
		allocate(a(na+nb))
			do i=1,na
			a(i)=aux(i)
			enddo
			do i=na+1,na+nb
			a(i)=b(i-na)
			enddo
			deallocate(aux)
	endif

end subroutine append
!end genlib

!odelib
subroutine ode1(f,x0,xf,y0,h,filename)
real*8 x0,y0,xf,h,x,y,k4,k1,k2,k3
real*8,external::f
character(len=*)::filename
open(1,file=filename)
x=x0
y=y0

do while(x.le.xf)
write(1,*) x,y
k1=f(x,y)
k2=f(x+h/2,y+h*k1/2)
k3=f(x+h/2,y+h*k2/2)
k4=f(x+h,y+h*k3)
y=y+(k1+2*k2+2*k3+k4)*h/6
x=x+h
end do
close(1)
end subroutine ode1



subroutine in_ode2(f,xi,xf,yi,dydx0,h,filename)
real*8 y0,y1,y2,y3,xi,xf,dydx0,h,p0,p1,p2,p3,f0,f1,f2,f3,p,x,y,yi
real*8,external::f
character(len=*)::filename
x=xi
y=yi
p=dydx0
open(1,file=filename)

do while(x.le.xf)
write(1,*) x,y,p
y0=y
p0=p
f0=f(x,y0,p0)
y1=y0+h*p0/2
p1=p0+h*f0/2
f1=f(x+h/2,y1,p1)
y2=y0+h*p1/2
p2=p0+h*f1/2
f2=f(x+h/2,y2,p2)
y3=y0+h*p2
p3=p0+h*f2
f3=f(x+h,y3,p3)

y=y+h*(p0+2*p1+2*p2+p3)/6
p=p+h*(f0+2*f1+2*f2+f3)/6
x=x+h
enddo
close(1)
end subroutine in_ode2

subroutine bvp_ode2(f,xi,xf,yi,yf,h,filename)
real*8,intent(in):: xi,xf,yi,yf,h
real*8 r1,r2,er
real*8,external::f
character(len=*)::filename

er=10.0**(-8)


r1=0
r2=1
do while(rootfqr(r1)*rootfqr(r2).gt.0.0)
r1=500000*(rand()-0.5)
r2=500000*(rand()-0.5)
end do
!write(*,*) rootfqr(r1),rootfqr(r2)

call brent(rootfqr,r1,r2,er,1000000)

call in_ode2(f,xi,xf,yi,r1,h,filename)


contains
real*8 function rootfqr(p12)
real*8,intent(in)::p12
real*8::x,y,y0,y1,y2,y3,p0,p1,p2,p3,f0,f1,f2,f3,p
!WRITE(*,*) xi,yi,h
p=p12
x=xi
y=yi
!WRITE(*,*) y
do while(x.le.xf)
!write(1,*) x,y,p
y0=y
p0=p
f0=f(x,y0,p0)
y1=y0+h*p0/2
p1=p0+h*f0/2
f1=f(x+h/2,y1,p1)
y2=y0+h*p1/2
p2=p0+h*f1/2
f2=f(x+h/2,y2,p2)
y3=y0+h*p2
p3=p0+h*f2
f3=f(x+h,y3,p3)

y=y+h*(p0+2*p1+2*p2+p3)/6
p=p+h*(f0+2*f1+2*f2+f3)/6
x=x+h
enddo
rootfqr=y-yf
end function rootfqr


end subroutine bvp_ode2

!end odelib

!statlib
real*8 function mean_a(x)
real*8::x(:)
integer n
n=size(x)
mean_a=sum(x)/n
end function mean_a

real*8 function sd_a(x)
real*8::x(:)
real*8,allocatable::y(:)
integer n
n=size(x)
allocate(y(n))
y(:)=x(:)**2
sd_a=sqrt(sum(y)/n-(sum(x))**2/n**2)
deallocate(y)
end function sd_a

real*8 function mean_f(f)
real*8 r,sda1,sda2,error
real*8,external::f
r=10
error=10.0**(-8.0)
sda1=1
sda2=2
do while(abs(sda1-sda2).gt.10.0**(-8))
sda1=int1a1(f1,-r,r,error)
sda2=int1a1(f1,-2*r,2*r,error)
r=r*2
end do
mean_f=sda2

contains
real*8 function f1(x)
real*8 x
f1=x*f(x)
end function f1
end function mean_f



real*8 function sd_f(f)
real*8 r,sda1,sda2,error
real*8,external::f
r=10
error=10.0**(-8.0)
sda1=1
sda2=2
do while(abs(sda1-sda2).gt.10.0**(-8))
sda1=int1a1(f1,-r,r,error)
sda2=int1a1(f1,-2*r,2*r,error)
r=r*2
end do
sd_f=(sda2-mean_f(f)**2)**(0.5)

contains
real*8 function f1(x)
real*8 x
f1=x**2*f(x)
end function f1
end function sd_f

real*8 function nrand()
real*8 r1,r2,f1

call random_number(r1)
call random_number(r2)
f1=sqrt(-2*log(r1))*cos(2*3.1415926535*r2)
nrand=f1
end function nrand

real*8 function rand()
real*8 r
call random_number(r)
rand=r
end function rand


subroutine aux_rand_func(f,a,b,x_array,prob_cum_array)
real*8,intent(in)::a,b
real*8,allocatable,intent(inout)::prob_cum_array(:),x_array(:)
real*8,external::f
real*8 xi,h,slope,slope1,h2,a1,a2
integer i,n

allocate(prob_cum_array(1000),x_array(1000))
xi=a+0.001 !patch
i=1
do while(i.le.1000)
	do while(xi.lt.b)
		if(prob_cum(f,a,b,xi).gt.0.001*dfloat(i)) then
			prob_cum_array(i)=prob_cum(f,a,b,xi)
!write(*,*) prob_cum_array(i)
			x_array(i)=xi
			exit
		endif
	xi=xi+0.001
	enddo
xi=x_array(i)
i=i+1

!print*, x_array(i)
enddo

contains

real*8 function prob_cum(f,a,b,x)
real*8,intent(in)::x,a,b
real*8,external::f
real*8::norm,tolerance
tolerance=0.1


norm=int11(f,a,b,tolerance)

prob_cum=int11(f,a,x,tolerance)/norm
end function prob_cum
end subroutine aux_rand_func

subroutine rand_func(x_array,dist_cum_array,random)
real*8 aux_p(4),aux_x(4),l(4),a,b,start,finish,r,auxpj,auxpi,lj
real*8,intent(in)::x_array(:),dist_cum_array(:)
real*8,intent(out)::random
integer i,num,j1
integer::n,j
n=size(x_array)

call random_number(r)

num=floor(1000*r)+1

if(num.gt.n-2) then
do i=1,4
aux_p(i)=dist_cum_array(num-4+i)
aux_x(i)=x_array(num-4+i)
enddo
!write(*,*) "flag right"
elseif(num.lt.2) then
do i=1,4
aux_p(i)=dist_cum_array(num+i)
aux_x(i)=x_array(num+i)
enddo
!write(*,*) "flag right"
else

do i=1,4
aux_p(i)=dist_cum_array(num-2+i)
aux_x(i)=x_array(num-2+i)
enddo
endif


do j=1,4
lj=1.0
auxpj=aux_p(j)
do i=1,4
if(i.ne.j) then
lj=lj*(r-aux_p(i))/(auxpj-aux_p(i))
endif
end do
l(j)=lj*aux_x(j)
end do
random=sum(l)
end subroutine rand_func

subroutine prob_dist1(xi,x,prob,div)
real*8::xi(:),x(div),prob(div),h,xj,xj1
integer::i,n,m,div,j

h=(maxval(xi)-minval(xi))/dfloat(div)

do i=1,div
x(i)=minval(xi)+dfloat(i-1)*h
enddo
n=size(xi)

do j=1,div-1
xj=x(j)
xj1=x(j+1)
m=0
do i=1,n
if(xi(i).gt.xj .and. xi(i).lt.xj1) then
m=m+1
endif
enddo
prob(j)=dfloat(m)/dfloat(n)
enddo
prob(div)=0
prob(div)=1-sum(prob)

end subroutine prob_dist1



!end statlib

!linear algebra


subroutine gen_eig(a,b,dc)
real*8 a(:,:)
complex*8::b(:,:),dc(:)
integer i,n,na1,na2,nb1,nb2,nc,size1,size2,size3,size4,j
logical::lg1,lg2
logical,parameter::Fa=.false.
character(len=1)::crap

na1=size(a,dim=1)
na2=size(a,dim=2)
nb1=size(b,dim=1)
nb2=size(b,dim=2)
nc=size(dc)
n=nc
if (na1.ne.na2 .or. nb1.ne.nb2 .or. na1.ne.nb2 .or. na1.ne.nc .or. nc.ne.n) then
write(*,*) "error, dimension mismatch"
stop
endif
open(1,file="temp_mat.dat")
write(*,*) "writing to datafile started"
do i=1,n
write(1,*) (/(a(i,j),j=1,n)/)
enddo
write(*,*) "writing to datafile completed"
close(1)
open(2,file="temp_oct.m")

write(2,*) 'a=dlmread("temp_mat.dat");'


write(2,*) '[V,l]=eig(a,"vector");'

write(2,*) 'save("temp_a.mat","V");'
write(2,*) 'save("temp_b.mat","l");'
close(2)


call execute_command_line("octave-cli temp_oct.m")
!write(*,*) "aa"
lg1=Fa
do while(lg1.eqv.Fa)
call busy(0.1_8)
inquire(file="temp_a.mat",exist=lg1)
!write(*,*) lg1
end do
!write(*,*) "aa"
lg2=Fa
do while(lg2.eqv.Fa)
call busy(0.1_8)
inquire(file="temp_a.mat",exist=lg2)
end do
!write(*,*) "aa"
size1=0
size2=1
do while(size1.ne.size2)
inquire(file="temp_a.mat",size=size1)
call busy(0.1_8)
inquire(file="temp_a.mat",size=size2)
enddo
!write(*,*) "aa"
size3=0
size4=1
do while(size3.ne.size4)
inquire(file="temp_a.mat",size=size3)
call busy(0.1_8)
inquire(file="temp_a.mat",size=size4)
enddo
!write(*,*) "aa"

open(3,file="temp_a.mat")
do i=1,5
read(3,*) crap
end do
do i=1,n
read(3,*) (b(i,j),j=1,n)
end do

open(4,file="temp_b.mat")
do i=1,5
read(4,*) crap
end do
do i=1,n
read(4,*) dc(i)
end do

close(3)
close(4)

end subroutine gen_eig

subroutine sym_eig(a,b,dc)
real*8 a(:,:),b(:,:),dc(:)
integer i,n,na1,na2,nb1,nb2,nc,size1,size2,size3,size4,j
logical::lg1,lg2
logical,parameter::Fa=.false.

na1=size(a,dim=1)
na2=size(a,dim=2)
nb1=size(b,dim=1)
nb2=size(b,dim=2)
nc=size(dc)
n=nc
if (na1.ne.na2 .or. nb1.ne.nb2 .or. na1.ne.nb2 .or. na1.ne.nc .or. nc.ne.n) then
write(*,*) "error, dimension mismatch"
stop
endif
open(1,file="temp_mat.dat")
write(*,*) "writing to datafile started"
do i=1,n
write(1,*) (/(a(i,j),j=1,n)/)
enddo
write(*,*) "writing to datafile completed"
close(1)
open(2,file="temp_oct.m")

write(2,*) 'a=dlmread("temp_mat.dat");'


write(2,*) '[V,l]=eig(a,"vector");'
write(2,*) 'save ("-ascii","temp_a.mat","V");'
write(2,*) 'save ("-ascii","temp_b.mat","l");'
close(2)


call execute_command_line("octave-cli temp_oct.m")
!write(*,*) "aa"
lg1=Fa
do while(lg1.eqv.Fa)
call busy(0.1_8)
inquire(file="temp_a.mat",exist=lg1)
!write(*,*) lg1
end do
!write(*,*) "aa"
lg2=Fa
do while(lg2.eqv.Fa)
call busy(0.1_8)
inquire(file="temp_a.mat",exist=lg2)
end do
!write(*,*) "aa"
size1=0
size2=1
do while(size1.ne.size2)
inquire(file="temp_a.mat",size=size1)
call busy(0.1_8)
inquire(file="temp_a.mat",size=size2)
enddo
!write(*,*) "aa"
size3=0
size4=1
do while(size3.ne.size4)
inquire(file="temp_a.mat",size=size3)
call busy(0.1_8)
inquire(file="temp_a.mat",size=size4)
enddo
!write(*,*) "aa"

open(3,file="temp_a.mat")
do i=1,n
read(3,*) (b(i,j),j=1,n)
end do

open(4,file="temp_b.mat")
do i=1,n
read(4,*) dc(i)
end do

close(3)
close(4)


end subroutine sym_eig


subroutine gen_eigs(r,dc,dat,dims,eig,vec,nout,string)
complex*8 eig(nout),vec(dims,nout)
real*8 dat(:)
integer r(:),dc(:)
integer i,n,na1,na2,nb1,nb2,nc,size1,size2,size3,size4,j,nout,dims
logical::lg1,lg2
logical,parameter::Fa=.false.
character(len=2)::string,crap

na1=size(r)
na2=size(dc)
nb1=size(dat)
nc=size(dc)
n=size(dat)
if (na1.ne.na2 .or. na1.ne.nb1 .or. na1.ne.nc) then
write(*,*) "error, dimension mismatch"
stop
endif
open(1,file="temp_mat.dat")
write(*,*) "writing to datafile started"
do i=1,n
write(1,*) r(i),dc(i),dat(i)
enddo
write(*,*) "writing to datafile completed"
close(1)
open(2,file="temp_oct.m")

write(2,*) 'a=dlmread("temp_mat.dat");'


write(2,*) "for i=1:size(a,dim=1);"
write(2,*)"r(i)=a(i,1);"
write(2,*)"c(i)=a(i,2);"
write(2,*)"dat(i)=a(i,3);"
write(2,*)"endfor;"

write(2,*) 'au=sparse(r,c,dat);'


write(2,*) '[V,l]=eigs(au,',nout,",'",string,"'",');'


write(2,*) 'for i=1:size(l);'
write(2,*) 'l1(i)=l(i,i);'
write(2,*) 'endfor;'
write(2,*) "l1=l1';"
write(2,*) 'save("temp_a.mat","V");'
write(2,*) 'save("temp_b.mat","l1");'
close(2)


call execute_command_line("octave-cli temp_oct.m")
!write(*,*) "aa"
lg1=Fa
do while(lg1.eqv.Fa)
call busy(0.1_8)
inquire(file="temp_a.mat",exist=lg1)
!write(*,*) lg1
end do
!write(*,*) "aa"
lg2=Fa
do while(lg2.eqv.Fa)
call busy(0.1_8)
inquire(file="temp_a.mat",exist=lg2)
end do
!write(*,*) "aa"
size1=0
size2=1
do while(size1.ne.size2)
inquire(file="temp_a.mat",size=size1)
call busy(0.1_8)
inquire(file="temp_a.mat",size=size2)
enddo
!write(*,*) "aa"
size3=0
size4=1
do while(size3.ne.size4)
inquire(file="temp_a.mat",size=size3)
call busy(0.1_8)
inquire(file="temp_a.mat",size=size4)
enddo
!write(*,*) "aa"

open(3,file="temp_a.mat")
do i=1,5
read(3,*) crap
end do
do i=1,dims
read(3,*) (vec(i,j),j=1,nout)
end do

open(4,file="temp_b.mat")
do i=1,5
read(4,*) crap
end do
do i=1,nout
read(4,*) eig(i)
end do

close(3)
close(4)


end subroutine gen_eigs

subroutine sym_eigs(r,dc,dat,dims,eig,vec,nout,string)
real*8 dat(:),eig(nout),vec(dims,nout)
integer r(:),dc(:)
integer i,n,na1,na2,nb1,nb2,nc,size1,size2,size3,size4,j,nout,dims
logical::lg1,lg2
logical,parameter::Fa=.false.
character(len=2)::string

na1=size(r)
na2=size(dc)
nb1=size(dat)
nc=size(dc)
n=size(dat)
if (na1.ne.na2 .or. na1.ne.nb1 .or. na1.ne.nc) then
write(*,*) "error, dimension mismatch"
stop
endif
open(1,file="temp_mat.dat")
write(*,*) "writing to datafile started"
do i=1,n
write(1,*) r(i),dc(i),dat(i)
enddo
write(*,*) "writing to datafile completed"
close(1)
open(2,file="temp_oct.m")

write(2,*) 'a=dlmread("temp_mat.dat");'
write(2,*) 'au=spconvert(a)'

write(2,*) '[V,l]=eigs(au,',nout,",'",string,"'",');'
write(2,*) 'for i=1:size(l);'
write(2,*) 'l1(i)=l(i,i);'
write(2,*) 'endfor;'
write(2,*) "l1=l1'"
write(2,*) 'save ("-ascii","temp_a.mat","V");'
write(2,*) 'save ("-ascii","temp_b.mat","l1");'
close(2)


call execute_command_line("octave-cli temp_oct.m")
!write(*,*) "aa"
lg1=Fa
do while(lg1.eqv.Fa)
call busy(0.1_8)
inquire(file="temp_a.mat",exist=lg1)
!write(*,*) lg1
end do
!write(*,*) "aa"
lg2=Fa
do while(lg2.eqv.Fa)
call busy(0.1_8)
inquire(file="temp_a.mat",exist=lg2)
end do
!write(*,*) "aa"
size1=0
size2=1
do while(size1.ne.size2)
inquire(file="temp_a.mat",size=size1)
call busy(0.1_8)
inquire(file="temp_a.mat",size=size2)
enddo
!write(*,*) "aa"
size3=0
size4=1
do while(size3.ne.size4)
inquire(file="temp_a.mat",size=size3)
call busy(0.1_8)
inquire(file="temp_a.mat",size=size4)
enddo
!write(*,*) "aa"

open(3,file="temp_a.mat")
do i=1,dims
read(3,*) (vec(i,j),j=1,nout)
end do

open(4,file="temp_b.mat")
do i=1,nout
read(4,*) eig(i)
end do

close(3)
close(4)


end subroutine sym_eigs

  subroutine mat_inverse(a,c)

implicit none 
integer n
double precision a(:,:), c(:,:)
double precision,allocatable::L(:,:),U(:,:),b(:),d(:),x(:),aux(:,:)
double precision coeff
integer i, j, k
n=size(a,dim=1)
allocate( L(n,n), U(n,n), b(n), d(n), x(n),aux(n,n))
L=0.0
U=0.0
b=0.0
aux=a
do k=1, n-1
   do i=k+1,n
      coeff=aux(i,k)/aux(k,k)
      L(i,k) = coeff
      do j=k+1,n
         aux(i,j) = aux(i,j)-coeff*aux(k,j)
      end do
   end do
end do

do i=1,n
  L(i,i) = 1.0
end do
do j=1,n
  do i=1,j
    U(i,j) = aux(i,j)
  end do
end do
do k=1,n
  b(k)=1.0
  d(1) = b(1)
  do i=2,n
    d(i)=b(i)
    do j=1,i-1
      d(i) = d(i) - L(i,j)*d(j)
    end do
  end do
  x(n)=d(n)/U(n,n)
  do i = n-1,1,-1
    x(i) = d(i)
    do j=n,i+1,-1
      x(i)=x(i)-U(i,j)*x(j)
    end do
    x(i) = x(i)/u(i,i)
  end do
  do i=1,n
    c(i,k) = x(i)
  end do
  b(k)=0.0
end do
deallocate( L, U, b, d, aux ,x)
end subroutine mat_inverse


REAL FUNCTION Det(matrix)
    IMPLICIT NONE
    REAL*8, DIMENSION(:,:) :: matrix
    INTEGER :: n
    REAL :: m, temp
    INTEGER :: i, j, k, l
    LOGICAL :: DetExists = .TRUE.
    n=size(matrix,dim=1)
    l = 1
    !Convert to upper triangular form
    DO k = 1, n-1
        IF (matrix(k,k) == 0) THEN
            DetExists = .FALSE.
            DO i = k+1, n
                IF (matrix(i,k) /= 0) THEN
                    DO j = 1, n
                        temp = matrix(i,j)
                        matrix(i,j)= matrix(k,j)
                        matrix(k,j) = temp
                    END DO
                    DetExists = .TRUE.
                    l=-l
                    EXIT
                ENDIF
            END DO
            IF (DetExists .EQV. .FALSE.) THEN
                Det = 0
                return
            END IF
        ENDIF
        DO j = k+1, n
            m = matrix(j,k)/matrix(k,k)
            DO i = k+1, n
                matrix(j,i) = matrix(j,i) - m*matrix(k,i)
            END DO
        END DO
    END DO
   
    !Calculate determinant by finding product of diagonal elements
    Det = l
    DO i = 1, n
        Det = Det * matrix(i,i)
    END DO
   
END FUNCTION Det


subroutine rand_vec(a)
real*8 a(:)
integer i,n
n=size(a)
do i=1,n
a(i)=rand()
end do
end subroutine rand_vec

subroutine nrand_vec(a)
real*8 a(:)
integer i,n
n=size(a)
do i=1,n
a(i)=nrand()
end do
end subroutine nrand_vec

subroutine rand_mat(a)
real*8 a(:,:)
integer i,n,j
n=size(a,dim=1)
do i=1,n
do j=1,n
a(j,i)=rand()
end do
end do
end subroutine rand_mat

subroutine nrand_mat(a)
real*8 a(:,:)
integer i,n,j
n=size(a,dim=1)
do i=1,n
do j=1,n
a(j,i)=nrand()
end do
end do
end subroutine nrand_mat



subroutine sym_rand_mat(a)
real*8::a(:,:)
integer::nrow,ncolumn,i,j,n
nrow=size(a,dim=1)
ncolumn=size(a,dim=2)
n=nrow
if(nrow.ne.ncolumn) then
write(*,*) "error, not square matrix"
stop
endif

do j=1,n
do i=j,n
a(i,j)=rand()
enddo
enddo


do j=2,n
do i=1,j
a(i,j)=a(j,i)
enddo
enddo
end subroutine sym_rand_mat



subroutine sym_nrand_mat(a)
real*8::a(:,:)
integer::nrow,ncolumn,i,j,n
nrow=size(a,dim=1)
ncolumn=size(a,dim=2)
n=nrow
if(nrow.ne.ncolumn) then
write(*,*) "error, not square matrix"
stop
endif

do j=1,n
do i=j,n
a(i,j)=nrand()
enddo
enddo


do j=1,n
do i=1,j
a(i,j)=a(j,i)
enddo
enddo
end subroutine sym_nrand_mat


subroutine temp_cl()
logical::x,y
y=.true.
call execute_command_line("rm temp_mat.dat")
call execute_command_line("rm temp_a.mat")
call execute_command_line("rm temp_b.mat")
call execute_command_line("rm temp_oct.m")
INQUIRE(FILE="octave-workspace", EXIST=x)

if(x.eqv.y) call execute_command_line("rm octave-workspace")

end subroutine temp_cl




!file read write(libreadwrite)

subroutine mat_read(list,filename)

integer :: i, isize,j
double precision, dimension(:,:), allocatable, intent(inout) :: list
integer n
character(len=*)::filename
n = 0 
OPEN (1, file =filename) 
DO 
    READ (1,*, END=10) 
    n = n + 1 
END DO 
10 CLOSE (1)

allocate(list(n,n))
open(2,file=filename)

do i=1,n
read(2,*) (list(i,j),j=1,n)
enddo
close(2)

end subroutine mat_read

subroutine vec_read1(list,filename)

integer :: i, isize,j
double precision, dimension(:), allocatable, intent(inout) :: list
integer n
character(len=*)::filename
n = 0 
OPEN (1, file =filename) 
DO 
    READ (1,*, END=10) 
    n = n + 1 
END DO 
10 CLOSE (1) 
allocate(list(n))
open(2,file=filename)

do i=1,n
read(2,*) list(i)
enddo
close(2)
end subroutine vec_read1

subroutine vec_read2(list1,list2,filename)

integer :: i, isize,j
double precision, dimension(:), allocatable, intent(inout) :: list1,list2
integer n
character(len=*)::filename
n = 0 
OPEN (1, file =filename) 
DO 
    READ (1,*, END=10) 
    n = n + 1 
END DO 
10 CLOSE (1) 
allocate(list1(n),list2(n))
open(2,file=filename)

do i=1,n
read(2,*) list1(i),list2(i)
enddo
close(2)
end subroutine vec_read2

subroutine vec_read3(list1,list2,list3,filename)
integer :: i, isize,j
double precision, dimension(:), allocatable, intent(inout) :: list1,list2,list3
integer n
character(len=*)::filename
n = 0 
OPEN (1, file =filename) 
DO 
    READ (1,*, END=10) 
    n = n + 1 
END DO 
10 CLOSE (1) 
 
allocate(list1(n),list2(n),list3(n))
open(2,file=filename)

do i=1,n
read(2,*) list1(i),list2(i),list3(i)
enddo
close(2)
end subroutine vec_read3

subroutine vec_read4(list1,list2,list3,list4,filename)

integer :: i, isize,j
double precision, dimension(:), allocatable, intent(inout) :: list1,list2,list3,list4
integer n
character(len=*)::filename
n = 0 
OPEN (1, file =filename) 
DO 
    READ (1,*, END=10) 
    n = n + 1 
END DO 
10 CLOSE (1) 
 
allocate(list1(n),list2(n),list3(n),list4(n))
open(2,file=filename)

do i=1,n
read(2,*) list1(i),list2(i),list3(i),list4(i)
enddo
close(2)
end subroutine vec_read4


subroutine mat_write(a,filename)
real*8,intent(in)::a(:,:)
integer::n
character(len=*)::filename
integer i,j
if(size(a,dim=1).ne.size(a,dim=2)) then
write(*,*) "not square matrix"
stop
endif
n=size(a,dim=1)

open(1,file=filename)


do i=1,n
write(1,*) (/(a(i,j),j=1,n)/)
enddo
close(1)
end subroutine mat_write



subroutine vec_write1(a,filename)
real*8::a(:)
integer::n
integer i
character(len=*)::filename
n=size(a)
open(1,file=filename)

do i=1,n
write(1,*) a(i)
enddo
close(1)
end subroutine vec_write1


subroutine vec_write2(a,b,filename)
real*8 a(:),b(:)
integer::n
integer i
character(len=*)::filename
if(size(a).ne.size(b)) then
write(*,*) "size not same"
stop
endif
n=size(a)
open(1,file=filename)

do i=1,n
write(1,*) a(i),b(i)
enddo
close(1)
end subroutine vec_write2


subroutine vec_write3(a,b,c,filename)
real*8,intent(in):: a(:),b(:),c(:)
integer::n
integer i
character(len=*)::filename
if(size(a).ne.size(b) .or. size(b).ne.size(c)) then
write(*,*) "size not same"
stop
endif

n=size(a)
open(1,file=filename)

do i=1,n
write(1,*) a(i),b(i),c(i)
enddo
close(1)
end subroutine vec_write3

subroutine vec_write4(a,b,c,d,filename)
real*8,intent(in):: a(:),b(:),c(:),d(:)
integer::n
integer i
character(len=*)::filename
if(size(a).ne.size(b) .or. size(b).ne.size(c) .or. size(c).ne.size(d)) then
write(*,*) "size not same"
stop
endif
n=size(a)
open(1,file=filename)

do i=1,n
write(1,*) a(i),b(i),c(i),d(i)
enddo
close(1)
end subroutine vec_write4

!end libreadwrite


end module mathlib

