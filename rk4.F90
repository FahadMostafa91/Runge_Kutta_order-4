!     Last change:  MIJ  24 May 2014    8:50 pm
PROGRAM rk4
IMPLICIT NONE
INTEGER ::n
REAL ::a,b,y0

!example4,p-279

OPEN (1,'ri.dat')
OPEN (2,'rt.dat')

READ (1,*)a,b,n,y0
WRITE (2,*)a,b,n,y0

CALL rk(a,b,n,y0)

END PROGRAM

SUBROUTINE rk(a,b,n,y0)
IMPLICIT NONE
INTEGER ::n,i
REAL ::f,a,b,y0,x,h,k1,k2,k3,k4,w
h=(b-a)/n
x=a
w=y0
WRITE (2,15)0,x,w
15 FORMAT(1x,I3,3x,F5.2,3x,F15.8)
DO i=1,n
  k1=h*f(x,w)
  k2=h*f((x+.5*h),(w+.5*k1))
  k3=h*f((x+.5*h),(w+.5*k2))
  k4=h*f((x+h),(w+k3))
  w=w+((k1+2*k2+2*k3+k4)/6.0)
  x=a+i*h
  WRITE (2,11)i,x,w
 11 FORMAT (1x,I3,3x,F5.2,3x,F15.8)
END DO
RETURN

END SUBROUTINE

REAL FUNCTION f(x,w)
f=w-(x**2)+1
RETURN
END
