C     PROGRAM
C
      F(X)=EXP(X)-3.*X
      DF(X)=EXP(X)-3.
      X=-.1
C
      DO 100 K=1,25
      X=X+.1
      FX=F(X)
      WRITE(6,110) X,FX
  110 FORMAT( ' x = ', F12.5, '  f(x) = ',F12.5 )
  100 CONTINUE
C
   15 WRITE(6,35)
   35 FORMAT(///,' Enter initial value of x to start:')
   20 READ(5,30) X
   30 FORMAT(F15.8)
C
      DO 10 i=1,30
      XN=X-F(X)/DF(X)
      WRITE(6,50) I,XN
   50 FORMAT(' Iteration number = ',I5,'   Root = ', F15.10)
      IF (ABS((XN-X)/(XN+X)).LT. 0.0000001) GO TO 60
      X=XN
   10 CONTINUE
C
      WRITE(6,70)
   70 FORMAT(///,' Newtons Method did not Converge')
      GO TO 90
   60 WRITE(6,80) XN
   80 FORMAT(///,' Newtons Method Root at X = ',F15.10)
      FF=F(XN)
      WRITE(6,75) FF
   75 FORMAT( ' For this Root F(X) = ',E15.8)
C
   90 GO TO 15
      END

