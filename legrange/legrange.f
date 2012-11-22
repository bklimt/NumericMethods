C Bryan Klimt
C 09/25/01
C LeGrange.f
C
C Set up arrays
      dimension XX(101), XL(101), P(101), F(101), X(101)
C Set up amount of data to read
      data N,M / 7,29 /
C Read in the values of X to use
      write(6,100)
  100 format(' What are the values of x? ')
      read(5,300) (X(I), I=1,7)
  300 format(7 f5.2)
C Read in the values of f(x) to use
      write(6,200)
  200 format(' What are the values of f(x)? ')
      read(5,400) (F(I), I=1,7)
  400 format(7 f9.7)
C Read in the values of x to approximate
      write(6,250)
  250 format(' What are the values of x to approximate? ')
      read(5,450) (XX(I), I=1,29)
  450 format(29 f5.2)
C Write out the values read (for verification)
      write(6,410) (X(I), I=1,7)
  410 format(' x = ', 7 f6.2)
      write(6,420) (F(I), I=1,7)
  420 format(' f(x) = ', 7 f11.7)
C For every value to approximate
      do 700 J=1,M
      SUM = 0.0
C For every term in the polynomial
      do 600 K=1,N
      PROD = 1.0
C For every factor in L(), multiply by (x-xi)/(xk-xi)
      do 500 I=1,N
      if (I.eq.K) goto 500
      PROD = PROD*(XX(J)-X(I))/(X(K)-X(I))
  500 continue
C Add this to the rest of the polynomial
      XL(K) = PROD
      SUM = SUM + F(K)*XL(K)
  600 continue
C Store the approximation
      P(J) = SUM
  700 continue
C Write out all of the approximations
      do 900 I=1,M
      write(6,800) XX(I), P(I)
  800 format(' P(', f5.2, ') = ', f10.7)
  900 continue
C End
      stop
      end
