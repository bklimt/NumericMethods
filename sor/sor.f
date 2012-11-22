C
C Bryan Klimt
C 11-13-01
C
C sor.f
C
      dimension A(25,25), B(25), T(25,25), C(25), BB(25), 
     _ X(25), XK(25), XKM1(25), OMEGA(6)
C
      DATA (OMEGA(I),I=1,5) / 0.4, 0.6, 1.0, 1.4, 1.6 /
C
C Read N
C
      write(6,100)
  100 format(' N? ')
      read(5,105) N
  105 format(i3)
      write(6,110) N
  110 format(' N = ',i3)
C
C Read A
C
      write(6,115)
  115 format(' A? ')
      do 130, I=1,N
      read(5,120) (A(I,J), J=1,N)
  120 format(25 f12.8)
  130 continue
      write(6,135)
  135 format(' A = ')
      do 150, I=1,N
      write(6,140) (A(I,J), J=1,N)
  140 format(25 f12.8)
  150 continue
C
C Read B
C
      write(6,160)
  160 format(' B? ')
      do 180, I=1,N
      read(5,170) B(I)
  170 format(f12.8)
  180 continue
      write(6,185)
  185 format(' B = ')
      do 200, I=1,N
      write(6,190) B(I)
  190 format(f12.8)
  200 continue
C
C Determine C and T
C
      do 320, I=1,N
      C(I) = B(I) / A(I,I)
      do 310, J=1,N
      T(I,J) = 0
      if (I.eq.J) goto 310
      T(I,J) = -1.0 * A(I,J) / A(I,I)
  310 continue
  320 continue
      write(6,330)
  330 format(' T = ')
      do 350, I=1,N
      write(6,340) (T(I,J), J=1,N)
  340 format(25 f12.8)
  350 continue
      write(6,360)
  360 format(' C = ')
      do 380, I=1,N
      write(6,370) C(I)
  370 format(f12.8)
  380 continue
C
C Initialize variable(s) for finding best omega
C
      MINITERATIONS = 26
C
C For each omega
C
      do 800, IOMEGA=1,5
      write(6,400) OMEGA(IOMEGA)
  400 format(' OMEGA = ', F5.2)
C
C Initialize XK
C
      write(6,405)
  405 format(' X0 = ')
      do 410, I=1,N
      XK(I) = 0.0
      write(6,407) XK(I)
  407 format(F12.8)
  410 continue
C
C Do at most 25 iterations
C
      do 540, ITERATION=1,25
C
C Set XKM1 = XK
C
      do 420, I=1,N
      XKM1(I) = XK(I)
  420 continue
C
C Find the new XK
C
      do 440, I=1,N
      SUM = 0.0
      do 430, J=1,N
      SUM = SUM + T(I,J)*XK(J)
  430 continue
      XK(I) = SUM + C(I)
  440 continue
C
C Find the XK for the next iteration
C
      do 450, I=1,N
      XK(I) = OMEGA(IOMEGA)*XK(I) - (OMEGA(IOMEGA)-1.0)*XKM1(I)
  450 continue
      write(6,510) ITERATION
  510 format(' XK = X', I2, ' = ')
      do 525, I=1,N
      X(I) = XK(I)
      write(6,520) X(I)
  520 format(F12.8)
  525 continue
C
C Find the norm(XK-XKM1) / norm(XK)
C
      TOPNORM = 0.0
      BOTTOMNORM = 0.0
      do 530 I=1,N
      TOPNORM = TOPNORM + (XK(I)-XKM1(I)) * (XK(I)-XKM1(I))
      BOTTOMNORM = BOTTOMNORM + XK(I) * XK(I)
  530 continue
      TOPNORM = sqrt(TOPNORM)
      BOTTOMNORM = sqrt(BOTTOMNORM)
      DIFF = TOPNORM / BOTTOMNORM
C
C Check for convergence
C
      if (DIFF.lt.0.0001) goto 542
  540 continue
      write(6,541)
  541 format(' NO CONVERGENCE AFTER 25 ITERATIONS!')
      goto 550
  542 write(6,543) ITERATION 
  543 format(' THERE WAS CONVERGENCE AFTER ', I2, ' ITERATIONS!')
C
C Check to see if this omega had the least iterations
C
      if (ITERATION.ge.MINITERATIONS) goto 550
      MINITERATIONS = ITERATION
      IBESTOMEGA = IOMEGA
C
C Print out the X found
C
  550 write(6,560)
  560 format(' X = ')
      do 580, I=1,N
      write(6,570) X(I)
  570 format(F12.8)
  580 continue
C
C Set BB = A * X
C
      do 660, I=1,N
      BB(I) = 0.0
      do 650, J=1,N
      BB(I) = BB(I) + ( A(I,J) * X(J) )
  650 continue
  660 continue
      write(6,665)
  665 format(' BB = A * X = ')
      do 680, I=1,N
      write(6,670) BB(I)
  670 format(f12.8)
  680 continue
C
C Print out some blank space before the next omega
C
      write(6,700)
      write(6,700)
      write(6,700)
  700 format(' ')
  800 continue
C
C Print out the best omega
C
      if (MINITERATIONS.lt.26) goto 880
      write(6,810)
  810 format(' ITERATIONS DID NOT CONVERGE WITH ANY OMEGA! ')
      goto 999
  880 write(6,890) OMEGA(IBESTOMEGA), MINITERATIONS
  890 format(' THE BEST OMEGA WAS ', F5.2, ' WITH ', I2, ' ITERATIONS.')
C
C Finished
C
  999 end
