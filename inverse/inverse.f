C
C Bryan Klimt
C 11-01-01
C
C inverse.f
C
      dimension A(25,25), AA(25,50), AINV(25,25), C(25,25),
     _          B(25), BB(25), X(25)
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
C Augment A
C
      do 320, I=1,N
      do 310, J=1,N
      AA(I,J) = A(I,J)
      AA(I,J+N) = 0
      if (I.ne.J) goto 310
      AA(I,J+N) = 1
  310 continue
  320 continue
      write(6,330)
  330 format(' AA = ')
      do 350, I=1,N
      write(6,340) (AA(I,J), J=1,2*N)
  340 format(25 f12.8)
  350 continue
      
C
C Gaussian Method
C
      do 450, K=1,N
      do 440, I=1,N
      TEMP = AA(I,K) / AA(K,K)
      if (I.eq.K) goto 440
      do 430, J=1,2*N
      AA(I,J) = AA(I,J) - (AA(K,J) * TEMP)
  430 continue
  440 continue
  450 continue
      do 452, I=1,N
      TEMP = AA(I,I)
      do 451, J=1,2*N
      AA(I,J) = AA(I,J) / TEMP 
  451 continue
  452 continue
C
C Set AINV
C
      do 470, I=1,N
      do 460, J=1,N
      AINV(I,J) = AA(I,J+N)
  460 continue
  470 continue
      write(6,475)
  475 format(' AINV = ')
      do 490, I=1,N
      write(6,480) (AINV(I,J), J=1,N)
  480 format(25 f12.8)
  490 continue
C
C Set C = A * AINV
C
      do 520, I=1,N
      do 510, J=1,N
      C(I,J) = 0.0
      do 500, K=1,N
      C(I,J) = C(I,J) + ( A(I,K) * AINV(K,J) )
  500 continue
  510 continue
  520 continue
      write(6,525)
  525 format(' C = A * AINV = ')
      do 540, I=1,N
      write(6,530) (C(I,J), J=1,N)
  530 format(25 f12.8)
  540 continue
C
C Set X = AINV * B
C
      do 560, I=1,N
      X(I) = 0
      do 550, J=1,N
      X(I) = X(I) + ( AINV(I,J) * B(J) )
  550 continue
  560 continue
      write(6,565)
  565 format(' X = AINV * B = ')
      do 580, I=1,N
      write(6,570) X(I)
  570 format(f12.8)
  580 continue
C
C Set BB = A * X
C
      do 660, I=1,N
      BB(I) = 0
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
C Finished
C
      end
