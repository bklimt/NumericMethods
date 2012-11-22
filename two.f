C
C Bryan Klimt
C 09/18/01
C
C Program 2 - multiplies 2 matrices
C
C Create space to store the matrices
      dimension x(10),a(10,10),b(10)
C Read N
      write(6,90)
   90 format(' N? ')
      read(5,100) n
  100 format(i5)
C Read A
      write(6,190)
  190 format(' A? ')
      read(5,200) ((a(i,j),j=1,n),i=1,n)
  200 format(3f5.2)
C Read B
      write(6,290)
  290 format(' B? ')
      read(5,300) (b(i),i=1,n)
  300 format(f5.2)
C Calculate x
      do 320 i=1,n
      x(i) = 0
      do 310 j=1,n
      x(i) = x(i) + a(i,j) * b(j)
  310 continue
  320 continue
C Print A
      write(6,400)
  400 format(' A = ')
      write(6,500) ((a(i,j),j=1,n),i=1,n)
  500 format(' ', 3f5.2)
C Print B
      write(6,600)
  600 format(' B = ')
      write(6,700) (b(i),i=1,n)
  700 format(' ', f5.2)
C Print X
      write(6,750)
  750 format(' X = ')
      write(6,700) (x(i),i=1,n)
C
      end
