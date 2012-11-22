C
C Bryan Klimt
C 09/18/01
C
C Program 1 - plays with do loops
C
C Create an array
C
      dimension jj(101)
C
C Fill in the array with values
C
      do 100 i=1,50
      jj(i)=i
  100 continue
C
C Print a header
C
      write(6,180)
  180 format('    i  jj(i) ')
C
C Print the list of numbers
C
      do 200 i=1,50
      write(6,190) i,jj(i)
  190 format(i5,i7)
  200 continue
C
C Add up the numbers
C
      isum = 0
      do 300 m=1,50
      isum = isum + jj(m)
  300 continue
C
C Print the results
C
      write(6,310) isum
  310 format(' isum=',i9)
C
      end
