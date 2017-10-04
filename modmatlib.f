C library of matrix related functions and subroutines for integers
C congruent modulo Z
C at times column major order optimized code is written
C subroutine multiplies m x n matrix by scalar
*************************************************************************      
      subroutine matscl(s, A, MDIM, m, n, Z)
      implicit none
      integer s, MDIM, m, n, Z, A(MDIM,n)
C
C local data      
C      
      integer i, j
      
      do j=1,n
        do i=1,m
            A(i,j)=mod(s*A(i,j), Z)
            if (A(i,j) .lt. 0) A(i,j)=A(i,j)+Z
        end do
      end do
      end

************************************************************************
C subroutine multiplies m x n matrix and n x p matrix and stores result
C in m x p matrix
************************************************************************      
      subroutine mtxmlt(A, B, C, MDIM, NDIM, m, n, p, Z)
      implicit none
      integer MDIM, NDIM, m, n, p, A(MDIM, n), B(NDIM, p), C(MDIM, p), Z
C
C local data
C      
      integer i, j, k

      do j=1,p                    
        do i=1,m
            C(i,j)=0
            do k=1,n
                C(i,j)=mod(C(i,j)+A(i,k)*B(k,j), Z)               
            end do
        end do
      end do      
      end
      
c***********************************************************************
c subroutine writes double precision values to cells of double precision 
c valued matrix
c***********************************************************************      
      subroutine inmat(A, MDIM, m, n, Z)
      implicit none
      integer MDIM, m, n, Z, A(MDIM, n)
C
C local data
C      
      integer i, j
      
      do j=1,n
        do i=1,m
            A(i,j)=int(Z*rand())
        end do
      end do
      end     
      
************************************************************************
C subroutine displays m x n matrix a
************************************************************************
      subroutine outmat(A, MDIM, m, n)
      implicit none
      integer MDIM, m, n, A(MDIM, n)
C
C local data      
C      
      integer i, j

      write (*,*)
      do j=1,n
        write (*,1000) (A(i,j),i=1,m) 
      end do
      write (*,*)      
1000  format (19(X,I3))     
      end 
      
************************************************************************
C subroutine to find inverse of square matrix
C it will have to be inverible or one is out of luck
************************************************************************
      subroutine matinv(A, AINV, MDIM, m, n, Z)
      implicit none
      integer MDIM, m, n, a(MDIM,n), ainv(MDIM,n), Z
C
C local data
C      
      integer modinv, scalar, detmod     

      scalar=detmod(A, MDIM, m, n, Z)  
      if (scalar .lt. 0) scalar=scalar+Z    
C      
C get classical adjoint matrix (i. e. transpose of the cofactor matrix)
C      
      call matadj(A, AINV, MDIM, m, n, Z)
C      
C here we shall have to find the inverse of |A| congruent modulo m      
C     
      scalar=modinv(scalar, Z)
      call matscl(scalar, AINV, MDIM, m, n, Z)
      end
      
************************************************************************
C integer function returns inverse of n congruent modulo m
C returns 0 if it does not exist
C basic U-group approach, power n up until it is congruent 1 modulo c      
C this version P must be prime      
************************************************************************
      integer function modinv(n, P)
      implicit none
      integer n, P
C
C local data
C      
      integer z

      z=1
      do while ( MOD(z*n, P) .ne. 1)
        z=z+1
        if (z .gt. P) exit
      end do
      if (z .lt. 0) z=z+P
      modinv=MOD(z, P)        
      end          

************************************************************************
C copy contents of matrix a into matrix b
************************************************************************
      subroutine matcpy(A, B, MDIM, m, n)
      implicit none
      integer MDIM, m, n
      integer A(MDIM, n), B(MDIM, n)
C
C local data      
C
      integer i, j

      do j=1,n
        do i=1,m
            B(i,j)=A(i,j)
        end do
      end do
      end
      
************************************************************************
C transpose square matrix
************************************************************************
      subroutine tmat(A, T, MDIM, NDIM, m, n)
      implicit none
      integer MDIM, NDIM, m, n, A(MDIM, n), T(NDIM, m)
C
C local data
C      
      integer i, j

      do j=1,n
        do i=1,m
            T(i,j)=A(j,i)
        end do
      end do
      end     
      
************************************************************************
C classic adjoint matrix (transpose of the cofactor matrix)
************************************************************************
      subroutine matadj(A, ADJ, MDIM, m, n, Z)
      implicit none
      integer MDIM, m, n, Z
      integer A(MDIM,n), ADJ(MDIM,n)
C
C local data
C      
      integer i, j, cofact
 
      do j=1,n
        do i=1,m
            ADJ(i,j)=cofact(A, MDIM, m, n, j, i, Z)
        end do
      end do
      end     
      
************************************************************************
C double precision function returns cofactor in i, j position
************************************************************************
      integer function cofact(A, MDIM, m, n, i, j, Z)
      implicit none
      integer MDIM, m, n, i, j, Z
      integer A(MDIM,n)
C
C local data
C      
      integer row, col, k, l, AMINOR(n-1,n-1), detmod
      
      k=1
      do col=1,n
        l=1
        if (col .ne. j) then
            do row=1,m
                if (row .ne. i) then
                    AMINOR(l,k)=A(row,col)
                    l=l+1
                end if
            end do
            k=k+1
        end if
      end do 
      cofact=(-1)**(i+j)*detmod(AMINOR, n-1, n-1, n-1, Z)     
      if (cofact .lt. 0) cofact=cofact+Z
      end
           
************************************************************************
C function written in FORTRAN77 to calculate determinant of a square
C matrix
************************************************************************
      integer function detmod(A, MDIM, m, n, Z)
      implicit none
      integer MDIM, m, n, A(MDIM,n), Z
C
C local data - compute with double precision variables
C      
      double precision temp, ELEM(n,n), scalar
      integer i, j, k, l
      integer*8 s
      logical exists

      do j=1,n
        do i=1,m
            ELEM(i,j)=A(i,j)
        end do
      end do             
      exists=.true.
      l=1
C      
C convert to upper triangular form
C      
      do k=1,n-1
        if (abs(ELEM(k,k)) .le. 1.0d-20) then
            exists=.false.          
            do i=k+1,n
                if (ELEM(i,k) .ne. 0.0) then      
                    do j=1,n
                        call dswap(ELEM(i,j), ELEM(k,j))
                    end do      
                    exists=.true.
                    l=-l
                    exit      
                end if      
            end do
            if (exists .eqv. .false.) then
                detmod=0
                return
            end if
        end if
        do j=k+1,n
            scalar=ELEM(j,k)/ELEM(k,k)
            do i=k+1,n
                ELEM(j,i)=ELEM(j,i)-scalar*ELEM(k,i)
            end do
        end do
      end do  
C      
C calculate determinant by finding product of diagonal elements
C      
      s=l
      do i=1,n
        s=anint(s*ELEM(i,i), 8)
      end do 
C      
C round and convert back to integer after double precision computations     
C
      s=mod(s, Z)
      if (s .lt. 0) s=s+Z
      detmod=s
      end 

*************************************************************************
C integer function returns greatest common divisor of m and n
*************************************************************************
      integer function gcd(m, n)
      implicit none
      integer m, n   
C
C local data
C      
      integer mlocal, nlocal, temp
C
C make local copies of all parameter arguments so as not to destroy
C them, FORTRAN 77 passes by reference
C      
      mlocal=m
      nlocal=n     
      do while (nlocal .ne. 0)
         temp=mlocal
         mlocal=nlocal
         nlocal=mod(temp, nlocal)
      end do
      gcd=mlocal       
      end  

*************************************************************************
C subroutine swaps two double precision locations
*************************************************************************
      subroutine dswap(x, y)
      implicit none
      double precision x, y
C
C local data
C   
      double precision tmp

      tmp=x
      x=y
      y=tmp
      end     

*************************************************************************
C subroutine swaps two integer locations
*************************************************************************
      subroutine lswap(i, j)
      implicit none
      integer i, j
C
C local data
C   
      integer tmp

      tmp=i
      i=j
      j=tmp
      end           
