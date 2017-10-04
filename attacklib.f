************************************************************************
C subroutine implements the hill cipher - In previous versions the
C general matrix multiplication subroutine has been used. This version
C is optimized for column major ordering...
************************************************************************
      subroutine hill(buf, n, A, MDIM, m, C)
      implicit none
      integer C, MDIM, m, n, A(MDIM,m)
      character buf(n)
C
C local data
C
      integer i, j, k, v(m), x(m)

      do i=1,n,m
         do j=1,m
            v(j)=ichar(buf(i+j-1))-97
         end do
         do j=1,m
            x(j)=0
            do k=1,m
               x(j)=x(j)+A(k,j)*v(k)
            end do
         end do
         do j=1,m
            buf(i+j-1)=char(mod(x(j), C)+97)
         end do
      end do
      end
************************************************************************
C real function returns monographic english index of coincidence
************************************************************************
      double precision function ioc(x, C)
      implicit none
      integer C, x(C)
C
C local data
C
      integer i, n
      double precision total

      n=0
      do i=1,C
        n=n+x(i)
      end do
      do i=1,C
        total=total+dble(x(i))*(x(i)-1)
      end do
      ioc=total/(dble(n)*(n-1)/C)
      end
************************************************************************
C function calculates monographic english plaintext correlation
************************************************************************
      double precision function ept(frq, relfrq, C, n)
      implicit none
      integer C, frq(C), n
      double precision relfrq(C)
C
C local data
C
      integer i
      double precision x

      x=0.0
      do i=1,C
        x=x+frq(i)*relfrq(i)/(n)
      end do
      ept=x
      end
************************************************************************
C subroutine tabulates monographs
************************************************************************
      subroutine tab(table, C, buf, n)
      implicit none
      integer C, n, table(C)
      character buf(n)
C
C local data
C
      integer i, idx
C
C clear character frequency counts array
C
      do i=1,C
        table(i)=0
      end do
C
C tabulate character totals
C
      do i=1,n
        idx=ichar(buf(i))-96
        table(idx)=table(idx)+1
      end do
      end
************************************************************************
C subroutine converts bit pattern into an adjacency matrix
************************************************************************
      subroutine bitmat(bits, A, NDIM, n)
      implicit none
      integer bits, NDIM, n, A(NDIM,n)
C
C local data
C
      integer j, row, col

      row=1
      col=1
      do j=1,n**2
        if (BTEST(bits, j-1) .eqv. .true.) then
            A(row,col)=13
        else
            A(row,col)=0
        end if
        col=col+1
        if (col .gt. n) then
            col=1
            row=row+1
        end if
      end do
      end
************************************************************************
C pgaussian elimination subroutine - solves systems of linear equations
C finds solution vector x where Ax = v (modulo p version, p prime)
************************************************************************
      subroutine pgauss(A, x, b, NDIM, n, P)
      implicit none
      integer NDIM, n, P, A(NDIM,n), x(n), b(n)
C
C local data
C
      integer i, j, k, scalar, modinv
C
C gfortran supports the ability to have such an argument dimensioned
C as below so we shall use one to take the burden off the calling
C portion of a program to make sure that matrix A is somehow preserved
C
      integer ALOCAL(NDIM,n)
C
C copy matrix A to matrix ALOCAL
C
      call matcpy(A, ALOCAL, NDIM, n, n)
C
C initialize solution vector x with values from vector v
C
C      call matcpy(b, x, NDIM, 1, n)
      do i=1,n
         x(i)=b(i)
      end do
C
C upper triangular processing
C
      do j=1,n-1
         do i=j+1,n
            if (ALOCAL(j,j) .eq. 0) then
C
C swap columns
C
               do k=1,n
                  call swap(ALOCAL(k,j), ALOCAL(k,i))
               end do
               call swap(x(i), x(j))
            end if
            if (ALOCAL(j,j) .ne. 0) then
C
C reduce column
C
               scalar=ALOCAL(j,i)*modinv(ALOCAL(j,j), P)
               do k=1,n
                  ALOCAL(k,i)=-scalar*ALOCAL(k,j)+ALOCAL(k,i)
                  ALOCAL(k,i)=MOD(ALOCAL(k,i), P)
                  if (ALOCAL(k,i) .lt. 0) ALOCAL(k,i)=ALOCAL(k,i)+P
               end do
               x(i)=-scalar*x(j)+x(i)
            end if
         end do
      end do
C
C lower triangular processing
C
      do j=n,2,-1
         do i=j-1,1,-1
            if (ALOCAL(j,j) .eq. 0) then
C
C swap columns
C
               do k=1,n
                  call swap(ALOCAL(k,j), ALOCAL(k,i))
               end do
               call swap(x(i), x(j))
            end if
            if (ALOCAL(j,j) .ne. 0) then
               scalar=ALOCAL(j,i)*modinv(ALOCAL(j,j), P)
               do k=1,n
                  ALOCAL(k,i)=-scalar*ALOCAL(k,j)+ALOCAL(k,i)
                  ALOCAL(k,i)=MOD(ALOCAL(k,i), P)
                  if (ALOCAL(k,i) .lt. 0) ALOCAL(k,i)=ALOCAL(k,i)+P
               end do
               x(i)=-scalar*x(j)+x(i)
            end if
         end do
      end do
C
C normalization processing
C
      do j=1,n
         scalar=modinv(ALOCAL(j,j), P)
C
C normalize column
C
         do i=1,n
            ALOCAL(i,j)=scalar*ALOCAL(i,j)
            ALOCAL(i,j)=MOD(ALOCAL(i,j), P)
            if (ALOCAL(i,j) .lt. 0) ALOCAL(i,j)=ALOCAL(i,j)+P
         end do
         x(j)=MOD(scalar*x(j), P)
         if (x(j) .lt. 0) x(j)=x(j)+P
      end do
      end
************************************************************************
c subroutine swaps two integer locations
************************************************************************
      subroutine swap(m, n)
      implicit none
      integer m, n

      integer mtmp

      mtmp=m
      m=n
      n=mtmp
      end
