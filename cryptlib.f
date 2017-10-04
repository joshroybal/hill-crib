************************************************************************
C subroutine implements the hill cipher
************************************************************************
      subroutine hill(buf, n, A, MDIM, m, C)
      implicit none
      integer C, MDIM, m, n
      integer A(MDIM,m)
      character buf(n)
C
C local data
C
      integer i, j, v(m,1), x(m,1)

      do i=1,n,m
         do j=1,m
            v(j,1)=ICHAR(buf(i+j-1))-97
         end do
         call mtxmlt(A, v, x, MDIM, MDIM, m, m, 1, C)
         do j=1,m
            if (i+j-1 .gt. n) return
            buf(i+j-1)=CHAR(MOD(x(j,1), c)+65)
         end do
      end do
      end
************************************************************************
C subroutine generates random key matrix
************************************************************************
      subroutine genkey(KEY, NDIM, n, Z)
      implicit none
      integer NDIM, n, Z, KEY(NDIM,n)
C
C local data
C
      integer i, j, detmod, gcd
      call srand(time())
C
C zero out matrix so the next loop always runs at least once
C
      do j=1,NDIM
         do i=1,NDIM
            KEY(i,j)=0
         end do
      end do
      do while (gcd(detmod(KEY, NDIM, n, n, Z), Z) .ne. 1)
         do i=1,n
            do j=1,n
               KEY(i,j)=int(Z*rand())
            end do
         end do
      end do
      end
