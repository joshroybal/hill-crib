C Hill cipher program
C this version assumes piped in 80 character uppercase filtered lines
************************************************************************
      program hill3
      implicit none
      integer BL, C, NDIM, i, idx, j, l, m, mdet, n, eof
      character blnk
      character*10 option
      character*50 keystr
      character*80 line
      parameter(C=26,BL=65536,NDIM=2)
      integer A(NDIM,NDIM), AINV(NDIM,NDIM), IDENT(NDIM,NDIM)
      character buf(BL)
C
C functions
C      
      logical isal
      character tolo, toup
C
C main processing
C
      call getarg(1, option)
C
C error trap
C
      if (option(1:1) .ne. 'e' .and. option(1:1) .ne. 'd') 
     1 stop 'insufficient or incorrect command line arguments'
      n=0
      read (*,1000,iostat=eof) line
      do while (eof .eq. 0)
         l=len(trim(line))
         do i=1,l
            if (isal(line(i:i))) then
               n=n+1
C
C bail out when buffer full
C               
               if (n .gt. BL) stop 'buffer limit exceeded'
               buf(n)=tolo(line(i:i))
            end if
         end do
         read (*,1000,iostat=eof) line
      end do

      if (option(1:1) .eq. 'e') then
         call genkey(A, NDIM, NDIM, C) 
         do while (mod(n, NDIM) .ne. 0)
            n=n+1
            buf(n)='z'
         end do
      else
         call getarg(2, keystr)
         read (keystr,*) ((A(i,j),j=1,NDIM),i=1,NDIM)
      end if

      m=NDIM

      if (option(1:1) .eq. 'e') then
         call hill(buf, n, A, NDIM, m, C)
      else if (option(1:1) .eq. 'd') then
         call matinv(A, AINV, NDIM, m, m, C)
         call hill(buf, n, AINV, NDIM, m, C)
      end if

      if (option(1:1) .eq. 'e') then
        write (*,2000) (buf(i),i=1,n)
      else
        write (*,2000) ( ( CHAR ( ICHAR (buf(i)) + 32) ) , i=1,n)
      end if
1000  format (a)
2000  format (80a1)      
      end
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
