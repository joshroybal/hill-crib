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
      parameter(C=26,BL=65536,NDIM=4)
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
