************************************************************************
C function finds length of character array sans trailing blanks
************************************************************************
      integer function fndlen(LL, buf)
      implicit none
      integer LL
      character buf(LL)
C
C local variables
C
      integer n

      n=LL
      do while (buf(n) .eq. ' ' .and. n .ne. 0)
         n=n-1
      end do
      fndlen=n
      end
************************************************************************
C function returns index of matching character
************************************************************************
      integer function fndidx(C, alpha, ch)
      implicit none
      integer C
      character alpha(C)
      character ch
C
C local variables
C
      integer n

      n=1
      do while (n .lt. C .and. ch .ne. alpha(n))
         n=n+1
      end do
      fndidx=n
      end
************************************************************************
C subprogram
************************************************************************
      logical function islo(ch)
      implicit none
      character ch

      if (ch .ge. 'a' .and. ch .le. 'z') then
         islo=.true.
      else
         islo=.false.
      end if
      end
************************************************************************
C subprogram
************************************************************************
      logical function isup(ch)
      implicit none
      character ch

      if (ch .ge. 'A' .and. ch .le. 'Z') then
         isup=.true.
      else
         isup=.false.
      end if
      end
************************************************************************
C subprogram
************************************************************************
      logical function isal(ch)
      implicit none
      character ch
C
C functions
C
      logical isup
      logical islo

      if (isup(ch) .or. islo(ch)) then
         isal=.true.
      else
         isal=.false.
      end if
      end
************************************************************************
C function returns lowercase of character ch if ch uppercase
C returns ch otherwise
************************************************************************
      character function tolo(ch)
      implicit none
      character ch
C
C functions
C
      logical isup

      if (isup(ch)) then
         tolo=char( ichar(ch) + 32 )
      else
         tolo=ch
      end if
      end
************************************************************************
C function returns uppercase of character ch if ch uppercase
C returns ch otherwise
************************************************************************
      character function toup(ch)
      implicit none
      character ch
C
C functions
C
      logical islo

      if (islo(ch)) then
         toup=char( ichar(ch) - 32 )
      else
         toup=ch
      end if
      end
************************************************************************
C subroutine creates mixing index
************************************************************************
      subroutine selidx(C, n, arr, idx)
      implicit none
      integer C, n, arr(n), idx(n)
C
C local variables
C
      integer i, j, k

      k=1
      do i=1,C
         do j=1,n
            if (arr(j) .eq. i) then
               idx(k)=j
               k=k+1
               if (k .gt. n) exit
            end if
         end do
         if (k .gt. n) exit
      end do
      end
************************************************************************
C subroutine mixes alphabet - same mix in single transposition ciphers
************************************************************************
      subroutine mix(C, n, idx, alpha)
      implicit none
      integer C, n, idx(n)
      character alpha(C)
C
C local variables
C
      integer i, j, k
      character tmp(C)

      do i=1,C
         alpha(i)=char(64+i)
         tmp(i)=' '
      end do

      i=1
      j=0
      k=1
      do while (k .le. C)
         do while (idx(i)+j*n .le. C)
            tmp(k)=alpha(idx(i)+j*n)
            k=k+1
            if (k .gt. C) exit
            j=j+1
         end do
         i=i+1
         if (i .gt. n) exit
         j=0
      end do
      do i=1,C
         alpha(i)=tmp(i)
      end do
      end
