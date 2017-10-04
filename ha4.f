C program implements a crib dragging attack on simple 3x3 hill ciphers
C this version assumes piped in filtered input from a shell terminal
************************************************************************
      program ha4
      implicit none
      integer C, BL, NDIM, P, SS, i, j, k, l, n, blk, row, col, s, gcd,
     1 detmod, samsiz, passno, eof
      real t1, t2
      double precision tmpioc, tmpept
      character*25 crib
      character*50 timstr
      character*80 line
      logical match
      parameter(NDIM=4,BL=65536,C=26,P=13,SS=200)
      integer A(NDIM,NDIM), B(NDIM,NDIM), KEY(NDIM,NDIM),
     & KEYINV(NDIM,NDIM), TMP(NDIM,NDIM), v(NDIM), x(NDIM), freqs(C),
     & TABLE(2**NDIM**2,NDIM,NDIM)
      double precision relfrq(C)
      character buf(BL), pt(25), plntxt(SS)
      data freqs/26*0/, relfrq/.0781,.0128,.0293,.0419,.1305,.0288,
     & .0139,.0565,.0677,.0023,.0042,.036,.0262,.0728,.0821,.0215,.0014,
     & .0664,.0646,.0902,.0277,.01,.0149,.003,.0151,.0009/
C
C functions
C
      double precision ioc, ept
      logical isal, islo, isup
      character tolo, toup
C
C map bits 0 through NDIM**NDIM**2 - 1 to the TABLE of adjacency matrices
C
      do k=1,2**NDIM**2
         call bitmat(k-1, TMP, NDIM, NDIM)
         do i=1,NDIM
            do j=1,NDIM
               TABLE(k,i,j)=TMP(i,j)
            end do
         end do
      end do
C
C get command line parameters
C
      call getarg(1, crib)
      read (crib,'(19A1)') (pt(i),i=1,19)
C
C get piped in input into buffer
C
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

      if (n .gt. SS) then
         samsiz=SS
      else
         samsiz=n
      end if
      call cpu_time(t1)
      do blk=1,n-samsiz,NDIM
C
C load the ciphertext matrix
C
         k=1
         do j=1,NDIM
            do i=1,NDIM
               A(i,j)=ichar(buf(blk+k-1))-97
               k=k+1
            end do
         end do

         do passno=1,NDIM
            k=1
            do j=1,NDIM
               do i=1,NDIM
                  B(i,j)=ichar(pt(k+passno-1))-97
                  k=k+1
               end do
            end do

            do k=1,NDIM
               do j=1,NDIM
                  v(j)=A(k,j)
               end do
               call pgauss(B, x, v, NDIM, NDIM, P)
               do i=1,NDIM
                  KEY(i,k)=x(i)
               end do
            end do

            s=detmod(KEY, NDIM, NDIM, NDIM, C)
C
C cycle to next iteration (block?) if KEY matrix is singular
C
            if (s .eq. 0) cycle

C
C insert code to run through all the possible transformations of the
C matrix lifed from Z/13Z to Z/26Z
C
            do k=1,2**NDIM**2
               do j=1,NDIM
                  do i=1,NDIM
                     TMP(i,j)=KEY(i,j)+TABLE(k,i,j)
                  end do
               end do
               s=detmod(TMP, NDIM, NDIM, NDIM, C)
               if (gcd(s, C) .ne. 1) cycle
               call matinv(TMP, KEYINV, NDIM, NDIM, NDIM, C)
               do i=1,NDIM**2
                  plntxt(i)=buf(blk+i+passno-2)
               end do
               call hill(plntxt, NDIM**2, KEYINV, NDIM, NDIM, C)
C
C check whether plntxt and pt match, if no skip to next cycle
C
               match=.true.
               do i=1,NDIM**2
                  if (pt(i) .ne. plntxt(i)) match=.false.
               end do
C
C if the crib and the trial decrypt do not match cycle to next iteration
C
               if (match .eqv. .false.) cycle
               call matinv(TMP, KEYINV, NDIM, NDIM, NDIM, C)
               do i=1,samsiz
                  plntxt(i)=buf(blk+i+passno-2)
               end do
               call hill(plntxt, samsiz, KEYINV, NDIM, NDIM, C)
               call tab(freqs, C, plntxt, samsiz)
               tmpioc=ioc(freqs, C)
               if (tmpioc .ge. 1.6) then
                  tmpept=ept(freqs, relfrq, C, samsiz)
                  if (tmpept .ge. 0.06) then
                     write (*,*) ' '
                     write (*,*) 'potential key'
                     call outmat(TMP, NDIM, NDIM, NDIM)
                     write (*,2000) 'index of coincidence = ', tmpioc
                     write (*,2000) 'english correlation = ', tmpept
                     write (*,3000) (plntxt(i),i=1,80)
                  end if
               end if
            end do
         end do
      end do
      call cpu_time(t2)
      write (timstr,4000) t2-t1
      write (*,*) 'elapsed time = ', trim(adjustl(timstr)), ' seconds'
      stop
1000  format (a80)
2000  format (a,f7.3)
3000  format (80a1)
4000  format (f12.3)
      end
