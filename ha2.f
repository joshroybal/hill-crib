C program implements a crib dragging attack on simple 3x3 hill ciphers
C this version assumes piped in filtered input from a shell terminal
************************************************************************
      program ha3
      implicit none
      integer C, BL, NDIM, P, SS, i, j, k, l, n, blk, row, col, s, gcd,
     1 detmod, samsiz, passno, eof
      real t1, t2
      double precision tmpioc, tmpept
      character*25 crib
      character*50 keystr, timstr
      character*80 line
      logical match
      parameter(NDIM=2,BL=65536,C=26,P=13,SS=200)
      integer A(NDIM,NDIM), B(NDIM,NDIM), KEY(NDIM,NDIM),
     & KEYINV(NDIM,NDIM), TMP(NDIM,NDIM), v(NDIM), x(NDIM), freqs(C),
     & TABLE(2**NDIM**2,NDIM,NDIM)
      double precision relfrq(C)
      character buf(BL), pt(25), plntxt(SS)
      data freqs/26*0/, relfrq/.0781,.0128,.0293,.0411,.1305,.0288,
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
      read (crib,'(5A1)') (pt(i),i=1,5)
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
***********************************************************************
C subroutine generates html form for world wide web submission
***********************************************************************
      subroutine webfrm(keystr, n)
      integer n
      character*(*) keystr
C
C local data
C
      integer i
      
      write (*,*) '<input type="radio" name="key" value="', TRIM(ADJUSTL
     1(keystr)), '"> select this trial decrypt'
      write (*,1000) ('*',i=1,80)
1000  format (80a1)
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
