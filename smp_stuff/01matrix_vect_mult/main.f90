!
!
      
      program main
      
      interface
          subroutine mxv(m, n, a, b, c)
              integer(kind=4), intent(in)    :: m, n
              real   (kind=8), intent(in)    :: b(1:m,1:n), c(1:n)
              real   (kind=8), intent(inout) :: a(1:m)
          end subroutine mxv
      end interface
      
      real   (kind=8), allocatable:: a(:), b(:,:), c(:)
      integer(kind=4)             :: m, n, i, memstat

      print *, 'Please give m and n: '; read(*,*) m, n

      allocate ( a(1:m), stat=memstat )
      if ( memstat /= 0 ) stop 'Error in memory allocation for a'
      allocate ( b(1:m,1:n), stat=memstat )
      if ( memstat /= 0 ) stop 'Error in memory allocation for b'
      allocate ( c(1:n), stat=memstat )
      if ( memstat /= 0 ) stop 'Error in memory allocation for c'

      print *, 'Initializing matrix B and vectr c'
      c(1:n) = 1.0
      do i = 1, m
        b(i,1:n) = i
      end do

      print *, 'Executing mxv routine for m = ', m, ' and n =', n
      call mxv(m, n, a, b, c)

      if ( allocated(a) ) deallocate(a, stat=memstat)
      if ( allocated(b) ) deallocate(b, stat=memstat)
      if ( allocated(c) ) deallocate(c, stat=memstat)
      stop
      end program main



      subroutine mxv(m, n, a, b, c)
      implicit none
      integer(kind=4):: m, n
      real   (kind=8):: a(1:m), b(1:m,1:n), c(1:n)

      integer(kind=4):: i, j
      do i = 1, m
        a(i) = 0.0
        do j = 1, n
            a(i) = a(i) + b(i,j) * c(j)
        end do
      end do

      return
      end subroutine mxv




