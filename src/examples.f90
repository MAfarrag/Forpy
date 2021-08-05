program Examples
	USE forpy
	
	implicit none
	real :: TargetValue, precision, realvalue1, realvalue2
	integer :: loc
	integer*8 :: ii, jj
	real*4, pointer :: x(:), newx(:)
	integer*4, pointer :: xi(:), newxi(:)
!-------------------------------------------------------------
! Round
	realvalue1 =  192293.57704555
	precision = 100.0
	realvalue1 = Round(realvalue1) !, precision
!-------------------------------------------------------------
! FindInArrayF
! find float number if array
! if you did not provide a precision the default precision will be two decimal numbers
	allocate(x(5))
	x = (/1.23569,2.15987,1.12347,4.263597,5.23597/)
	TargetValue = 1.12347
	call FindInArrayF(TargetValue, x, loc)

	! less decimal numbers
	TargetValue = 1.23
	call FindInArrayF(TargetValue, x, loc)

	! more decimal numbers
	TargetValue = 1.235696565
	call FindInArrayF(TargetValue, x, loc)

	deallocate(x)
	!-------------------------------------------------------------
	! Expand1DArrayR
	! add row at the end of the array
	! the new element will be filled with -123456789 value
	allocate(x(5), xi(5))
	x = (/1,2,1,4,5/)
	write(*,*) x
	xi = (/1,2,1,4,5/)
	write(*,*) xi
	call Expand1DArrayR(x)
	write(*,*) x
	call Expand1DArrayI(xi)
	write(*,*) xi 
	deallocate(x, xi)
	!-------------------------------------------------------------
	! SetF
	! get unique values of array x, delete repeated value
	allocate(x(5), xi(5))
	x = (/1,2,1,4,5/)
	xi = (/1,2,1,4,8/)
	write(*,*) x
	call SetF(x, newx)
	write(*,*) newx
	write(*,*) xi
	call SetI(xi, newxi)
	write(*,*) newxi
	deallocate(x, xi)


end program Examples
