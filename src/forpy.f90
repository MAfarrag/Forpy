MODULE forpy
!1-ExpandArrayR
!2-ExpandArrayI
!3-AppendR
!4-AppendI
!5-AppendInverseR
!6-AppendInverseI
!7-SetI
!8-SetF
!9-PrintArrayI
!10-PrintArrayR
!11-FindInArray
!12-FindInArrayF (SetF, should be the general one )
!13-FindFInArrayF (0)
!14-Sorting1Column
!15-SORTING
!16-int2str
!17-Interp
!18-sol2eq
!19-PolygonGeometry
!20-ReadASCII
!21-WriteASCII
IMPLICIT NONE

CONTAINS
    Function Round(x, precision)
		real :: x, defualt_precision, Round
		real, optional :: precision
		
		if (.not. present(precision)) then
			defualt_precision = 100.0
		else
			defualt_precision = precision 
		end if
		Round = INT(x * defualt_precision) / defualt_precision
		return
	end function Round

	SUBROUTINE ExpandArrayR(OldArray, Dim)
	!------------------------------------------------------------------
	! ExpandArrayR(OldArray, Dim)
	!------------------------------------------------------------------
	! ExpandArrayR subroutine expand the dimension of a given 2D array
	! by adding one row at the bottom or one column at the right of the 
	! given array
	!	Inpus:
	!		1- OldArray:[2D Real]
	!			1D array you want to append a value to it
	!		3-Dim:[Integer]
	!			1 to append the NewValue after the last row, 2 to append the the NewValue after
	!			the last column
	!	Example:
	!		
	!------------------------------------------------------------------
		INTEGER :: rowsold, colsold, Dim
		Real, POINTER :: OldArray(:,:), NewArray(:,:)
		
		
		rowsold = size(OldArray,1)
		colsold = size(OldArray,2)
		if (Dim == 1) then 
			allocate(NewArray(rowsold+1,colsold))
		else
			allocate(NewArray(rowsold,colsold+1))
		end if
		
		NewArray = 0
		
		if (Dim == 1) then
			if (rowsold == 1) then
				NewArray(1,:) = OldArray(1,:)
			else
				NewArray(1:rowsold,:) = OldArray(:,:)
			end if 
		else
			if (colsold == 1) then
				NewArray(:,1) = OldArray(:,1)
			else
				NewArray(:,1:colsold) = OldArray(:,:)
			end if 
		end if
		
		deallocate(OldArray)

		if (Dim == 1) then 
			allocate(NewArray(rowsold+1,colsold))
		else
			allocate(NewArray(rowsold,colsold+1))
		end if
		
		OldArray = NewArray 
		
		RETURN 
	ENDSUBROUTINE ExpandArrayR

	SUBROUTINE Expand1DArrayR(OldArray)
	!------------------------------------------------------------------
	! ExpandArrayR(OldArray, Dim)
	!------------------------------------------------------------------
	! ExpandArrayR subroutine expand the dimension of a given 2D array
	! by adding one row at the bottom or one column at the right of the 
	! given array
	!	Inpus:
	!		1- OldArray:[2D Real]
	!			1D array you want to append a value to it
	!		3-Dim:[Integer]
	!			1 to append the NewValue after the last row, 2 to append the the NewValue after
	!			the last column
	!	Example:
	!		
	!------------------------------------------------------------------
		INTEGER :: rowsold
		Real, POINTER :: OldArray(:), NewArray(:)
		
		
		rowsold = size(OldArray,1)
		
		allocate(NewArray(rowsold+1))
		
		NewArray = -123456789
		
		
		if (rowsold == 1) then
			NewArray(1) = OldArray(1)
		else
			NewArray(1:rowsold) = OldArray(:)
		end if 
		
		deallocate(OldArray)

		allocate(OldArray(rowsold+1))
		
		OldArray = NewArray 
		
		RETURN 
	ENDSUBROUTINE Expand1DArrayR

	SUBROUTINE Expand1DArrayI(OldArray)
	!------------------------------------------------------------------
	! ExpandArrayR(OldArray, Dim)
	!------------------------------------------------------------------
	! ExpandArrayR subroutine expand the dimension of a given 2D array
	! by adding one row at the bottom or one column at the right of the 
	! given array
	!	Inpus:
	!		1- OldArray:[2D Real]
	!			1D array you want to append a value to it
	!		3-Dim:[Integer]
	!			1 to append the NewValue after the last row, 2 to append the the NewValue after
	!			the last column
	!	Example:
	!		
	!------------------------------------------------------------------
		INTEGER :: rowsold
		integer, POINTER :: OldArray(:), NewArray(:)
		
		
		rowsold = size(OldArray,1)
		
		allocate(NewArray(rowsold+1))
		
		NewArray = -123456789
		
		
		if (rowsold == 1) then
			NewArray(1) = OldArray(1)
		else
			NewArray(1:rowsold) = OldArray(:)
		end if 
		
		deallocate(OldArray)

		allocate(OldArray(rowsold+1))
		
		OldArray = NewArray 
		
		RETURN 
	ENDSUBROUTINE Expand1DArrayI

	SUBROUTINE ExpandArrayI(OldArray, Dim)
	!------------------------------------------------------------------
	! ExpandArrayI(OldArray, Dim)
	!------------------------------------------------------------------
	! ExpandArrayR subroutine expand the dimension of a given 2D array
	! by adding one row at the bottom or one column at the right of the 
	! given array
	!	Inpus:
	!		1- OldArray:[2D Integer]
	!			1D array you want to append a value to it
	!		3-Dim:[Integer]
	!			1 to append the NewValue after the last row, 2 to append the the NewValue after
	!			the last column
	!	Example:
	!		
	!------------------------------------------------------------------
		INTEGER :: rowsold, colsold, Dim
		INTEGER, POINTER :: OldArray(:,:), NewArray(:,:)
		
		
		rowsold = size(OldArray,1)
		colsold = size(OldArray,2)
		if (Dim == 1) then 
			allocate(NewArray(rowsold+1,colsold))
		else
			allocate(NewArray(rowsold,colsold+1))
		end if
		
		NewArray = 0
		
		if (Dim == 1) then
			if (rowsold == 1) then
				NewArray(1,:) = OldArray(1,:)
			else
				NewArray(1:rowsold,:) = OldArray(:,:)
			end if 
		else
			if (colsold == 1) then
				NewArray(:,1) = OldArray(:,1)
			else
				NewArray(:,1:colsold) = OldArray(:,:)
			end if 
		end if
		
		deallocate(OldArray)

		if (Dim == 1) then 
			allocate(OldArray(rowsold+1,colsold))
		else
			allocate(OldArray(rowsold,colsold+1))
		end if
		
		OldArray = NewArray 
		
		RETURN 
	ENDSUBROUTINE ExpandArrayI


	SUBROUTINE AppendR(OldArray, NewValue)
	!------------------------------------------------------------------
	! AppendR(OldArray, NewValue)
	!------------------------------------------------------------------
	! AppendR subroutine append a new value at the end of an array 
	! 
	!	Inpus:
	!		1- OldArray:[Real]
	!			1D array you want to append a value to it
	!		2-NewValue:[real]
	!			real value to append to the array, if you want to append a value of a different
	!			type(integer) conver the value using the intrinsic functions int, real
	!	Example:
	!		
	!------------------------------------------------------------------
		INTEGER :: oldsize
		Real :: NewValue
		Real, POINTER :: OldArray(:), NewArray(:)
		
		
		oldsize = size(OldArray,1)
		allocate(NewArray(oldsize+1))
		
		NewArray = 0
		if (oldsize == 1) then
			NewArray(1) = OldArray(oldsize)
			NewArray(2) = NewValue
		else
			NewArray(1:oldsize) = OldArray
			NewArray(oldsize+1) = NewValue
		end if
		
		deallocate(OldArray)
		allocate(OldArray(oldsize+1))
		OldArray = NewArray 
		
		RETURN 
	ENDSUBROUTINE AppendR

	

	SUBROUTINE AppendI(OldArray, NewValue)
	!------------------------------------------------------------------
	! AppendI(OldArray, NewValue)
	!------------------------------------------------------------------
	! AppendI subroutine append a new value at the end of an array 
	! 
	!	Inpus:
	!		1- OldArray:[Integer]
	!			1D array you want to append a value to it
	!		2-NewValue:[real]
	!			real value to append to the array, if you want to append a value of a different
	!			type(integer) conver the value using the intrinsic functions int, real
	!	Example:
	!		
	!------------------------------------------------------------------
		INTEGER :: oldsize
		Real :: NewValue
		Integer, POINTER :: OldArray(:), NewArray(:)
		
		
		oldsize = size(OldArray,1)
		allocate(NewArray(oldsize+1))

		if (oldsize == 1) then
			NewArray(1) = OldArray(oldsize)
			NewArray(2) = NewValue
		else
			NewArray(1:oldsize) = OldArray
			NewArray(oldsize+1) = NewValue
		end if
		
		deallocate(OldArray)
		allocate(OldArray(oldsize+1))
		OldArray = NewArray 

		RETURN 
	ENDSUBROUTINE AppendI


	SUBROUTINE AppendInverseR(OldArray, NewValue)
	!------------------------------------------------------------------
	! AppendInverseR(OldArray, NewValue)
	!------------------------------------------------------------------
	! AppendInverseR subroutine append a new value at the top of an array
	! 
	!	Inpus:
	!		1- OldArray:[Real]
	!			1D array you want to append a value to it
	!		2-NewValue:[real]
	!			real value to append to the array, if you want to append a value of a different
	!			type(integer) conver the value using the intrinsic functions int, real
	!	Example:
	!		
	!------------------------------------------------------------------
		INTEGER :: oldsize
		Real :: NewValue
		Real, POINTER :: OldArray(:), NewArray(:)
		
		! get the size of the array and assign the new array with +1 the old size
		oldsize = size(OldArray,1)
		allocate(NewArray(oldsize+1))

		NewArray = 0
		if (oldsize == 1) then
			NewArray(2) = OldArray(oldsize)
			NewArray(1) = NewValue
		else
			NewArray(2:oldsize+1) = OldArray
			NewArray(1) = NewValue
		end if
		! reallocate the old array and assign it with the new array
		deallocate(OldArray)
		allocate(OldArray(oldsize+1))
		OldArray = NewArray 
		RETURN 
	ENDSUBROUTINE AppendInverseR


	SUBROUTINE AppendInverseI(OldArray, NewValue)
	!------------------------------------------------------------------
	! AppendInverseI(OldArray, NewValue)
	!------------------------------------------------------------------
	! AppendInverseR subroutine append a new value at the top of an array
	! 
	!	Inpus:
	!		1- OldArray:[Integer]
	!			1D array you want to append a value to it
	!		2-NewValue:[real]
	!			real value to append to the array, if you want to append a value of a different
	!			type(integer) conver the value using the intrinsic functions int, real
	!	Example:
	!		
	!------------------------------------------------------------------

		INTEGER :: oldsize
		Real :: NewValue
		Integer, POINTER :: OldArray(:), NewArray(:)
		
		
		oldsize = size(OldArray,1)
		allocate(NewArray(oldsize+1))

		if (oldsize == 1) then
			NewArray(2) = OldArray(oldsize)
			NewArray(1) = NewValue
		else
			NewArray(2:oldsize+1) = OldArray
			NewArray(1) = NewValue
		end if
		deallocate(OldArray)
		allocate(OldArray(oldsize+1))
		OldArray = NewArray 

		RETURN 
	ENDSUBROUTINE AppendInverseI


	SUBROUTINE SetI(Array1, Array2)
	!------------------------------------------------------------------
	! SetI(Array1, Array2)
	!------------------------------------------------------------------
	! SetI subroutine takes an array full of values that might be repeated
	! and return an array of unique values (delete repeated values)
	!	Inpus:
	!		1-Array1:[Integer]
	!			
	!	Example:
	!		INTEGER , POINTER :: Array1(:), Array2(:)
	!		allocate(Array1(7),Array2(7))
	!		Array1 = (/1,2,3,4,5,6,6/)
	!		call SetI(Array1, Array2)

		Integer, pointer :: Array1(:), Array2(:)
		Integer :: i, loc, CurrentValue, N
		
		N = 1
		allocate(Array2(1))
		Array2(1) = Array1(1)
		
		do i=2, size(Array1,1)
			CurrentValue = Array1(i)
			! search for the value i array2 
			call FindInArrayF(real(CurrentValue),real(Array2),loc)
			! if you did not find it add it to the new array
			if (loc == -1) then
				call Expand1DArrayI(Array2)
				N = N + 1
				Array2(N) = CurrentValue
			end if
		end do
		
	END SUBROUTINE SetI

	SUBROUTINE SetF(Array1, Array2)
	!------------------------------------------------------------------
	! SetI(Array1, Array2)
	!------------------------------------------------------------------
	! SetI subroutine takes an array full of values that might be repeated
	! and return an array of unique values (delete repeated values)
	!	Inpus:
	!		1-Array1:[Integer]
	!			
	!	Example:
	!		Real , POINTER :: Array1(:), Array2(:)
	!		allocate(Array1(7),Array2(7))
	!		Array1 = (/1,2,3,4,5,6,6/)
	!		call SetF(Array1, Array2)
	implicit none
		Real, pointer :: Array1(:), Array2(:)
		real :: CurrentValue
		Integer :: i, loc, N
		N = 1
		
		allocate(Array2(1))
		Array2(1) = Array1(1)
		
		do i=2, size(Array1,1)
			CurrentValue = Array1(i)
			! search for the value i array2 
			call FindInArrayF(real(CurrentValue),Array2,loc)
			! if you did not find it add it to the new array
			if (loc == -1) then
				call Expand1DArrayR(Array2)
				N = N + 1
				Array2(N) = CurrentValue
			end if
		end do
		
	END SUBROUTINE SetF


	SUBROUTINE FindDuplicate()
		
		implicit none
		Real, pointer :: Array1(:), Array2(:)
		real :: CurrentValue
		Integer :: i, loc, N
		N = 1
		
		allocate(Array2(1))
		Array2(1) = Array1(1)
		
		do i=2, size(Array1,1)
			CurrentValue = Array1(i)
			! search for the value i array2 
			call FindInArrayF(real(CurrentValue),Array2,loc)
			! if you did not find it add it to the new array
			if (loc == -1) then
				call Expand1DArrayR(Array2)
				N = N + 1
				Array2(N) = CurrentValue
			end if
		end do

	END SUBROUTINE FindDuplicate
	
	
	SUBROUTINE PrintArrayI(array, rows, columns, fname,decision, formatt)
	!------------------------------------------------------------------
	!! PrintArray(array, rows, columns, fileU,decision, formatt)
	!! PrintArray is used to print a 2D array on the screen or in a file
	!! if decision is 0 the array will be printed on screen if not array
	!! will be printed on file
	!! 
	!! iNPUTS:
	!!		1-array:[integer] 2D
	!!				dimensions of the array  should be (rows, columns)
	!!		2-rows:[integer]
	!!				number of rows (size of the array)
	!!		3-columns:[integer]
	!!				number of columns (size of the array)
	!!		4-fileU:[integer]
	!!				file unit to open (not yet)
	!!		5-decision:[integer]
	!!				0 to write on screen or 1 to write on a file
	!!		6-formatt:[character]
	!!				format of how things are going to be written in the file 
	!!Outputs:
	!!		1- content of the array are going to be eitherw ritten in the 
	!!			screen or in a file
	!! Examples:
	!!		! free format
	!!		CALL PrintArrayI(info(1:3,1:3), 3, 3, 900,0, "")
	!!		! on the screen formatted
	!!		CALL PrintArrayI(info(1:3,1:3), 3, 3, 900,0,"(i3,2x,i6,2x,i6)")
	!!		! on file free formatted
	!!		CALL PrintArrayI(info(1:3,1:3), 3, 3, 900,1,'' )
	!!		! on file formatted
	!!		CALL PrintArrayI(info(1:3,1:3), 3, 3, 900,1,"(i3,2x,i6,2x,i6)" )
	!------------------------------------------------------------------
	! DECLARATION
	CHARACTER(*) :: formatt, fname
	! 1D
	INTEGER :: rows, columns,i, j, decision !,fileU
	! 2D
	INTEGER, DIMENSION(rows,columns) :: array
	
	! EXECUTION
	! check if decision 0 on screen and format length more than 1 write formatted
	if (decision == 0 .and. len(trim(formatt)) == 0 ) then
	! print on the screen free format
		do i = 1,rows
			write(*,*) (array(i,j), j = 1,columns)
		end do
	else if(decision == 0 .and. len(trim(formatt)) > 0 ) then
	! print on the screen formatted
		do i = 1,rows
			write(*,formatt) (array(i,j), j = 1,columns)
		end do
	else if (decision == 1 .and. len(trim(formatt)) == 0 ) then
	! print on file free formatted
		open(90000, file=fname, status='unknown')
		do i = 1,rows
			write(90000,*) (array(i,j), j = 1,columns)
		end do
	else
		! print on file free formatted
		open(90000, file=fname, status='unknown')
		do i = 1,rows
			write(90000,formatt) (array(i,j), j = 1,columns)
		end do
	end if
	close(90000)
	
	END SUBROUTINE



	SUBROUTINE PrintArrayR(array, rows, columns, fname,decision, formatt)
	!------------------------------------------------------------------
	!! PrintArray(array, rows, columns, fileU,decision, formatt)
	!! PrintArray is used to print a 2D array on the screen or in a file
	!! if decision is 0 the array will be printed on screen if not array
	!! will be printed on file
	!! 
	!! iNPUTS:
	!! 		1-array:[Real] 2D
	!!				dimensions of the array  should be (rows, columns)
	!!		2-rows:[integer]
	!!				number of rows (size of the array)
	!!		3-columns:[integer]
	!!				number of columns (size of the array)
	!!		4-fileU:[integer]
	!!				file unit to open (not yet)
	!!		5-decision:[integer]
	!!				0 to write on screen or 1 to write on a file
	!!		6-formatt:[character]
	!!				format of how things are going to be written in the file 
	!! Outputs:
	!!		1- content of the array are going to be eitherw ritten in the 
	!!			screen or in a file
	!! Examples:
	!!		! free format
	!!		CALL PrintArrayI(info(1:3,1:3), 3, 3, 900,0, "")
	!!		! on the screen formatted
	!!		CALL PrintArrayI(info(1:3,1:3), 3, 3, 900,0,"(i3,2x,i6,2x,i6)")
	!!		! on file free formatted
	!!		CALL PrintArrayI(info(1:3,1:3), 3, 3, 900,1,'' )
	!!		! on file formatted
	!!		CALL PrintArrayI(info(1:3,1:3), 3, 3, 900,1,"(i3,2x,i6,2x,i6)" )
	!------------------------------------------------------------------

	! DECLARATION
	CHARACTER(*) :: formatt, fname
	! 1D
	INTEGER :: rows, columns,i, j, decision !fileU,
	! 2D
	REAL, DIMENSION(rows,columns) :: array
	
	! EXECUTION
	 ! check if decision 0 on screen and format length more than 1 write formatted
	if (decision == 0 .and. len(trim(formatt)) == 0 ) then
	! print on the screen free format
		do i = 1,rows
			write(*,*) (array(i,j), j = 1,columns)
		end do
	else if(decision == 0 .and. len(trim(formatt)) > 0 ) then
	! print on the screen formatted
		do i = 1,rows
			write(*,formatt) (array(i,j), j = 1,columns)
		end do
	else if (decision == 1 .and. len(trim(formatt)) == 0 ) then
		! print on file free formatted
		open(90000, file=fname, status='unknown')
		do i = 1,rows
			write(90000,*) (array(i,j), j = 1,columns)
		end do
	else
		! print on file free formatted
		open(90000, file=fname, status='unknown')
		do i = 1,rows
			write(90000,formatt) (array(i,j), j = 1,columns)
		end do
	end if
	close(90000)
	
	END SUBROUTINE PrintArrayR



	Subroutine FindInArray(TargetValue,Arraye,loc)
	!-------------------------------------------------------------------------------------------------
	! Subroutine FindInArray(TargetValue,Arraye,loc)
	! ----------------------------------------------------
	! This function find the location of an integer input value in an integer 1D array and if 
	! the value does not exist in the array a value of -1 will be returned
	! as a location
	!
	!	1-Argument   : DESCRIPTION (UNIT)
	!		1-TargetValue: 
	!			[Integer] the value you want to locate
	!			
	!		2-Arraye:
	!			[Integer] 1D array that contains values

	!	2-Output:
	!		1- loc:
	!			[integer] the index of the row that the target value 
	!			locates in the array
	!
	!	3- Examples:
	!			allocate(a(50))
	!			a=(/(i,i=1,50)/)
	!			call FindInArray(20,a,loc)
	!-------------------------------------------------------------------------------------------------
	! DECLARATION
		INTEGER :: TargetValue, i, loc
		INTEGER, INTENT(in) :: Arraye (:)
		
	! Execution
		loc = -1
		do i = 1, size(Arraye,1)
			if (Arraye(i) == TargetValue) then
				loc = i
				exit
			endif
		end do

		
	END SUBROUTINE FindInArray



	Subroutine FindInArrayF(TargetValue, Arr, loc, precision)
	!------------------------------------------------------------------------------------------------------
	! Subroutine FindInArray(TargetValue,Array,loc)
	! ----------------------------------------------------
	! This function find the location of an Rael input value in a Real 1D array and if 
	! the value does not exist in the array a value of -1 will be returned
	! as a location, to use the same function to search for an integer value use the real
	! intrinsic function to convert the TargetValue into real
	! FindInArrayF(int(TargetValue),Arr,loc)
	!
	!	1-Argument   : DESCRIPTION (UNIT)
	!		1-TargetValue: 
	!			[Integer] the value you want to locate
	!			
	!		2-Array:
	!			[Real] 1D array that contains values
	!			
	!	2-Output:
	!		1- loc:
	!			[integer] the index of the row that the target value 
	!			locates in the array
	!			
	!	3- Examples:
	!			allocate(a(50))
	!			a=(/(i,i=1,50)/)
	!			call FindInArray(20,a,loc)
	!------------------------------------------------------------------------------------------------------
	! DECLARATION
		!INTEGER :: TargetValue, i, loc
		INTEGER :: i, loc
		Real :: TargetValue, val1, val2
		Real, INTENT(in) :: Arr (:)
		real, optional :: precision
		
		loc = -1
		if (.not. present(precision)) then
			do i = 1, size(Arr,1)
				val1 = Round(Arr(i))
				val2 = Round(TargetValue)
				if (val1 == val2) then
					loc = i
					exit
				endif
			end do
		else
			do i = 1, size(Arr,1)
				val1 = Round(Arr(i),precision)
				val2 = Round(TargetValue,precision)
				if ( val1 == val2) then
					loc = i
					exit
				endif
			end do
		end if 
	END SUBROUTINE FindInArrayF


	Subroutine FindFInArrayF(TargetValue,Arr,loc)
	!------------------------------------------------------------------------------------------------------
	! Subroutine FindInArray(TargetValue,Array,loc)
	! ----------------------------------------------------
	! This function find the location of a the closest number in real 1D array to Real input value 
	! the value does not exist in the array a value of -1 will be returned
	! as a location
	!
	!	1-Argument   : DESCRIPTION (UNIT)
	!		1-TargetValue: 
	!			[Real] the value you want to locate
	!			
	!		2-Array:
	!			[Real] 1D array that contains values
			
	!	2-Output:
	!		1- loc:
	!			[integer] the index of the row that the target value 
	!			locates in the array
	!	
	!	3- Examples:
	!			allocate(a(50))
	!			a=(/(i,i=1,50)/)
	!			call FindInArray(20,a,loc)
	!------------------------------------------------------------------------------------------------------
	! DECLARATION
		REAL :: TargetValue
		INTEGER :: i, loc
		Real, INTENT(in) :: Arr (:) 
		REAL,DIMENSION(:), ALLOCATABLE :: diff
	! Execution
		ALLOCATE(diff(size(Arr,1)))
		loc = -1
		do i = 1, size(Arr,1)
			diff(i) = abs( Arr(i) - TargetValue)
		end do
		
		loc = minloc(diff,1)
		
	END SUBROUTINE FindFInArrayF





	Subroutine FindIn2Darray(TargetValue, DirectUS, i)
		REAL :: TargetValue !, DS
		INTEGER :: i, loc
		!Real, INTENT(in) :: Arr (:) 
		REAL, POINTER :: DirectUS(:,:)

		do i = 1, size(DirectUS,1)
			! search in each row if the TargetValue is found that is our row
			call FindInArrayF(real(TargetValue), DirectUS(i,:), loc)
			if (loc /= -1 ) then
				!DS = SubID(loc)
				EXIT
			end if 
		end do

	END SUBROUTINE 

	SUBROUTINE Sort1D(arr,n)
	!---------------------------------------------------------------------------------
	!Sorting1Column(a,m,n,column)
	!	Inputs:
	!		1-arr:
	!			[array] 2D array
	!		2-m:
	!			[INTEGER] number of rows in the array
	!		3-n:
	!			[INTEGER] number of columns in the array
	!		4-column:
	!			[INTEGER] the column you want to sort the array with respect to
	!		
	!		Outputs:
	!			1- array:
	!				[array] sorted array
	!---------------------------------------------------------------------------------
		IMPLICIT NONE
		INTEGER :: row, swap_index, n !nrows,
		integer :: arr(n), tmp
		
		DO row = 1, n - 1
			swap_index = MINLOC( arr(row:n) , dim=1 ) + row - 1
			! Swap_index
			tmp = arr(row)
			! Swap_index
			arr(row) = arr(swap_index)
			arr(swap_index) = tmp
		END DO
	
	END SUBROUTINE Sort1D


	SUBROUTINE Sorting1Column(arr,m,n,column)
	!---------------------------------------------------------------------------------
	!Sorting1Column(a,m,n,column)
	!	Inputs:
	!		1-arr:
	!			[array] 2D array
	!		2-m:
	!			[INTEGER] number of rows in the array
	!		3-n:
	!			[INTEGER] number of columns in the array
	!		4-column:
	!			[INTEGER] the column you want to sort the array with respect to
	!		
	!		Outputs:
	!			1- array:
	!				[array] sorted array
	!---------------------------------------------------------------------------------
		IMPLICIT NONE
		INTEGER :: column, row, nrows, swap_index, m, n
		REAL :: arr(m,n), tmp(n)
		nrows = size(arr,1 )
		
		! sort based on the first column
		DO row = 1, nrows-1
			! first column
			swap_index = MINLOC( arr(row:nrows,column) , dim=1 ) + row - 1
			! Swap_index
			tmp = arr(row,:)
			! Swap_index
			arr(row,:) = arr(swap_index,:)
			arr(swap_index,:) = tmp
		END DO
	
	END SUBROUTINE Sorting1Column




	SUBROUTINE SORTING(array,m,n,column1,column2)
	!---------------------------------------------------------------------------------
	!SORTING(a,m,n,column)
	!	Inputs:
	!		1-array:
	!			[REAL] 2D array
	!		2-m:
	!			[INTEGER] number of rows in the array
	!		3-n:
	!			[INTEGER] number of columns in the array
	!		4-column:
	!			[INTEGER] the column you want to sort the array with respect to
	!		
	!		Outputs:
	!			1- array:
	!				[array] sorted array
	!---------------------------------------------------------------------------------
	
	! DECLARATION
		IMPLICIT NONE
		INTEGER :: column1, column2, row, nrows, m, n, ro, & 
					no_unrepeated, start, endd 
			REAL :: array(m,n)
		REAL, DIMENSION(:,:), ALLOCATABLE :: unrepeated, smaller_array!, second_sort
		REAL, DIMENSION(:), ALLOCATABLE :: dummy
		
	! EXECUTION
		! sort based on the first column
		nrows = SIZE( array,1 )
		!write(*,*) 'sorted column = ', column1
		CALL Sorting1Column(array,m,n,column1)
			!write(*,*) 'sorted 1D---------------------------------------------------------------------------------'
		
		!do row=1,m
		!	write(*,'(i4,f8.1,2x,f8.1)') row,array(row,:)
			!end do
		!write(*,*) 'done'
		! sort based on the second column
		allocate(dummy(m))
		ro = 2
		dummy = -1
		dummy(1)=array(1,column1)
		
			do row = 2, nrows
				if (array(row-1,column1) /= array(row,column1)) then
				dummy(ro) = array(row,column1)
					ro = ro + 1
				end if 
		end do
			
			no_unrepeated = size(pack(dummy,dummy /= -1.0),1)
		
			!write(*,*) dummy
		!write(*,*) 'no_unrepeated',no_unrepeated
		
			allocate(unrepeated(no_unrepeated,2))
			
			unrepeated = 0
		! first column will be the number from the sorted column
		! the second column will be the number of rows that has the same 
		! number in the first column
		unrepeated(:,1) = dummy(1:no_unrepeated)
		
		!write(*,*)unrepeated(:,1)
		
			deallocate (dummy)
		!write(*,*) unrepeated(:,1)
		ro = 1
		! initialize the first repeated number by 1
		unrepeated(ro,2) = 1
		!counter = 0
		
		do row = 2, nrows
		  ! if the number is different 
			if (array(row-1,column1) /= array(row,column1)) then
				! go to the next row
			  ro = ro +1
				! initialize no of repeated number in the second column with 1
			  unrepeated(ro,2) = 1
			else
			  ! if the number is the same
			  ! increase the number in the second column with 1
				unrepeated(ro,2) = unrepeated(ro,2) +1
				end if         
		end do
			start = 1
		
			do row = 1, no_unrepeated
			if (row == 1) then
				endd = unrepeated(row,2)
					allocate (smaller_array(start:endd,n))
					smaller_array(:,:) = array(start:endd,1:n)
				
				CALL Sorting1Column(smaller_array,size(smaller_array,1),size(smaller_array,2),column2)
				
					array(start:endd,1:n) = smaller_array
				!write(*,*) smaller_array!(:,1:n)
				!write(*,*)'-----------'
			else
				start = start + unrepeated(row-1,2)
				endd = start + unrepeated(row,2)-1
					allocate (smaller_array(start:endd,n))
					smaller_array(:,:) = array(start:endd,1:n)
				
				CALL Sorting1Column(smaller_array,size(smaller_array,1),size(smaller_array,2),column2)
				
					array(start:endd,1:n) = smaller_array
				!write(*,*) smaller_array!(:,1:n)
				!write(*,*)'-----------'
				end if
				
				
				deallocate(smaller_array)
		end do 
		
	END SUBROUTINE SORTING
	


	FUNCTION int2str(num)
	!------------------------------------------------------------------------------------------------------
	! int2str(int)
	! ----------------------------------------------------
	! This function converts the integer number into a string type
	! 
	!	1-Argument:
	! 		1-int:
	!			[integer] number of type integer
	!
	!	2-Output:
	!		1- int2str:
	!			[character] the string form of the inputed integer number
	!
	!	3- Examples:
	!				num_str=int2str(100)
	!------------------------------------------------------------------------------------------------------

	INTEGER, INTENT(IN) :: num
	CHARACTER*6 :: int2str,temp
	CHARACTER*6 :: fs
	! choosing the format of the number by checking the length
	if (num>=100000) then
		fs = '(I6)'
	elseif (num>=10000) then
		fs = '(I5)'
	elseif (num>=1000) then
		fs = '(I4)'
	elseif (num>=100) then
		fs = '(I3)'
	elseif (num>=10) then
		fs = '(I2)'
	else 
		fs = '(I1)'
	end if

	write(temp,fs) num
	int2str = trim(temp)
	return
	END FUNCTION int2str



	FUNCTION str2int(string)
	!---------------------------------------------------------------------------
	! int2str(int)
	! ----------------------------------------------------
	! This function converts the integer number into a string type
	! 
	!	1-Argument:
	! 		1-int:
	!			[integer] number of type integer
	!
	!	2-Output:
	!		1- int2str:
	!			[character] the string form of the inputed integer number
	!
	!	3- Examples:
	!				num_str=int2str(100)
	!---------------------------------------------------------------------------

	INTEGER :: str2int !intent(out)
	character(len=*), intent(in) :: string
	! choosing the format of the number by checking the length
	read(string,*) str2int

	return

	END FUNCTION str2int


	FUNCTION str2real(string)
		!---------------------------------------------------------------------------
		! int2str(int)
		! ----------------------------------------------------
		! This function converts the integer number into a string type
		! 
		!	1-Argument:
		! 		1-int:
		!			[integer] number of type integer
		!
		!	2-Output:
		!		1- int2str:
		!			[character] the string form of the inputed integer number
		!
		!	3- Examples:
		!				num_str=int2str(100)
		!---------------------------------------------------------------------------
	
		real :: str2real !intent(out)
		character(len=*), intent(in) :: string
		! choosing the format of the number by checking the length
		read(string,*) str2real
	
		return
	
	END FUNCTION str2real

	SUBROUTINE Interp(InputVector,TargetTempRes,InputTempRes, InterpolatedVector)
	!------------------------------------------------------------------------------------------------------
	! Interp(InputVector,TargetTempRes,InputTempRes, InterpolatedVector)
	! ----------------------------------------------------
	! This function interpolate input time series from input temporal resolution (tsinput)
	! to another dt
	!
	!	1-Argument   : DESCRIPTION (UNIT)
	!		1-InputVector:
	!			[real] 1D array of values you want to interpolate
	!		2-dt:
	!			[integer] resulted time resolution (sec)
	!			 default is 60 sec
	!		3-tsInput:
	!			[integer] input time step (hrs)
	!			 default is 24 hrs
	!
	!	2-Output:
	!		1- hin:
	!			[real] 1D array of interpolated values(m)
	!
	!	3- Examples:
	!		HydrologicalTempRes = 24  ! (hourly)
	!		dt1D = 60		  !(min)
	!		! size(lflow(sSIM:eSIM,i))=508
	!		call Interp(lflow(sSIM:eSIM,i), dt1D, HydrologicalTempRes,  bnd_q)
	!------------------------------------------------------------------------------------------------------
	! DECLARATION
		REAL*4, DIMENSION(:), INTENT(IN) :: InputVector			! input InputVector
		INTEGER*4, INTENT(IN) :: TargetTempRes, InputTempRes	!time step of the input
		!REAL*4, ALLOCATABLE, DIMENSION(:), INTENT(OUT) :: hin !interpolated h
		REAL*4, POINTER :: InterpolatedVector(:) !interpolated h
		INTEGER :: i, j, correct
		
		correct = 24/InputTempRes							! 1
		! new size will be(508 day(size(InputVector)) * 24 *60)
		allocate(InterpolatedVector(86400/correct/TargetTempRes*(size(InputVector)-1)))		! (86400/1/60)*508= 731,520
		
		do i = 0, size(InputVector)-2 ! 0 to 507 days
			do j = 1, int(86400/correct/TargetTempRes) ! from 1 min to 1440 min
				InterpolatedVector(j+i*86400/correct/TargetTempRes) = InputVector(i+1) + &
                (InputVector(i+2) - InputVector(i+1))/(86400/correct)*j*TargetTempRes
			end do
		end do
		
		return
	END SUBROUTINE Interp


	FUNCTION sol2eq(a,b,c)
	!------------------------------------------------------------------------------------------------------
	! FUNCTION sol2eq(a,b,c)
	! ----------------------------------------------------
	! This function solves a quadratic equation at the form of aX^2+bX+c=0
	!
	!	1-Argument   : DESCRIPTION (UNIT)
	!		1-a: 
	!			[real] coefficient of X^2
	!			
	!		2-b:
	!			[real] coefficient of X
	!			
	!		3-c:
	!			[real] Free term
	!			
	!	2-Output:
	!		1- sol2eq:
	!			[real] only one solution of the equation (-b+sqrt(b^2-4ac))/2a
	!
	!	3- Examples:
	!		sol2eq(2,5,4)
	!------------------------------------------------------------------------------------------------------
	! DECLARATION
	!solution for the special case: a>0, delta>0, only the greater solution is needed
		REAL*4 :: sol2eq
		REAL*4, INTENT(IN) :: a, b, c
		REAL*4 :: delta
		delta = b**2 - 4*a*c
		sol2eq = (-b+sqrt(delta))/(2*a)
		!print*, sol2eq
		return
	END FUNCTION sol2eq




	FUNCTION PolygonGeometry(mp)
	!------------------------------------------------------------------------------------------------------
	! PolygonGeometry(mp)
	! ----------------------------------------------------
	! This function calculates the area and perimeter of the polygon using
	! the coordinates of the vortexes of polygon
	! 
	!	1-Argument : DESCRIPTION (UNIT)
	! 		1-mp:
	!			[real] 2D array of containing the coordinates of the vortexes of 
	!			the cross section with the x coord in the first column and the
	!			y coord in the second column
	!
	!	2-Output:
	!		1-PolygonGeometry:
	!			[real] 1D array contains the area and perimeter of the input polygon
	! 
	!	3- Examples:
	!				mp(1,1:4)=(/0,1,5,6/)   ! x coordinates
	!				mp(2,1:4)=(/1,0,0,1/)   ! y coordinates
	!				prop=PolygonGeometry(mp)
	!				area=prop(1)
	!				peri=prop(2)
	!------------------------------------------------------------------------------------------------------

		!REAL*4, ALLOCATABLE, DIMENSION(:,:), INTENT(IN) :: mp
		REAL*4, POINTER :: mp(:,:)
		REAL*4, DIMENSION(2):: PolygonGeometry
		REAL*4 :: area, peri
		INTEGER :: i, j

		j = size(mp,1)
		area = 0.0
		peri = 0.0
		do i = 1, size(mp,1)-1
			area = area + mp(i,1)*mp(i+1,2) - mp(i+1,1)*mp(i,2)
			peri = peri + ((mp(i+1,1)-mp(i,1))**2 + (mp(i+1,2)-mp(i,2))**2)**0.5		
		end do
		area = area + mp(j,1)*mp(1,2) - mp(1,1)*mp(j,2)
		area = area*0.5
		PolygonGeometry(:) = (/area, peri/)
		return
	END FUNCTION PolygonGeometry


	SUBROUTINE ReadASCII(Filename,Grid,Gridrows,Gridcols,LeftCornerX,Leftcornery,Nodata)
	!----------------------------------------------------------------------
	! populates the grid from the inputfile
	! clips the calculation grid from the base grid
	! based on the breach locations
	! and defines inflow cells
	!----------------------------------------------------------------
	!!   1-Argument   : DESCRIPTION (UNIT)
	!!
	!!		1-grd:
	!!			[Real] 2D array having the values of the grid you want to clip
	!!		2-grdrows:
	!!			[Integer] number of rows in the grid
	!!		3-grdcols:
	!!			[Integer] number of columns in the grid
	!!		4-filename:
	!!
	!!
	!!
	!!
	!----------------------------------------------------------------------
	CHARACTER*200		:: Filename
	DOUBLE PRECISION	:: Leftcornerx,Leftcornery,Nodata
	INTEGER				:: cellsize
	INTEGER				:: Gridrows,Gridcols
	REAL*4, Pointer		:: Grid(:,:)
	CHARACTER			:: dummy*15
	INTEGER				:: i!,j

	! open grid file
	open(9000,file=trim(Filename),action='read')
	read(9000,*) dummy,Gridcols
	read(9000,*) dummy,Gridrows
	read(9000,*) dummy,Leftcornerx
	read(9000,*) dummy,Leftcornery
	read(9000,*) dummy,cellsize
	read(9000,*) dummy,Nodata

	allocate(Grid(Gridrows,Gridcols))

	do i=1,Gridrows
		read(9000,*) Grid(i,1:Gridcols)
	end do

	close(9000)


	END SUBROUTINE ReadASCII
	
	
	
	SUBROUTINE WriteASCII(Filename,Grid,Gridrows,Gridcols,LeftCornerX,Leftcornery,cellsize,Nodata,formatt,&
				typeagree)
	!----------------------------------------------------------------------
	! populates the grid from the inputfile
	! clips the calculation grid from the base grid
	! based on the breach locations
	! and defines inflow cells
	!----------------------------------------------------------------
	!!	1-Argument   : DESCRIPTION (UNIT)
	!!		1-Filename:
	!!			[Real] 2D array having the values of the grid you want to clip
	!!		2-Grid:
	!!			[]
	!!		3-Gridrows:
	!!			[Integer] number of rows in the grid
	!!		4-Gridcols:
	!!			[Integer] number of columns in the grid
	!!		5-LeftCornerX:
	!!			[]
	!!		6-Leftcornery:
	!!			[]
	!!		7-cellsize:
	!!			[]
	!!		8-Nodata:
	!!			[]
	!!		9-formatt:
	!!			[String] string having the format of the number between two parentheses
	!!		10-typeagree:
	!!			[Integer] 1 if the array is float, 0 if the array is integer
	!----------------------------------------------------------------------
	CHARACTER*200		:: Filename,formatt
	DOUBLE PRECISION	:: Leftcornerx,Leftcornery,Nodata
	INTEGER				:: cellsize
	INTEGER				:: Gridrows,Gridcols
	REAL*4				:: Grid(Gridrows,Gridcols)
	!CHARACTER			:: dummy*15
	INTEGER				:: typeagree,i!,j
	
	! open grid file
	open(666,file=trim(Filename),action='write',STATUS="Unknown")
	write(666,'(A14,i6)') 'ncols         ', Gridcols
	write(666,'(A14,i6)') 'nrows         ', Gridrows
	write(666,'(A14,f16.4)') 'xllcorner     ', Leftcornerx
	write(666,'(A14,f16.4)') 'yllcorner     ', Leftcornery
	write(666,'(A14,i4)') 'cellsize      ', cellsize
	write(666,'(A14,i6)') 'NODATA_value  ', nint(Nodata)
	
	!allocate(Grid(Gridrows,Gridcols))
	if (typeagree ==1 ) then
	! if the type of the array is float as it is declared inside of the function
		do i=1,Gridrows
			!write(666,'('//trim(int2str(Gridcols))//'f6.2)') Grid(i,1:Gridcols)
			write(666,formatt) Grid(i,1:Gridcols)
		end do
	else
	! if the type of the array is integer in contrary to what is declared inside of the function
		do i=1,Gridrows
			write(666,formatt) int(Grid(i,1:Gridcols))
		end do
	end if
	
	close(666)


	END SUBROUTINE WriteASCII


	SUBROUTINE WriteData(wpath, step, SubID, BCWrite, QminWrite, HminWrite, bnd_q, bnd_h, RIMTempRes, &
						RRMTempRes, q_day, h_day)

		implicit none
		character*200 :: wpath, formattxt, dummy, fname
		integer ::BCWrite, QminWrite, HminWrite, step, SubID, RIMTempRes, RRMTempRes, m
		real*4, pointer :: bnd_h(:), bnd_q(:), h_day(:,:), q_day(:,:)

		if (BCWrite == 1) then
			open(10, file = trim(wpath)//'/results/USbnd/'//trim(int2str(SubID))//&
					'-'//trim(int2str(step))//'.txt',err=824)
			write(10,'(2F9.2)') (bnd_q(m), bnd_h(m), m= 1, size(bnd_q))
			close(10)
		end if
		! write the whole timeseries of Q and H----------------------------------------------------
		formattxt = "("//int2str(RIMTempRes*RRMTempRes)//'(F8.3,2x)'//")"
		dummy = trim(wpath)//'\results\'
				
		if (QminWrite == 1) then
			fname = trim(dummy)//'q\'//trim(int2str(int(SubID)))//'-q-'//trim(int2str(step))//'.txt'
			call PrintArrayR(q_day, size(q_day,1), size(q_day,2),fname,1, formattxt)
		end if
				
		if (HminWrite == 1) then
			fname = trim(dummy)//'h\'//trim(int2str(int(SubID)))//'-h-'//trim(int2str(step))//'.txt'
			call PrintArrayR(h_day, size(h_day,1), size(h_day,2),fname,1, formattxt)
		end if

		go to 1000
		824 write(*,*) 'file = '//'/USbnd/'//trim(int2str(SubID))//'-'//trim(int2str(step))//'.txt','  failed to open'

	1000 END SUBROUTINE WriteData


END MODULE forpy
