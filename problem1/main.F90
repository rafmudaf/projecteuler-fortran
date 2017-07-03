
program problem1
  implicit none

  integer :: i
  integer :: sum = 0
  logical :: isMultipleOf

  do i=1,999
    if ((isMultipleOf(i,3) .eqv. .TRUE.) .or. (isMultipleOf(i,5) .eqv. .TRUE.)) then
      sum = sum + i
    end if
  end do
  print *, sum
end program

function isMultipleOf(val, query)
  logical :: isMultipleOf
  integer, intent(in) :: val, query
  isMultipleOf = mod(val, query) == 0
end function
