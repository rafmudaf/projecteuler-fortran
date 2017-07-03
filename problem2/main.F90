program problem2
  implicit none
  logical :: isEven
  integer :: count = 0, sum = 0, pm1 = 1, p = 1, tmp = 0
  character(len=11) :: fmt

  fmt = "(i5,A,i8,A)"

  do while (p < 4000000)
    count = count + 1
    tmp = p
    p = p + pm1
    pm1 = tmp
    if (isEven(pm1) .eqv. .TRUE.) then
        print fmt, count, " - ", pm1, " even"
        sum = sum + pm1
    else
        print fmt, count, " - ", pm1, " odd"
    end if
  end do
  print "(A,i8)", "sum of even values in fibonacci ", sum
end program

function isEven(val)
  logical :: isEven
  integer, intent(in) :: val
  isEven = mod(val, 2) == 0
end function
