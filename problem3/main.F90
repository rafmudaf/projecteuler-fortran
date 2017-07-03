program problem3

! The prime factors of 13195 are 5, 7, 13 and 29.
! What is the largest prime factor of the number 600851475143 ?
!
! References:
! https://en.wikipedia.org/wiki/Primality_test
! https://www.mathsisfun.com/numbers/prime-factorization-tool.html

implicit none
logical :: isPrime
integer :: startingVal, currentVal, divisor, maxTest
real :: result
character(len=11) :: fmt

startingVal = 600851475143
currentVal = startingVal
divisor = 2
maxTest = sqrt(real(startingVal))

do while (divisor .le. maxTest)
  if (divisor == maxTest .and. mod(currentVal, divisor) .ne. 0) then
    divisor = startingVal
    break
  end if

  do while (mod(currentVal, divisor) .ne. 0 .or. .not. isPrime(divisor))
    divisor = divisor + 1
  end do

  result = currentVal/divisor

  print *, currentVal, " / ", divisor, " = ", result
  currentVal = result

  if (result .eq. 1) then
    break
  end if
end do

print *, "the largest prime factor of ", startingVal, " is ", divisor

end program


function isPrime(n)
logical :: isPrime
integer, intent(in) :: n
integer :: i, testvalue
real :: root

if (n .le. 1) then
    isPrime = .FALSE.
    return
else if (n .le. 3) then
    isPrime = .TRUE.
    return
else if ((mod(n,2) == 0) .or. (mod(n,3) == 0)) then
    isPrime = .FALSE.
    return
end if

i = 1
testvalue = 0
root = sqrt(real(n))
do while (testvalue < root)
    testvalue = 6*i-1
    if (n == testvalue) then
        isPrime = .TRUE.
        return
    else if (mod(n, testvalue) == 0) then
        isPrime = .FALSE.
        return
    end if

    testvalue = 6*i+1
    if (n == testvalue) then
        isPrime = .TRUE.
        return
    else if (mod(n, testvalue) == 0) then
        isPrime = .FALSE.
        return
    end if

    i = i + 1
end do

isPrime = .TRUE.

return

end function
