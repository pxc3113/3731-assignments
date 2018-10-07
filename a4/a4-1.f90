
program foo
IMPLICIT NONE
INTEGER, DIMENSION (:), ALLOCATABLE :: Aone
INTEGER :: n,check
DO
PRINT *, "enter the number of elements (or 0 to stop):"
READ *, n
IF (n == 0) STOP
If (n < 0) THEN
PRINT *, "negative number of elements"
ELSE
ALLOCATE(Aone(n),STAT=check)
If (check .ne. 0) THEN
PRINT *, "unsuccessful ALLOCATE for Aone"
ELSE
PRINT '(1X,"enter the values of",I3," elements:")', n
READ *, Aone
PRINT '(1X,"data: ",15I5)', Aone
CALL countSeq(Aone)
!PRINT '(1X,"result: ",15I5)', Aone
DEALLOCATE(Aone)
END If
END if
END DO
CONTAINS
subroutine CountSeq (A)
integer, dimension (:) :: A
integer :: f,g,s,i,n,c
    n=SIZE(A)
    c = 0
    s = 0

    if (n < 2) print*, "c = ", -1

    do i = 1,n-1
        f = i
        g = i + 1

    select case (s)
            case (0) 
                if (A(f)<A(g)) then
                    s = 1
                    c = c+1
                else if(A(f)>A(g)) then
                    s = -1
                    c=c+1
                end if
            case (1)
                if (A(f) == A(g)) then
                    s=0
                else if (A(f) > A(g)) then
                    s=0
                    c=c+1
                end if
            case (-1) 
                if (A(f) == A(g)) then
                    s=0
                else if (A(f) < A(g)) then
                    s=0
                    c=c+1
                end if
        end select
    end do
    print*, "c = ",c
end subroutine CountSeq
end program 