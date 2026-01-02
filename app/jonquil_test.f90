program jonquil_test

    use jonquil
    use iso_fortran_env, only: int64,real64

    implicit none

    class(json_value), allocatable :: data
    type(json_error), allocatable :: error

    integer(int64) :: start, finish, count_rate

    call system_clock(start, count_rate)
    call json_load(data, 'canada.json', error=error)

    call system_clock(finish)

    write(*,'(A30,1X,F7.4,1X,A)') 'jonquil : ', (finish-start)/real(count_rate,real64), ' seconds'

    if (allocated(error)) error stop error%message

end program jonquil_test
