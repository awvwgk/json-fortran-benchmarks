program json_fortran_test

    use json_module, only: json_file
    use iso_fortran_env, only: int64,real64

    implicit none

    type(json_file) :: f
    integer(int64) :: start, finish, count_rate
    logical :: status_ok
    character(len=:),allocatable :: error_msg

    call system_clock(start, count_rate)
    call f%load('canada.json')  
    call system_clock(finish)

    write(*,'(A30,1X,F7.4,1X,A)') 'json_fortran : ', (finish-start)/real(count_rate,real64), ' seconds'

    if (f%failed()) then
        call f%check_for_errors(status_ok=status_ok, error_msg=error_msg)
        write(*,*) error_msg
        error stop 'error parsing JSON file'
    end if

end program json_fortran_test