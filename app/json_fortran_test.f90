program json_fortran_test

    use json_module, only: json_file

    implicit none

    type(json_file) :: f
    integer :: start, finish, count_rate

    call system_clock(start, count_rate)
    call f%load('canada.json')  
    call system_clock(finish)

    write(*,'(A30,1X,F7.4,1X,A)') 'json_fortran : ', (finish-start)/real(count_rate), ' seconds'

end program json_fortran_test