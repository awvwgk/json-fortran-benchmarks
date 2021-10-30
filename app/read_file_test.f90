


program read_file_test
    use iso_fortran_env, only: int64,real64

    !! Reads the contents of the file into an allocatable string str.

    implicit none

    character(len=*),parameter :: filename = 'canada.json'

    character(len=:),allocatable :: str

    integer :: iunit,istat,filesize

    integer(int64) :: start, finish, count_rate

    call system_clock(start, count_rate)

    open( newunit = iunit,&
          file    = filename,&
          status  = 'OLD',&
          form    = 'UNFORMATTED',&
          access  = 'STREAM',&
          iostat  = istat )

    if (istat==0) then
        inquire(file=filename, size=filesize)
        if (filesize>0) then
            allocate( character(len=filesize) :: str )
            read(iunit,pos=1,iostat=istat) str
            if (istat/=0) deallocate(str)
            close(iunit, iostat=istat)
        end if
    end if

    call system_clock(finish)

    write(*,'(A30,1X,F7.4,1X,A)') 'read file to a string : ', (finish-start)/real(count_rate,real64), ' seconds'

end program read_file_test