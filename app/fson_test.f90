! JW NOTE: I just cat'ed together all the FSON files
! and added a test program at the bottom. Don't @ me.
!
! used this commit: 16731d9b510b94a4e074a1fa7e35706f6045a85f


! Copyright (c) 2012 Joseph A. Levin
!
! Permission is hereby granted, free of charge, to any person obtaining a copy of this
! software and associated documentation files (the "Software"), to deal in the Software
! without restriction, including without limitation the rights to use, copy, modify, merge,
! publish, distribute, sublicense, and/or sell copies of the Software, and to permit 
! persons to whom the Software is furnished to do so, subject to the following conditions:
!
! The above copyright notice and this permission notice shall be included in all copies or 
! substantial portions of the Software.
!
! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, 
! INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
! PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE 
! LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT
! OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
! DEALINGS IN THE SOFTWARE.

!     
! File:   string.f95
! Author: josephalevin
!
! Created on March 7, 2012, 7:40 PM
!

module fson_string_m

    private

    public :: fson_string, fson_string_create, fson_string_destroy, fson_string_length, fson_string_append !, fson_string_clear 
    public :: fson_string_equals, fson_string_copy

    integer, parameter :: BLOCK_SIZE = 32

    type fson_string
        character (len = BLOCK_SIZE) :: chars
        integer :: index = 0
        type(fson_string), pointer :: next => null()
    end type fson_string

    interface fson_string_append
        module procedure append_chars, append_string
    end interface fson_string_append

    interface fson_string_copy
        module procedure copy_chars
    end interface fson_string_copy

    interface fson_string_equals
        module procedure equals_string
    end interface fson_string_equals
    
    interface fson_string_length
        module procedure string_length
    end interface fson_string_length

contains

    !
    ! FSON STRING CREATE
    !
    function fson_string_create(chars) result(new)
        character(len=*), optional :: chars
        type(fson_string), pointer :: new

        nullify(new)
        allocate(new)
        
        ! append chars if available
        if(present(chars)) then
            call append_chars(new, chars)
        end if

    end function fson_string_create
    
    !
    ! FSON STRING CREATE
    !
    recursive subroutine fson_string_destroy(this)

      implicit none
      type(fson_string), pointer :: this

      if (associated(this)) then

         if(associated(this % next)) then
            call fson_string_destroy(this % next)
         end if

         deallocate(this)
         nullify (this)

      end if

    end subroutine fson_string_destroy

    !
    ! ALLOCATE BLOCK
    !
    subroutine allocate_block(this)

      implicit none
      type(fson_string), pointer :: this
      type(fson_string), pointer :: new

      if (.not.associated(this % next)) then
         nullify(new)
         allocate(new)
         this % next => new
      end if

    end subroutine allocate_block


    !
    ! APPEND_STRING
    !
    subroutine append_string(str1, str2)
        type(fson_string), pointer :: str1, str2
        integer :: length, i

        length = string_length(str2)

        do i = 1, length
            call append_char(str1, get_char_at(str2, i))
        end do


    end subroutine append_string

    !
    ! APPEND_CHARS
    !
    subroutine append_chars(str, c)
        type(fson_string), pointer :: str
        character (len = *), intent(in) :: c
        integer length, i

        length = len(c)

        do i = 1, length
            call append_char(str, c(i:i))
        end do


    end subroutine append_chars

    !
    ! APPEND_CHAR
    !
    recursive subroutine append_char(str, c)
        type(fson_string), pointer :: str
        character, intent(in) :: c

        if (str % index .GE. BLOCK_SIZE) then
            !set down the chain
            call allocate_block(str)
            call append_char(str % next, c)

        else
            ! set local
            str % index = str % index + 1
            str % chars(str % index:str % index) = c
        end if

    end subroutine append_char

    !
    ! COPY CHARS
    !
    subroutine copy_chars(this, to)
        type(fson_string), pointer :: this
        character(len = *), intent(inout) :: to
        integer :: i, length

        length = min(string_length(this), len(to))

        do i = 1, length
            to(i:i) = get_char_at(this, i)
        end do

        ! pad with nothing
        do i = length + 1, len(to)
            to(i:i) = ""
        end do


    end subroutine copy_chars



    !
    ! CLEAR
    !
    recursive subroutine string_clear(this)
        type(fson_string), pointer :: this

        if (associated(this % next)) then
            call string_clear(this % next)
            deallocate(this % next)
            nullify (this % next)
        end if

        this % index = 0

    end subroutine string_clear

    !
    ! SIZE    
    !
    recursive integer function string_length(str) result(count)
        type(fson_string), pointer :: str

        count = str % index

        if (str % index == BLOCK_SIZE .AND. associated(str % next)) then
            count = count + string_length(str % next)
        end if

    end function string_length


    !
    ! GET CHAR AT
    !
    recursive character function get_char_at(this, i) result(c)
        type(fson_string), pointer :: this
        integer, intent(in) :: i

        if (i .LE. this % index) then
            c = this % chars(i:i)
        else
            c = get_char_at(this % next, i - this % index)
        end if

    end function get_char_at

    !
    ! EQUALS STRING
    !
    logical function equals_string(this, other) result(equals)
        type(fson_string), pointer :: this, other
        integer :: i
        equals = .false.
        
        if(fson_string_length(this) .ne. fson_string_length(other)) then
            equals = .false.
            return
        else if(fson_string_length(this) == 0) then
            equals = .true.
            return
        end if
        
        do i=1, fson_string_length(this)
            if(get_char_at(this, i) .ne. get_char_at(other, i)) then
                equals = .false.
                return
            end if
        end do
        
        equals = .true.
        
    end function equals_string

end module fson_string_m

! Copyright (c) 2012 Joseph A. Levin
!
! Permission is hereby granted, free of charge, to any person obtaining a copy of this
! software and associated documentation files (the "Software"), to deal in the Software
! without restriction, including without limitation the rights to use, copy, modify, merge,
! publish, distribute, sublicense, and/or sell copies of the Software, and to permit 
! persons to whom the Software is furnished to do so, subject to the following conditions:
!
! The above copyright notice and this permission notice shall be included in all copies or 
! substantial portions of the Software.
!
! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, 
! INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
! PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE 
! LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT
! OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
! DEALINGS IN THE SOFTWARE.

!     
! File:   value_m.f95
! Author: josephalevin
!
! Created on March 7, 2012, 10:14 PM
!

module fson_value_m

    use fson_string_m

    implicit none

    private

    public :: fson_value, fson_value_create, fson_value_destroy, fson_value_add, fson_value_get, fson_value_count, fson_value_print

    !constants for the value types
    integer, public, parameter :: TYPE_UNKNOWN = -1
    integer, public, parameter :: TYPE_NULL = 0
    integer, public, parameter :: TYPE_OBJECT = 1
    integer, public, parameter :: TYPE_ARRAY = 2
    integer, public, parameter :: TYPE_STRING = 3
    integer, public, parameter :: TYPE_INTEGER = 4
    integer, public, parameter :: TYPE_REAL = 5
    integer, public, parameter :: TYPE_LOGICAL = 6


    !
    ! FSON VALUE
    !
    type fson_value
        type(fson_string), pointer :: name => null()
        integer :: value_type = TYPE_UNKNOWN
        logical :: value_logical
        integer :: value_integer
        integer(kind = 8) :: value_long_integer
        real :: value_real
        double precision :: value_double
        integer, private :: count = 0
        type(fson_string), pointer :: value_string => null()
        type(fson_value), pointer :: next => null()
        type(fson_value), pointer :: parent => null()
        type(fson_value), pointer :: children => null()
        type(fson_value), pointer :: tail => null()
    end type fson_value

    !
    ! FSON VALUE GET
    !
    ! Use either a 1 based index or member name to get the value.
    interface fson_value_get
        module procedure get_by_index
        module procedure get_by_name_chars
        module procedure get_by_name_string
    end interface fson_value_get

contains

    !
    ! FSON VALUE CREATE
    !
    function fson_value_create() result(new)
        type(fson_value), pointer :: new

        nullify(new)
        allocate(new)

    end function fson_value_create

    !
    ! FSON VALUE DESTROY
    !
    recursive subroutine fson_value_destroy(this, destroy_next)

      implicit none
      type(fson_value), pointer :: this
      logical, intent(in), optional :: destroy_next

      type(fson_value), pointer :: p
      logical :: donext

      if (present(destroy_next)) then
         donext = destroy_next
      else
         donext = .true.
      end if

      if (associated(this)) then

         if(associated(this % name)) then
            call fson_string_destroy(this % name)
            nullify (this % name)
         end if

         if(associated(this % value_string)) then
            call fson_string_destroy(this % value_string)
            nullify (this % value_string)
         end if

         if(associated(this % children)) then
            do while (this % count > 0)
               p => this % children
               this % children => this % children % next
               this % count = this % count - 1
               call fson_value_destroy(p, .false.)
            end do
            nullify(this % children)
         end if

         if ((associated(this % next)) .and. (donext)) then
            call fson_value_destroy(this % next)
            nullify (this % next)
         end if

         if(associated(this % tail)) then
            nullify (this % tail)
         end if

         deallocate(this)
         nullify(this)

      end if

    end subroutine fson_value_destroy

    !
    ! FSON VALUE ADD
    !
    ! Adds the member to the linked list

    subroutine fson_value_add(this, member)

      implicit none
      type(fson_value), pointer :: this, member

      ! associate the parent
      member % parent => this

      ! add to linked list
      if (associated(this % children)) then
         this % tail % next => member
      else
         this % children => member
      end if

      this % tail => member
      this % count = this % count + 1

    end subroutine fson_value_add

    !
    ! FSON_VALUE_COUNT
    !
    integer function fson_value_count(this) result(count)
        type(fson_value), pointer :: this

        count = this % count

    end function

    !
    ! GET BY INDEX
    !
    function get_by_index(this, index) result(p)
        type(fson_value), pointer :: this, p
        integer, intent(in) :: index
        integer :: i

        p => this % children

        do i = 1, index - 1
            p => p % next
        end do

    end function get_by_index

    !
    ! GET BY NAME CHARS
    !
    function get_by_name_chars(this, name) result(p)
        type(fson_value), pointer :: this, p
        character(len=*), intent(in) :: name
        
        type(fson_string), pointer :: string
        
        ! convert the char array into a string
        string => fson_string_create(name)
        
        p => get_by_name_string(this, string)

        call fson_string_destroy(string)
        
    end function get_by_name_chars
    
    !
    ! GET BY NAME STRING
    !
    function get_by_name_string(this, name) result(p)
        type(fson_value), pointer :: this, p
        type(fson_string), pointer :: name
        integer :: i, count
        
        if(this % value_type .ne. TYPE_OBJECT) then
            nullify(p)
            return 
        end if

        count = fson_value_count(this)
        p => this%children
        do i = 1, count
           if (fson_string_equals(p%name, name)) then
              return
           end if
           p => p%next
        end do
        
        ! didn't find anything
        nullify(p)
        
    end function get_by_name_string
    
    !
    ! FSON VALUE PRINT
    !
    recursive subroutine fson_value_print(this, indent)
        type(fson_value), pointer :: this, element
        integer, optional, intent(in) :: indent
        character (len = 1024) :: tmp_chars
        integer :: tab, i, count, spaces
                
        if (present(indent)) then
            tab = indent
        else
            tab = 0
        end if
        
        spaces = tab * 2

        select case (this % value_type)
        case(TYPE_OBJECT)
            print *, repeat(" ", spaces), "{"
            count = fson_value_count(this)
            element => this%children
            do i = 1, count
               ! get the name
               call fson_string_copy(element % name, tmp_chars)
               ! print the name
               print *, repeat(" ", spaces), '"', trim(tmp_chars), '":'
               ! recursive print of the element
               call fson_value_print(element, tab + 1)
               ! print the separator if required
               if (i < count) then
                  print *, repeat(" ", spaces), ","
               end if
               element => element%next
            end do

            print *, repeat(" ", spaces), "}"
        case (TYPE_ARRAY)
            print *, repeat(" ", spaces), "["
            count = fson_value_count(this)
            element => this%children
            do i = 1, count
               ! recursive print of the element
               call fson_value_print(element, tab + 1)
               ! print the separator if required
               if (i < count) then
                  print *, ","
               end if
               element => element%next
            end do
            print *, repeat(" ", spaces), "]"
        case (TYPE_NULL)
            print *, repeat(" ", spaces), "null"
        case (TYPE_STRING)
            call fson_string_copy(this % value_string, tmp_chars)
            print *, repeat(" ", spaces), '"', trim(tmp_chars), '"'
        case (TYPE_LOGICAL)
            if (this % value_logical) then
                print *, repeat(" ", spaces), "true"
            else
                print *, repeat(" ", spaces), "false"
            end if
        case (TYPE_INTEGER)
            print *, repeat(" ", spaces), this % value_long_integer
        case (TYPE_REAL)
            print *, repeat(" ", spaces), this % value_double
        end select
    end subroutine fson_value_print
       

end module fson_value_m


! Copyright (c) 2012 Joseph A. Levin
!
! Permission is hereby granted, free of charge, to any person obtaining a copy of this
! software and associated documentation files (the "Software"), to deal in the Software
! without restriction, including without limitation the rights to use, copy, modify, merge,
! publish, distribute, sublicense, and/or sell copies of the Software, and to permit 
! persons to whom the Software is furnished to do so, subject to the following conditions:
!
! The above copyright notice and this permission notice shall be included in all copies or 
! substantial portions of the Software.
!
! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, 
! INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
! PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE 
! LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT
! OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
! DEALINGS IN THE SOFTWARE.


! FSON MODULE 
!
! File:   fson.f95
! Author: Joseph A. Levin
!
! Created on March 6, 2012, 7:48 PM
!


! Copyright (c) 2012 Joseph A. Levin
!
! Permission is hereby granted, free of charge, to any person obtaining a copy of this
! software and associated documentation files (the "Software"), to deal in the Software
! without restriction, including without limitation the rights to use, copy, modify, merge,
! publish, distribute, sublicense, and/or sell copies of the Software, and to permit 
! persons to whom the Software is furnished to do so, subject to the following conditions:
!
! The above copyright notice and this permission notice shall be included in all copies or 
! substantial portions of the Software.
!
! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, 
! INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
! PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE 
! LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT
! OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
! DEALINGS IN THE SOFTWARE.

!     
! File:   fson_path_m.f95
! Author: Joseph A. Levin
!
! Created on March 10, 2012, 11:01 PM
!

module fson_path_m
    
    use fson_value_m 
    use fson_string_m

    private
    
    public :: fson_path_get
    
    interface fson_path_get
        module procedure get_by_path
        module procedure get_integer
        module procedure get_long_integer
        module procedure get_real
        module procedure get_double
        module procedure get_logical
        module procedure get_chars
        module procedure get_array_1d_integer
        module procedure get_array_2d_integer
        module procedure get_array_1d_real
        module procedure get_array_2d_real
        module procedure get_array_1d_double
        module procedure get_array_2d_double
        module procedure get_array_1d_logical
        module procedure get_array_2d_logical
        module procedure get_array_1d_char
        module procedure get_array_2d_char
    end interface fson_path_get

    abstract interface

       subroutine array_callback_1d(element, i, count)
         use fson_value_m
         implicit none
         type(fson_value), pointer,intent(in) :: element
         integer, intent(in) :: i        ! index
         integer, intent(in) :: count    ! size of array
       end subroutine array_callback_1d

       subroutine array_callback_2d(element, i1, i2, count1, count2)
         use fson_value_m
         implicit none
         type(fson_value), pointer,intent(in) :: element
         integer, intent(in) :: i1, i2
         integer, intent(in) :: count1, count2
       end subroutine array_callback_2d

    end interface

contains
    !
    ! GET BY PATH
    !
    ! $     = root 
    ! @     = this
    ! .     = child object member
    ! []    = child array element
    !
    recursive subroutine get_by_path(this, path, p)
        type(fson_value), pointer :: this, p        
        character(len=*) :: path
        integer :: i, length, child_i
        character :: c
        logical :: array        
                
        ! default to assuming relative to this
        p => this
        
        child_i = 1          
        
        array = .false.
        
        length = len_trim(path)
        
        do i=1, length
            c = path(i:i)    
            select case (c)
                case ("$")
                    ! root
                    do while (associated (p % parent))
                        p => p % parent
                    end do
                    child_i = i + 1
                case ("@")
                    ! this                    
                    p => this
                    child_i = i + 1
                case (".", "[")                    
                    ! get child member from p                          
                    if (child_i < i) then                          
                        p => fson_value_get(p, path(child_i:i-1))
                    else
                        child_i = i + 1
                        cycle
                    end if
                    
                    if(.not.associated(p)) then
                        return                                        
                    end if
                    
                    child_i = i+1
                    
                    ! check if this is an array
                    ! if so set the array flag
                    if (c == "[") then
                        ! start looking for the array element index
                        array = .true.
                    end if
                case ("]")
                    if (.not.array) then
                        print *, "ERROR: Unexpected ], not missing preceding ["
                        call exit(1)
                    end if
                    array = .false.
                    child_i = parse_integer(path(child_i:i-1))                                                
                    p => fson_value_get(p, child_i)                                                                                                                    
                    
                    child_i= i + 1                                     
            end select            
        end do
                
        ! grab the last child if present in the path
        if (child_i <= length) then            
            p => fson_value_get(p, path(child_i:length))
            if(.not.associated(p)) then
                return
            else                
            end if
        end if
                
        
    end subroutine get_by_path
    
    !
    ! PARSE INTEGER
    !
    integer function parse_integer(chars) result(integral)
        character(len=*) :: chars
        character :: c
        integer :: tmp, i
                
        integral = 0        
        do i=1, len_trim(chars)
            c = chars(i:i)            
            select case(c)
                case ("0":"9")
                    ! digit        
                    read (c, '(i1)') tmp                                               
                    
                    ! shift
                    if(i > 1) then
                        integral = integral * 10
                    end if
                    ! add
                    integral = integral + tmp
                                                    
                case default                          
                    return
            end select            
        end do
    
    end function parse_integer    
    
    !
    ! GET INTEGER
    !
    subroutine get_integer(this, path, value)
        type(fson_value), pointer :: this, p
        character(len=*), optional :: path
        integer :: value        
        
        
        nullify(p)                
        if(present(path)) then
            call get_by_path(this=this, path=path, p=p)
        else
            p => this
        end if
        
        if(.not.associated(p)) then
            print *, "Unable to resolve path: ", path
            call exit(1)
        end if
                
        
        if(p % value_type == TYPE_INTEGER) then            
            value = p % value_integer
        else if (p % value_type == TYPE_REAL) then
            value = int(p % value_real)
        else if (p % value_type == TYPE_LOGICAL) then
            if (p % value_logical) then
                value = 1
            else
                value = 0
            end if
        else
            print *, "Unable to resolve value to integer: ", path
            call exit(1)
        end if
        
    end subroutine get_integer
    
    !
    ! GET LONG INTEGER
    !
    subroutine get_long_integer(this, path, value)
      type(fson_value), pointer :: this, p
      character(len=*), optional :: path
      integer(kind = 8) :: value

      nullify(p)
      if(present(path)) then
         call get_by_path(this=this, path=path, p=p)
      else
         p => this
      end if

      if(.not.associated(p)) then
         print *, "Unable to resolve path: ", path
         call exit(1)
      end if

      if(p % value_type == TYPE_INTEGER) then
         value = p % value_long_integer
      else if (p % value_type == TYPE_REAL) then
         value = int(p % value_real, kind = 8)
      else if (p % value_type == TYPE_LOGICAL) then
         if (p % value_logical) then
            value = 1
         else
            value = 0
         end if
      else
         print *, "Unable to resolve value to long integer: ", path
         call exit(1)
      end if

    end subroutine get_long_integer

    !
    ! GET REAL
    !
    subroutine get_real(this, path, value)
        type(fson_value), pointer :: this, p
        character(len=*), optional :: path
        real :: value        
        
        
        nullify(p)                
        
        if(present(path)) then
            call get_by_path(this=this, path=path, p=p)
        else
            p => this
        end if
        
        if(.not.associated(p)) then
            print *, "Unable to resolve path: ", path
            call exit(1)
        end if
                
        
        if(p % value_type == TYPE_INTEGER) then            
            value = real(p % value_long_integer)
        else if (p % value_type == TYPE_REAL) then
            value = p % value_real
        else if (p % value_type == TYPE_LOGICAL) then
            if (p % value_logical) then
                value = 1
            else
                value = 0
            end if
        else
            print *, "Unable to resolve value to real: ", path
            call exit(1)
        end if
        
    end subroutine get_real
    
    !
    ! GET DOUBLE
    !
    subroutine get_double(this, path, value)
        type(fson_value), pointer :: this, p
        character(len=*), optional :: path
        double precision :: value        
        
        
        nullify(p)                
        
        if(present(path)) then
            call get_by_path(this=this, path=path, p=p)
        else
            p => this
        end if
        
        if(.not.associated(p)) then
            print *, "Unable to resolve path: ", path
            call exit(1)
        end if
                
        
        if(p % value_type == TYPE_INTEGER) then            
            value = p % value_long_integer
        else if (p % value_type == TYPE_REAL) then
            value = p % value_double
        else if (p % value_type == TYPE_LOGICAL) then
            if (p % value_logical) then
                value = 1
            else
                value = 0
            end if
        else
            print *, "Unable to resolve value to double: ", path
            call exit(1)
        end if
        
    end subroutine get_double
    
    
    !
    ! GET LOGICAL
    !
    subroutine get_logical(this, path, value)
        type(fson_value), pointer :: this, p
        character(len=*), optional :: path
        logical :: value        
        
        
        nullify(p)                
        
        if(present(path)) then
            call get_by_path(this=this, path=path, p=p)
        else
            p => this
        end if
        
        if(.not.associated(p)) then
            print *, "Unable to resolve path: ", path
            call exit(1)
        end if
                
        
        if(p % value_type == TYPE_INTEGER) then            
            value = (p % value_long_integer > 0)
        else if (p % value_type == TYPE_LOGICAL) then
            value = p % value_logical
        else
            print *, "Unable to resolve value to real: ", path
            call exit(1)
        end if
        
    end subroutine get_logical
    
    !
    ! GET CHARS
    !
    subroutine get_chars(this, path, value)
        type(fson_value), pointer :: this, p
        character(len=*), optional :: path
        character(len=*) :: value  
        
        nullify(p)                
        
        if(present(path)) then
            call get_by_path(this=this, path=path, p=p)
        else
            p => this
        end if
        
        if(.not.associated(p)) then
            print *, "Unable to resolve path: ", path
            call exit(1)
        end if
                
        
        if(p % value_type == TYPE_STRING) then            
            call fson_string_copy(p % value_string, value)          
        else
            print *, "Unable to resolve value to characters: ", path
            call exit(1)
        end if
        
    end subroutine get_chars
    
    !
    ! GET ARRAY 1D
    !
    
    subroutine get_array_1d(this, path, array_callback)
        type(fson_value), pointer :: this
        character(len = *), optional :: path
        procedure(array_callback_1d) :: array_callback

        type(fson_value), pointer :: p, element
        integer :: index, count
                
        nullify(p)                
        
        ! resolve the path to the value
        if(present(path)) then
            call get_by_path(this=this, path=path, p=p)
        else
            p => this
        end if
            
        if(.not.associated(p)) then
            print *, "Unable to resolve path: ", path
            call exit(1)
        end if
        
        if(p % value_type == TYPE_ARRAY) then            
            count = fson_value_count(p)
            element => p % children
            do index = 1, count
                call array_callback(element, index, count)
                element => element % next
            end do
        else
            print *, "Resolved value is not an array. ", path
            call exit(1)
        end if

        if (associated(p)) nullify(p)

      end subroutine get_array_1d

!
! GET ARRAY INTEGER 1D
!
    subroutine get_array_1d_integer(this, path, arr)

      implicit none
      type(fson_value), pointer, intent(in) :: this
      character(len=*), intent(in), optional :: path   
      integer, allocatable, intent(out) :: arr(:)

      if (allocated(arr)) deallocate(arr)
      call get_array_1d(this, path, array_callback_1d_integer)

    contains

      subroutine array_callback_1d_integer(element, i, count)
        implicit none
        type(fson_value), pointer, intent(in) :: element
        integer, intent(in) :: i, count
        if (.not. allocated(arr)) allocate(arr(count))
        call fson_path_get(element, "", arr(i))
      end subroutine array_callback_1d_integer

    end subroutine get_array_1d_integer

!
! GET ARRAY REAL 1D
!
    subroutine get_array_1d_real(this, path, arr)

      implicit none
      type(fson_value), pointer, intent(in) :: this
      character(len=*), intent(in), optional :: path   
      real, allocatable, intent(out) :: arr(:)

      if (allocated(arr)) deallocate(arr)
      call get_array_1d(this, path, array_callback_1d_real)

    contains

      subroutine array_callback_1d_real(element, i, count)
        implicit none
        type(fson_value), pointer, intent(in) :: element
        integer, intent(in) :: i, count
        if (.not. allocated(arr)) allocate(arr(count))
        call fson_path_get(element, "", arr(i))
      end subroutine array_callback_1d_real

    end subroutine get_array_1d_real

!
! GET ARRAY DOUBLE 1D
!
    subroutine get_array_1d_double(this, path, arr)

      implicit none
      type(fson_value), pointer, intent(in) :: this
      character(len=*), intent(in), optional :: path   
      double precision, allocatable, intent(out) :: arr(:)

      if (allocated(arr)) deallocate(arr)
      call get_array_1d(this, path, array_callback_1d_double)

    contains

      subroutine array_callback_1d_double(element, i, count)
        implicit none
        type(fson_value), pointer, intent(in) :: element
        integer, intent(in) :: i, count
        if (.not. allocated(arr)) allocate(arr(count))
        call fson_path_get(element, "", arr(i))
      end subroutine array_callback_1d_double

    end subroutine get_array_1d_double

!
! GET ARRAY LOGICAL 1D
!
    subroutine get_array_1d_logical(this, path, arr)

      implicit none
      type(fson_value), pointer, intent(in) :: this
      character(len=*), intent(in), optional :: path   
      logical, allocatable, intent(out) :: arr(:)

      if (allocated(arr)) deallocate(arr)
      call get_array_1d(this, path, array_callback_1d_logical)

    contains

      subroutine array_callback_1d_logical(element, i, count)
        implicit none
        type(fson_value), pointer, intent(in) :: element
        integer, intent(in) :: i, count
        if (.not. allocated(arr)) allocate(arr(count))
        call fson_path_get(element, "", arr(i))
      end subroutine array_callback_1d_logical

    end subroutine get_array_1d_logical

!
! GET ARRAY CHAR 1D
!
    subroutine get_array_1d_char(this, path, arr)

      implicit none
      type(fson_value), pointer, intent(in) :: this
      character(len=*), intent(in), optional :: path
      character(len = *), allocatable, intent(out) :: arr(:)

      if (allocated(arr)) deallocate(arr)
      call get_array_1d(this, path, array_callback_1d_char)

    contains

      subroutine array_callback_1d_char(element, i, count)
        implicit none
        type(fson_value), pointer, intent(in) :: element
        integer, intent(in) :: i, count
        if (.not. allocated(arr)) allocate(arr(count))
        call fson_path_get(element, "", arr(i))
      end subroutine array_callback_1d_char

    end subroutine get_array_1d_char


    !
    ! GET ARRAY 2D
    !
    
    subroutine get_array_2d(this, path, array_callback)
        type(fson_value), pointer :: this
        character(len = *), optional :: path
        procedure(array_callback_2d) :: array_callback

        type(fson_value), pointer :: p, element, item
        integer :: i1, i2, count1, count2, c
                
        nullify(p)                
        
        ! resolve the path to the value
        if(present(path)) then
            call get_by_path(this=this, path=path, p=p)
        else
            p => this
        end if
            
        if(.not.associated(p)) then
            print *, "Unable to resolve path: ", path
            call exit(1)
        end if
        
        if(p % value_type == TYPE_ARRAY) then            
            count1 = fson_value_count(p)
            element => p % children
            do i1 = 1, count1
               if (element % value_type == TYPE_ARRAY) then
                  c = fson_value_count(element)
                  if (i1 == 1) then
                     count2 = c
                  else if (c /= count2) then
                     print *, "Resolved value has the wrong number of elements. ", &
                          path, "[", i1, "]"
                     call exit(1)
                  end if
                  item => element % children
                  do i2 = 1, count2
                     call array_callback(item, i1, i2, count1, count2)
                     item => item % next
                  end do
                  element => element % next
               else
                  print *, "Resolved value is not an array. ", path, "[", i1, "]"
                  call exit(1)
               end if
            end do
        else
            print *, "Resolved value is not an array. ", path
            call exit(1)
        end if

        if (associated(p)) nullify(p)

      end subroutine get_array_2d

!
! GET ARRAY INTEGER 2D
!
    subroutine get_array_2d_integer(this, path, arr)

      implicit none
      type(fson_value), pointer, intent(in) :: this
      character(len=*), intent(in), optional :: path   
      integer, allocatable, intent(out) :: arr(:, :)

      if (allocated(arr)) deallocate(arr)
      call get_array_2d(this, path, array_callback_2d_integer)

    contains

      subroutine array_callback_2d_integer(element, i1, i2, count1, count2)
        implicit none
        type(fson_value), pointer, intent(in) :: element
        integer, intent(in) :: i1, i2, count1, count2
        if (.not. allocated(arr)) allocate(arr(count1, count2))
        call fson_path_get(element, "", arr(i1, i2))
      end subroutine array_callback_2d_integer

    end subroutine get_array_2d_integer

!
! GET ARRAY REAL 2D
!
    subroutine get_array_2d_real(this, path, arr)

      implicit none
      type(fson_value), pointer, intent(in) :: this
      character(len=*), intent(in), optional :: path   
      real, allocatable, intent(out) :: arr(:, :)

      if (allocated(arr)) deallocate(arr)
      call get_array_2d(this, path, array_callback_2d_real)

    contains

      subroutine array_callback_2d_real(element, i1, i2, count1, count2)
        implicit none
        type(fson_value), pointer, intent(in) :: element
        integer, intent(in) :: i1, i2, count1, count2
        if (.not. allocated(arr)) allocate(arr(count1, count2))
        call fson_path_get(element, "", arr(i1, i2))
      end subroutine array_callback_2d_real

    end subroutine get_array_2d_real

!
! GET ARRAY DOUBLE 2D
!
    subroutine get_array_2d_double(this, path, arr)

      implicit none
      type(fson_value), pointer, intent(in) :: this
      character(len=*), intent(in), optional :: path   
      double precision, allocatable, intent(out) :: arr(:, :)

      if (allocated(arr)) deallocate(arr)
      call get_array_2d(this, path, array_callback_2d_double)

    contains

      subroutine array_callback_2d_double(element, i1, i2, count1, count2)
        implicit none
        type(fson_value), pointer, intent(in) :: element
        integer, intent(in) :: i1, i2, count1, count2
        if (.not. allocated(arr)) allocate(arr(count1, count2))
        call fson_path_get(element, "", arr(i1, i2))
      end subroutine array_callback_2d_double

    end subroutine get_array_2d_double

!
! GET ARRAY LOGICAL 2D
!
    subroutine get_array_2d_logical(this, path, arr)

      implicit none
      type(fson_value), pointer, intent(in) :: this
      character(len=*), intent(in), optional :: path   
      logical, allocatable, intent(out) :: arr(:, :)

      if (allocated(arr)) deallocate(arr)
      call get_array_2d(this, path, array_callback_2d_logical)

    contains

      subroutine array_callback_2d_logical(element, i1, i2, count1, count2)
        implicit none
        type(fson_value), pointer, intent(in) :: element
        integer, intent(in) :: i1, i2, count1, count2
        if (.not. allocated(arr)) allocate(arr(count1, count2))
        call fson_path_get(element, "", arr(i1, i2))
      end subroutine array_callback_2d_logical

    end subroutine get_array_2d_logical

!
! GET ARRAY CHAR 2D
!
    subroutine get_array_2d_char(this, path, arr)

      implicit none
      type(fson_value), pointer, intent(in) :: this
      character(len=*), intent(in), optional :: path
      character(len = *), allocatable, intent(out) :: arr(:, :)

      if (allocated(arr)) deallocate(arr)
      call get_array_2d(this, path, array_callback_2d_char)

    contains

      subroutine array_callback_2d_char(element, i1, i2, count1, count2)
        implicit none
        type(fson_value), pointer, intent(in) :: element
        integer, intent(in) :: i1, i2, count1, count2
        if (.not. allocated(arr)) allocate(arr(count1, count2))
        call fson_path_get(element, "", arr(i1, i2))
      end subroutine array_callback_2d_char

    end subroutine get_array_2d_char

end module fson_path_m




module fson
    use fson_value_m, fson_print => fson_value_print, fson_destroy => fson_value_destroy
    use fson_string_m
    use fson_path_m, fson_get => fson_path_get

    implicit none

    private

    public :: fson_parse, fson_value, fson_get, fson_print, fson_destroy

    ! FILE IOSTAT CODES
    integer, parameter :: end_of_file = -1
    integer, parameter :: end_of_record = -2

    ! PARSING STATES
    integer, parameter :: STATE_LOOKING_FOR_VALUE = 1
    integer, parameter :: STATE_IN_OBJECT = 2
    integer, parameter :: STATE_IN_PAIR_NAME = 3
    integer, parameter :: STATE_IN_PAIR_VALUE = 4

    ! POP/PUSH CHARACTER
    integer :: pushed_index = 0
    character (len = 10) :: pushed_char

contains

    !
    ! FSON PARSE
    !
    function fson_parse(file, unit, str) result(p)
        type(fson_value), pointer :: p
        integer, optional, intent(inout) :: unit
        character(len = *), optional, intent(in) :: file
        character(len = *), optional, intent(in) :: str
        character(len=:),allocatable :: strBuffer
        logical :: unit_available
        integer :: u
        ! init the pointer to null
        nullify(p)

        ! select the file unit to use
        if (present(unit) .and. present(file)) then
            u = unit
        elseif (present(file)) then
            ! find the first available unit
            unit_available = .false.
            u = 20

            do while (.not.unit_available)
                inquire(unit = u, exist = unit_available)
                u = u + 1
            end do
        elseif (present(str)) then
            strBuffer = str
            u = 0
        else 
            print *, "ERROR: Need a file or a string"
            call exit (1)
        end if

        ! open the file
        if (present(file)) then
            open (unit = u, file = file, status = "old", action = "read", form = "formatted", position = "rewind")
        end if

        ! create the value and associate the pointer        
        p => fson_value_create()

        ! parse as a value
        call parse_value(unit = u, value = p, str = strBuffer)

        ! close the file
        if( .not. present(unit)) then
            close (u)
        end if

        if(allocated(strBuffer)) deallocate(strBuffer)

    end function fson_parse

    !
    ! PARSE_VALUE
    !
    recursive subroutine parse_value(unit, str, value)
        integer, intent(inout) :: unit
        character(*), intent(inout) :: str
        type(fson_value), pointer :: value
        logical :: eof
        character :: c

        ! pop the next non whitespace character off the file
        c = pop_char(unit, str, eof = eof, skip_ws = .true.)

        if (eof) then
            return
        else
            select case (c)
            case ("{")
                ! start object                
                value % value_type = TYPE_OBJECT
                call parse_object(unit, str, value)
            case ("[")
                ! start array
                value % value_type = TYPE_ARRAY
                call parse_array(unit, str, value)
            case ("]")
                ! end an empty array
               call push_char(c)
               nullify(value)
            case ('"')
                ! string                                      
                value % value_type = TYPE_STRING
                value % value_string => parse_string(unit, str)
            case ("t")
                !true
                value % value_type = TYPE_LOGICAL
                call parse_for_chars(unit, str, "rue")
                value % value_logical = .true.
            case ("f")
                !false
                value % value_type = TYPE_LOGICAL
                value % value_logical = .false.
                call parse_for_chars(unit, str, "alse")
            case ("n")
                value % value_type = TYPE_NULL
                call parse_for_chars(unit, str, "ull")
            case("-", "0": "9")
                call push_char(c)
                call parse_number(unit, str, value)
            case default
                print *, "ERROR: Unexpected character while parsing value. '", c, "' ASCII=", iachar(c)
                call exit (1)
            end select
        end if

    end subroutine parse_value

    !
    ! PARSE OBJECT
    !    
    recursive subroutine parse_object(unit, str, parent)
        integer, intent(inout) :: unit
        character(*), intent(inout) :: str
        type(fson_value), pointer :: parent, pair


        logical :: eof
        character :: c

        ! pair name
        c = pop_char(unit, str, eof = eof, skip_ws = .true.)
        if (eof) then
            print *, "ERROR: Unexpected end of file while parsing start of object."
            call exit (1)
        else if ("}" == c) then
            ! end of an empty object
            return
        else if ('"' == c) then
            pair => fson_value_create()
            pair % name => parse_string(unit, str)
        else
            print *, "ERROR: Expecting string: '", c, "'"
            call exit (1)
        end if

        ! pair value
        c = pop_char(unit, str, eof = eof, skip_ws = .true.)
        if (eof) then
            print *, "ERROR: Unexpected end of file while parsing object member. 1"
            call exit (1)
        else if (":" == c) then
            ! parse the value                       
            call parse_value(unit, str, pair)
            call fson_value_add(parent, pair)
        else
            print *, "ERROR: Expecting : and then a value. ", c
            call exit (1)
        end if

        ! another possible pair
        c = pop_char(unit, str, eof = eof, skip_ws = .true.)
        if (eof) then
            return
        else if ("," == c) then
            ! read the next member            
            call parse_object(unit = unit, str=str, parent = parent)
        else if ("}" == c) then
            return
        else
            print *, "ERROR: Expecting end of object.", c
            call exit (1)
        end if

    end subroutine parse_object

    !
    ! PARSE ARRAY
    !    
    recursive subroutine parse_array(unit, str, array)

      implicit none
      integer, intent(inout) :: unit
      character(*), intent(inout) :: str
      type(fson_value), pointer :: array, element

      logical :: eof, finished
      character :: c

      finished = .false.
      do while (.not. finished)

         ! try to parse an element value
         element => fson_value_create()
         call parse_value(unit, str, element)

         ! parse value will disassociate an empty array value
         if (associated(element)) then
            call fson_value_add(array, element)
         end if

         ! pop the next character
         c = pop_char(unit, str, eof = eof, skip_ws = .true.)

         if (eof) then
            finished = .true.
         else if ("]" == c) then
            ! end of array
            finished = .true.
         end if

      end do

    end subroutine parse_array

    !
    ! PARSE STRING
    !
    function parse_string(unit, str) result(string)
        integer, intent(inout) :: unit
        character(*), intent(inout) :: str
        type(fson_string), pointer :: string

        logical :: eof, escape
        character :: c

        string => fson_string_create()
        escape = .false.

        do
            c = pop_char(unit, str, eof = eof, skip_ws = .false.)
            if (eof) then
               print *, "Expecting end of string"
               call exit(1)
            else if (escape) then
              call fson_string_append(string,c)
              escape = .false.
            else
               if (c == '\') then
                  escape = .true.
               else if (c == '"') then
                  exit
               else
                  call fson_string_append(string,c)
               end if
            end if
        end do
    end function parse_string

    !
    ! PARSE FOR CHARACTERS
    !
    subroutine parse_for_chars(unit, str, chars)
        integer, intent(in) :: unit
        character(*), intent(inout) :: str
        character(len = *), intent(in) :: chars
        integer :: i, length
        logical :: eof
        character :: c

        length = len_trim(chars)

        do i = 1, length
            c = pop_char(unit, str, eof = eof, skip_ws = .true.)
            if (eof) then
                print *, "ERROR: Unexpected end of file while parsing array."
                call exit (1)
            else if (c .ne. chars(i:i)) then
                print *, "ERROR: Unexpected character.'", c,"'", chars(i:i)
                call exit (1)
            end if
        end do

    end subroutine parse_for_chars

    !
    ! PARSE NUMBER
    !
    subroutine parse_number(unit, str, value)
        integer, intent(inout) :: unit
        character(*), intent(inout) :: str
        type(fson_value), pointer :: value
        logical :: eof, negative, decimal, scientific
        character :: c
        integer :: exp, digit_count
        integer(kind=8) :: integral
        double precision :: frac


        ! first character is either - or a digit        
        c = pop_char(unit, str, eof = eof, skip_ws = .true.)
        if (eof) then
            print *, "ERROR: Unexpected end of file while parsing number."
            call exit (1)
        else if ("-" == c) then
            negative = .true.
        else
            negative = .false.
            call push_char(c)
        end if


        ! parse the integral
        integral = parse_integer(unit, str)

        decimal = .false.
        scientific = .false.
        exp = 0
        frac = 0.0d0

        do
            ! first character is either - or a digit        
            c = pop_char(unit, str, eof = eof, skip_ws = .true.)
            if (eof) then
                print *, "ERROR: Unexpected end of file while parsing number."
                call exit (1)
            else
                select case (c)
                case (".")
                    ! this is already fractional number
                    if (decimal) then
                        ! already found a decimal place
                        print *, "ERROR: Unexpected second decimal place while parsing number."
                        call exit(1)
                    end if
                    decimal = .true.
                    frac = parse_integer(unit, str, digit_count, allow_truncate = .true.)
                    frac = frac / (10.0d0 ** digit_count)
                case ("e", "E")
                    ! this is already an exponent number
                    if (scientific) then
                        ! already found a e place
                        print *, "ERROR: Unexpected second exponent while parsing number."
                        call exit(1)
                    end if
                    scientific = .true.
                    decimal = .true.
                    ! this number has an exponent
                    exp = int(parse_integer(unit, str), kind = 4)
                case default
                    if (decimal) then
                        ! add the integral
                        frac = frac + integral
                        if (scientific) then
                            ! apply exponent
                            frac = frac * (10.0d0 ** exp)
                        end if
                        ! apply negative
                        if (negative) then
                            frac = -frac
                        end if
                        value % value_type = TYPE_REAL
                        value % value_real = real(frac)
                        value % value_double = frac
                    else
                       if (negative) then
                          ! apply negative
                          integral = -integral
                       end if
                       value % value_type = TYPE_INTEGER
                       value % value_integer = int(integral, kind = 4)
                       value % value_long_integer = integral
                    end if
                    call push_char(c)
                    exit
                end select
            end if
        end do

    end subroutine

    !
    ! PARSE INTEGER    
    !
    integer(kind=8) function parse_integer(unit, str, digit_count, allow_truncate) &
         result(integral)
        integer, intent(in) :: unit
        character(*), intent(inout) :: str
        integer, optional, intent(out) :: digit_count
        logical, optional, intent(in) :: allow_truncate
        logical :: eof, found_sign, found_digit
        character :: c
        integer :: tmp, icount, isign
        logical :: do_truncate, truncating
        integer, parameter :: max_integer_length = 18

        if (present(allow_truncate)) then
           do_truncate = allow_truncate
        else
           do_truncate = .false.
        end if

        icount = 0
        integral = 0
        isign = 1
        found_sign = .false.
        found_digit = .false.
        truncating = .false.
        do
            c = pop_char(unit, str, eof = eof, skip_ws = .true.)
            if (eof) then
                print *, "ERROR: Unexpected end of file while parsing digit."
                call exit (1)
            else
                select case(c)
                case ("+")
                    if (found_sign.or.found_digit) then
                        print *, "ERROR: Misformatted number."
                        call exit(1)
                    end if
                    found_sign = .true.
                case ("-")
                    if (found_sign.or.found_digit) then
                        print *, "ERROR: Misformatted number."
                        call exit(1)
                    end if
                    found_sign = .true.
                    isign = -1
                case ("0":"9")
                    found_sign = .true.
                    if ((icount > max_integer_length) .and. (.not. truncating)) then
                       if (do_truncate) then
                          truncating = .true.
                       else
                          print *, "ERROR: Too many digits for an integer."
                          call exit(1)
                       end if
                    end if
                    ! digit        
                    read (c, '(i1)') tmp
                    ! shift
                    if (.not. truncating) then
                       if (icount > 0) then
                          integral = integral * 10
                       end if
                       ! add
                       integral = integral + tmp
                       ! increase the icount
                       icount = icount + 1
                    end if

                case default
                    if (present(digit_count)) then
                        digit_count = icount
                    end if
                    call push_char(c)
                    integral = isign * integral
                    return
                end select
            end if
        end do

    end function parse_integer

    !
    ! POP CHAR
    !
    recursive character function pop_char(unit, str, eof, skip_ws) result(popped)
        integer, intent(in) :: unit
        character(*), intent(inout) :: str
        logical, intent(out) :: eof
        logical, intent(in), optional :: skip_ws

        integer :: ios
        character :: c
        logical :: ignore

        eof = .false.
        if (.not.present(skip_ws)) then
            ignore = .false.
        else
            ignore = skip_ws
        end if

        do
            if (pushed_index > 0) then
                ! there is a character pushed back on, most likely from the number parsing                
                c = pushed_char(pushed_index:pushed_index)
                pushed_index = pushed_index - 1
                ios = 0
            else
                if (unit .gt. 0) then
                    read (unit = unit, fmt = "(a)", advance = "no", iostat = ios) c
                else
                    read (unit = str, fmt = "(a)", iostat = ios) c
                    str = str(2:)
                endif
            end if
            if (ios == end_of_record) then
                cycle            
            else if (ios == end_of_file) then
                eof = .true.
                exit
            else if (iachar(c) <= 31) then
                ! non printing ascii characters
                cycle
            else if (ignore .and. c == " ") then
                cycle
            else
                popped = c
                exit
            end if
        end do

    end function pop_char

    !
    ! PUSH CHAR
    !
    subroutine push_char(c)
        character, intent(inout) :: c
        pushed_index = pushed_index + 1
        pushed_char(pushed_index:pushed_index) = c

    end subroutine push_char

end module fson

program fson_test

    use fson
    use iso_fortran_env, only: int64,real64

    type(fson_value), pointer :: value
    integer(int64) :: start, finish, count_rate

    call system_clock(start, count_rate)
    value => fson_parse('canada.json')
    call system_clock(finish)

    write(*,'(A30,1X,F7.4,1X,A)') 'fson : ', (finish-start)/real(count_rate,real64), ' seconds'

end program fson_test
