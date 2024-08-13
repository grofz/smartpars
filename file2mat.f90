! ==============================================
! (c) 2024 Z. Grof (UCT Prague) <grofz@vscht.cz>
! ==============================================



! -----------------------------------------------------------------------------
! file2mat.f90:
!
! Utilities to produce or modify arrays of strings (string_t)
!
!   function read_matrix(file, cols, separator) result(arr)
!     --> file: name of the file
!     --> cols: array of integers, which columns are selected
!     --> separator: what is the delimiter (optional argument)
!   <-- rank 2 array of string_t
!
!   subroutine split(str, separator, items)
!     --> character variable
!     --> delimiter (separator)
!     <-- rank 1 array of string_t
!
!   subroutine remove_empty_strings(items)
!     <-> rank 1 array of strings, size can be changed on return
!
!   subroutine adjust_strings(items)
!     <-> rank 1 array of strings (leading and trailing blanks removed)
!
!   function read_strings(file) result(strings)
!     --> file: name of file
!   <-- rank 1 array of strings (lines from the file)
!
!   subroutine read_line_from_file(fid, line, was_eof)
!     --> fid: id of an opened file
!     <-- line: character(len=:), allocatable - line from file
!     <-- is_eof (optional) - last line was read
!
!
! Methods of string_t:
!   % to_number(allow_nan) - elemental function
!       --> allow_nan (optional) - Program will stop if value can not
!           be converted to number and allow_nan = .false. Conversion
!           errors are silently ignored otherwise.
!
! Here is an example how to get an array of numbers from the file.
!
!   use file2mat_mod, only : string_t, read_matrix
!   type(string_t), allocatable :: strings(:,:)
!   real, allocatable    :: numr(:,:)
!   integer, allocatable :: numi(:,:)
!
!   ! To read first and third column from the file:
!   strings = read_matrix('test.txt', [1,3])
!   numr = strings % to_number()
!   numi = strings % to_number() ! auto-conversion from real to int
!
!   ! numr can contain NaN values (if conversion could not be made)
!   ! if NaN should not be silently ignored, call as:
!   numr = strings % to_number(allow_nan=.false.)
!
!
!  ! Using different delimiter than default space (optional argument):
!   strings = read_matrix('test.tct', [3,1], ';')
!
! -----------------------------------------------------------------------------
! Version 1 (March 2023)



  module file2mat_mod
    use iso_fortran_env, only : DP => real64
    implicit none(type, external)
    private
    public string_t
    public read_matrix, split
    public read_strings, remove_empty_strings, adjust_strings
    public read_line_from_file

    type :: string_t
      character(len=:), allocatable :: str
    contains
      procedure :: to_number => string_to_number
    end type string_t
    interface string_t
      module procedure string_new
    end interface string_t

  contains

! -----------------------------------------------------------------------------
! String class implementation:
! constructor, character to number conversion
! -----------------------------------------------------------------------------

    pure type(string_t) function string_new(str) result(new)
      character(len=*), intent(in) :: str
      allocate(character(len=len(str)) :: new%str)
      new%str = str
    end function string_new



    pure elemental function string_to_number(this, allow_nan) result(num)
      use, intrinsic :: ieee_arithmetic
      class(string_t), intent(in) :: this
      logical, intent(in), optional :: allow_nan
      real(DP) :: num

      integer :: ios
      logical :: allow_nan0

      allow_nan0 = .true.
      if (present(allow_nan)) allow_nan0 = allow_nan

      ! if formating error, a NaN value is silently set
      num = ieee_value(num, ieee_quiet_nan)
      if (len(this%str)>0) read(this%str,*,iostat=ios) num
      if (.not. allow_nan0 .and. ios/=0) &
      &  error stop 'string_to_number - conversion error and strict mode set'
    end function string_to_number



! -----------------------------------------------------------------------------
! Utilities to produce or modify arrays of strings:
! -----------------------------------------------------------------------------

    function read_matrix(file, cols, separator) result(arr)
      character(len=*), intent(in) :: file
      integer, intent(in) :: cols(:)
      character(len=*), intent(in), optional :: separator
      type(string_t), allocatable :: arr(:,:)
!
! Return 2D array of strings containing selected columns from the file.
! Optional argument for column delimiter can be added (if not " " by default).
!
      character(len=:), allocatable :: separator0
      type(string_t), allocatable :: lines(:), rowitems(:)
      integer :: nlines, ncols, i, j

      separator0 = ' ' ! space is a default value
      if (present(separator)) separator0 = separator

      ! read lines, ignore empty lines
      lines = read_strings(file)
      nlines = size(lines)
      call remove_empty_strings(lines)
      associate (empty => nlines-size(lines))
        if (empty > 0) then
          print '("read_matrix WARNING: ",i0," empty lines ignored")', empty
          nlines = nlines - empty
        end if
      end associate

      ncols = size(cols)
      allocate(arr(nlines, ncols))

      do i=1,nlines
        call split(lines(i)%str, separator0, rowitems)
        ! A special treatment for space as a separator:
        ! multiple spaces will produce empty items, get rid of them
        if (separator0==' ') call remove_empty_strings(rowitems)
        call adjust_strings(rowitems)
        do j=1, ncols
          associate (cj=>cols(j))
            arr(i,j) = string_t('')
            if (cj < 1) cycle
            if (cj <= size(rowitems)) arr(i,j) = string_t(rowitems(cj)%str)
          end associate
        end do
      end do
    end function read_matrix



    subroutine split(str, separator, items)
      character(len=*), intent(in) :: str, separator
      type(string_t), intent(out), allocatable :: items(:)
!
! Split string using separator.
!
      integer, parameter :: INIT_SEPS_MAX=50
      integer :: i, j, len_str, len_sep, n_seps, n_seps_max
      integer :: i1, i2
      integer, allocatable :: iseps(:)

      len_sep = len(separator)
      len_str = len(str)

      ! count, how many times the separator appears in the string and
      ! save its positions
      n_seps_max = INIT_SEPS_MAX
      allocate(iseps(n_seps_max))
      n_seps = 0
      j = 1
      do
        if (j > len_str) exit
        i = index(str(j:), separator)
        if (i <= 0) exit
        if (n_seps == n_seps_max) call expand(iseps, n_seps_max)
        n_seps = n_seps + 1
        iseps(n_seps) = i+j-1
        j = j + i + (len_sep-1)
      end do

      allocate(items(n_seps+1))

      do i = 1, n_seps+1
        if (i==1) then
          i1 = 1
        else
          i1 = iseps(i-1)+len_sep
        end if

        if (i==n_seps+1) then
          i2 = len_str
        else
          i2 = iseps(i)-1
        end if

        if (i2 >= i1) then
          items(i) = string_t(str(i1:i2))
        else
          items(i) = string_t('')
        end if
      end do

    contains
      subroutine expand(arr, nmax)
        integer, allocatable, intent(inout) :: arr(:)
        integer, intent(inout) :: nmax
        integer, allocatable :: wrk(:)
        integer :: nmax_ext

        if (size(arr)/=nmax) &
        &  error stop 'split/expand - array size does not match with input'
        nmax_ext = nmax*2
        allocate(wrk(nmax_ext))
        wrk(1:nmax) = arr
        nmax = nmax_ext
        call move_alloc(wrk, arr)
      end subroutine expand
    end subroutine split



    subroutine remove_empty_strings(items)
      type(string_t), allocatable, intent(inout) :: items(:)
!
! From the array of strings, remove empty strings.
!
      type(string_t), allocatable :: wrk(:)

      integer :: i, n, j

      ! count number of non-empty strings, return if nothing to do
      n = 0
      do i=1, size(items)
        if (.not. is_empty(items(i))) n = n+1
      end do
      if (n==size(items)) return

      allocate(wrk(n))
      j = 0
      do i=1, size(items)
        if (is_empty(items(i))) cycle
        j = j+1
        wrk(j) = string_t(items(i)%str)
      end do
      call move_alloc(wrk, items)

    contains
      logical function is_empty(this)
        type(string_t), intent(in) :: this
        is_empty = .false.
        if (len(this%str)==0) is_empty = .true.
      end function is_empty
    end subroutine remove_empty_strings



    subroutine adjust_strings(items)
      type(string_t), intent(inout) :: items(:)
!
! Remove leading and trailing blanks from strings.
!
      integer :: i

      do i=1, size(items)
        items(i) = string_t(trim(adjustl(items(i)%str)))
      end do
    end subroutine adjust_strings



    function read_strings(file) result(strings)
      character(len=*), intent(in) :: file
      type(string_t), allocatable :: strings(:)
!
! Return lines from the file as an array of strings.
!
      integer :: fid, i, n
      logical :: exist
      character(len=:), allocatable :: line

      inquire(file=file, exist=exist)
      if (.not. exist) then
        error stop 'read_strings - file "'//trim(file)//'" does not exist'
      end if
      open(newunit=fid, file=file, status='old')
      n = count_lines(fid)
      allocate(strings(n))
      do i=1, n
        call read_line_from_file(fid, line)
        strings(i) = string_t(line)
      end do
      close(fid)
    end function read_strings



! -----------------------------------------------------------------------------
! Local helper functions
! -----------------------------------------------------------------------------

    integer function count_lines(fid) result(n)
      integer, intent(in) :: fid
!
! Get the number of lines in the file. Empty lines are included in the count.
!
      integer :: ios
      character(len=1) :: buffer
      rewind(fid)
      n = 0
      do
        read(fid,'(a)',iostat=ios) buffer
        if (is_iostat_end(ios)) exit
        if (ios /= 0) error stop 'count_lines - read error'
        n = n + 1
      end do
      rewind(fid)
    end function count_lines



    subroutine read_line_from_file(fid, line, was_eof)
      integer, intent(in) :: fid
      character(len=:), allocatable, intent(out) :: line
      logical, intent(out), optional :: was_eof
!
! Read one line from an opened file. Optional argument can be used
! to tell whether EOF was reached.
!
      integer :: nread, ios
      character(len=80) :: buffer

      nread = 0
      buffer = ''
      line = ''

      ! The line length is unlimited. Line longer than 'len(buffer)'
      ! will be composed within several loop cycles.
      do
        read(fid,'(a)',advance='no',size=nread,iostat=ios) buffer
        if (is_iostat_end(ios) .or. is_iostat_eor(ios)) then
          if (nread>0) line = line//buffer(1:nread)
          exit
        else if (ios==0) then
          line = line//buffer
        else
          error stop 'read_line_from_file - read error'
        end if
      end do
      if (present(was_eof)) was_eof = is_iostat_end(ios)
    end subroutine read_line_from_file

  end module file2mat_mod
