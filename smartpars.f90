!
! Syntax for addrule
!   call object%addrule(rule_t(...), scalar_component_pointer)
!   call object%addrule(rule_t(...), num, vector_component_pointer)
!
! where rule_t(...) is
!   rule_t(keyword, [idefval=int-scalar | idefvals=int-array] )
!   rule_t(keyword, [rdefval=dp-scalar | rdefvals=dp-array] )
!

  module smartpars_mod
    use iso_fortran_env, only : DP => real64, error_unit, output_unit
    use file2mat_mod, only : string_t, read_strings
    implicit none (external,type)
    private
    public DP

    integer, parameter :: &
      PTYPE_INT = 1, PTYPE_REAL = 2


    type, public :: rule_t
      private
      ! Provided by an user of the extended type of "smartpars_at"
      ! via defined constructor
      character(len=:), allocatable :: keyword
      integer, allocatable :: idefvals(:)
      real(DP), allocatable :: rdefvals(:)
      ! Will be set automatically by local routines
      logical :: has_defval=.false.
      integer :: ptype
      integer :: pos
      integer :: n
      logical :: is_defined=.false.
    end type
    interface rule_t
      module procedure rule_new
    end interface


!TODO Improve error hanfling and messagess (in loadfromfile)
    type, public, abstract :: smartpars_at
      private
      integer, allocatable :: ipars(:)
      real(DP), allocatable :: rpars(:)
      type(rule_t), allocatable :: rules(:)
      integer :: iused=0, rused=0
    contains
      ! Initialize or Copy before use
      procedure :: init
      generic :: assignment(=) => copy
      ! Undefine all values
      procedure :: undefine_all
      ! Set default values
      procedure :: setdefvals
      ! Load values from file
      procedure :: loadfromfile
      ! Write values to unit
      procedure :: write2unit
      ! Test if all non-default values were given
      procedure :: all_defined
      ! For debugging
      procedure :: print => smartpars_print
      ! For implementation of child types
      ! (provide "localize" that specifies all fields by calling "addrule")
      procedure(localize_ai) , deferred :: localize
      generic :: addrule => addrule_int, addrule_intarr, addrule_real, addrule_realarr
      procedure, private :: addrule_int, addrule_intarr, addrule_real, addrule_realarr
      procedure, private, pass(new) :: copy
    end type


    abstract interface
      pure subroutine localize_ai(this)
        import smartpars_at
        implicit none
        class(smartpars_at), intent(inout), target :: this
      end subroutine
    end interface

  contains

    pure function rule_new(keyword, idefval, idefvals, rdefval, rdefvals) result(new)
      character(len=*), intent(in) :: keyword
      integer, intent(in), optional :: idefval
      real(DP), intent(in), optional :: rdefval
      integer, intent(in), optional :: idefvals(:)
      real(DP), intent(in), optional :: rdefvals(:)
      type(rule_t) :: new

      allocate(character(len=len(keyword)) :: new%keyword)
      new%keyword = keyword
      if (present(idefval) .or. present(rdefval) .or. present(idefvals) .or. present(rdefvals)) then
        new%has_defval = .true.
      else
        new%has_defval = .false.
      end if
      if (present(idefval) .and. .not. present(idefvals)) then
        new%idefvals = [idefval]
      else if (.not. present(idefval) .and. present(idefvals)) then
        new%idefvals = idefvals
      else if (present(idefval) .and. present(idefvals)) then
!       write(error_unit,'(a)') 'rule_new error - both idefval and idefvals present'
        error stop
      end if
      if (present(rdefval) .and. .not. present(rdefvals)) then
        new%rdefvals = [rdefval]
      else if (.not. present(rdefval) .and. present(rdefvals)) then
        new%rdefvals = rdefvals
      else if (present(rdefval) .and. present(rdefvals)) then
!       write(error_unit,'(a)') 'rule_new error - both rdefval and rdefvals present'
        error stop
      end if
    end function rule_new


    subroutine init(this, maxipars, maxrpars)
      class(smartpars_at), intent(out) :: this
      integer, intent(in) :: maxipars, maxrpars
!
! Initialize a new object
!
      integer :: ierr, i

      allocate( &
        this%ipars(maxipars), &
        this%rpars(maxrpars), &
        this%rules(0), stat=ierr)
      if (ierr/=0) then
        write(error_unit,*) 'Allocation error'
        error stop
      end if
      this%iused = 0
      this%rused = 0

      ! Set rules table and make pointer links by user procedure
      call this%localize()

      ! Verify that the size of parameter corresponds to the size of
      ! default value parameter if given. Report errors.
      do i=1, size(this%rules)
        associate(r=>this%rules(i))
          if (.not. r%has_defval) cycle
          select case (r%ptype)
          case(PTYPE_INT)
            if (allocated(r%idefvals)) then
              if (size(r%idefvals) == r%n) cycle
            end if
          case(PTYPE_REAL)
            if (allocated(r%rdefvals)) then
              if (size(r%rdefvals) == r%n) cycle
            end if
          case default
            error stop 'init - unknown ptype'
          end select
        end associate
        ! should not get here, unless something is wrong
        write(error_unit, '("Error with the size of default values arguments for rule ",i0)') i
        error stop
      end do

    end subroutine init


    subroutine copy(new, old)
      class(smartpars_at), intent(inout) :: new
      class(smartpars_at), intent(in) :: old
!
! Copy constructor - default assignment can not be used
!
      if (.not. allocated(old%ipars)) then
        write(error_unit, *) 'Error smartpars_copy: old is not initialized'
        error stop
      end if

      call new%init(old%iused, old%rused)
      new%ipars = old%ipars(1:old%iused)
      new%rpars = old%rpars(1:old%rused)
      new%rules%is_defined = old%rules%is_defined
    end subroutine copy

!
! Should be called from localize
!
! Complete user-given rule, add it to rules table, link pointer.
! Combinations for integer/real and scalar/rank-1 array
!
    pure subroutine addrule_int(this, rule, ivar)
      class(smartpars_at), intent(inout), target :: this
      type(rule_t), intent(in) :: rule
      integer, pointer, intent(inout) :: ivar

      type(rule_t) :: rule0

      rule0 = rule
      rule0%ptype = PTYPE_INT
      rule0%n = 1
      rule0%pos = this%iused+1
      this%iused = this%iused + rule0%n
      rule0%is_defined=.false.
      if (rule0%pos+rule0%n-1 > size(this%ipars)) then
!       write(error_unit,'(a)') 'Error - out of bounds of ipars'
        error stop
      end if

      this%rules = [this%rules, rule0]
      ivar => this%ipars(rule0%pos)
    end subroutine addrule_int

    pure subroutine addrule_intarr(this, rule, n, iarr)
      class(smartpars_at), intent(inout), target :: this
      type(rule_t), intent(in) :: rule
      integer, intent(in) :: n
      integer, pointer, intent(inout) :: iarr(:)
      type(rule_t) :: rule0

      rule0 = rule
      rule0%ptype = PTYPE_INT
      rule0%n = n
      rule0%pos = this%iused+1
      this%iused = this%iused + rule0%n
      rule0%is_defined=.false.
      if (rule0%pos+rule0%n-1 > size(this%ipars)) then
!       write(error_unit,'(a)') 'Error - out of bounds of ipars'
        error stop
      end if

      this%rules = [this%rules, rule0]
      iarr => this%ipars(rule0%pos:rule0%pos+rule0%n-1)
    end subroutine addrule_intarr

    pure subroutine addrule_real(this, rule, rvar)
      class(smartpars_at), intent(inout), target :: this
      type(rule_t), intent(in) :: rule
      real(DP), pointer, intent(inout) :: rvar

      type(rule_t) :: rule0

      rule0 = rule
      rule0%ptype = PTYPE_REAL
      rule0%n = 1
      rule0%pos = this%rused+1
      this%rused = this%rused + rule0%n
      rule0%is_defined=.false.
      if (rule0%pos+rule0%n-1 > size(this%rpars)) then
!       write(error_unit,'(a)') 'Error - out of bounds of rpars'
        error stop
      end if

      this%rules = [this%rules, rule0]
      rvar => this%rpars(rule0%pos)
    end subroutine addrule_real

    pure subroutine addrule_realarr(this, rule, n, rarr)
      class(smartpars_at), intent(inout), target :: this
      type(rule_t), intent(in) :: rule
      integer, intent(in) :: n
      real(DP), pointer, intent(inout) :: rarr(:)
      type(rule_t) :: rule0

      rule0 = rule
      rule0%ptype = PTYPE_REAL
      rule0%n = n
      rule0%pos = this%rused+1
      this%rused = this%rused + rule0%n
      rule0%is_defined=.false.
      if (rule0%pos+rule0%n-1 > size(this%rpars)) then
!       write(error_unit,'(a)') 'Error - out of bounds of rpars'
        error stop
      end if

      this%rules = [this%rules, rule0]
      rarr => this%rpars(rule0%pos:rule0%pos+rule0%n-1)
    end subroutine addrule_realarr


    pure function all_defined(this) result(yes)
      class(smartpars_at), intent(in) :: this
      logical :: yes

      if (.not. allocated(this%ipars)) &
        & error stop 'object not initialized'
      yes = all(this%rules%is_defined)
    end function all_defined


    pure subroutine setdefvals(this, overwrite)
      class(smartpars_at), intent(inout) :: this
      logical, intent(in), optional :: overwrite
!
! Set values of undefined parameters to its default values. If parameter
! is already defined, its value is unchanged,
! unless "overwrite=TRUE" is provided.
! Parameters without default values are ignored.
!
      logical :: overwrite0
      integer :: i, j

      ! test object has been initialized
      if (.not. allocated(this%ipars)) then
!       write(error_unit,'("setdefvals - object not initialized")')
        error stop
      end if

      ! do not overwrite unless explicitly told
      overwrite0 = .false.
      if (present(overwrite)) overwrite0 = overwrite

      do i=1,size(this%rules)
        associate(r=>this%rules(i))
          if (.not. r%has_defval) cycle
          if (r%is_defined .and. .not. overwrite0) cycle
          do j=r%pos, r%pos+r%n-1
            select case(r%ptype)
            case(PTYPE_INT)
              this%ipars(j) = r%idefvals(j-r%pos+1)
            case(PTYPE_REAL)
              this%rpars(j) = r%rdefvals(j-r%pos+1)
            case default
              error stop 'unknown ptype'
            end select
          end do
          r%is_defined = .true.
        end associate
      end do
    end subroutine setdefvals


    subroutine write2unit(this, fid)
      class(smartpars_at), intent(in) :: this
      integer, intent(in) :: fid
!
! Write <keyword> <value(s)> of all defined parameters to unit "fid"
!
      integer :: i, maxlen

      maxlen = 0
      do i=1, size(this%rules)
        associate (l=>len(this%rules(i)%keyword))
          if (l > maxlen) maxlen = l
        end associate
      end do

      do i=1, size(this%rules)
        associate(r=>this%rules(i))
          if (.not. r%is_defined) cycle
          write(fid,'(a)',advance='no') &
              & r%keyword//repeat(' ',1+maxlen-len(r%keyword))
          select case(r%ptype)
          case(PTYPE_INT)
            write(fid,'(*(g0,1x))') this%ipars(r%pos:r%pos+r%n-1)
          case(PTYPE_REAL)
            write(fid,'(*(g0,1x))') this%rpars(r%pos:r%pos+r%n-1)
          case default
            error stop 'write2unit - unknown ptype'
          end select
        end associate
      end do
    end subroutine write2unit


    pure subroutine undefine_all(this)
      class(smartpars_at), intent(inout) :: this
!
! Mark all parameters as undefined
!
      this%rules%is_defined = .false.
    end subroutine undefine_all


    subroutine loadfromfile(this, file, overwrite)
      class(smartpars_at), intent(inout) :: this
      character(len=*), intent(in) :: file
      logical, intent(in), optional :: overwrite
!
! Load from file
!
      type(string_t), allocatable :: lines(:)
      type(string_t) :: word, trunk
      integer :: i, j, iostat
      character(len=100) :: errmsg, errmsg2
      logical :: overwrite0
      character(len=1) first_char
      logical, allocatable :: is_defined_here(:)

      ! verify object has been initialized
      if (.not. allocated(this%ipars)) then
        write(error_unit,'("loadfromfile - object not initialized")')
        error stop
      end if

      ! defined values are protected (unless "overwrite=T")
      overwrite0 = .false.
      if (present(overwrite)) overwrite0 = overwrite
      allocate(is_defined_here(size(this%rules)), source=.false.)

      ! parse lines from file
      print *, 'Loading parameters from "'//trim(file)//'" ...'
      errmsg = ''
      lines = read_strings(file)
      do i=1,size(lines)
        ! skip empty lines
        if (len(lines(i)%str)==0) cycle

        ! skip comment lines
        first_char = adjustl(lines(i)%str)
        if (scan('%!#',first_char) /= 0) cycle

        ! identify keyword
        call chop_first_word(lines(i)%str, word, trunk)
        j = findrule(this%rules, word%str)
        if (j==0) then
          write(errmsg, '("keyword """,a,""" on line",i0," not recognized")') word%str, i
          exit
        end if

        associate(r => this%rules(j))
          ! check if already defined
          if (is_defined_here(j)) then
            write(errmsg,'("Parameter ",a," duplicity found")') word%str
            exit
          else if (r%is_defined .and. .not. overwrite0) then
            write(errmsg,'("Parameter ",a," already defined")') word%str
            exit
          else if (r%is_defined) then
            write(output_unit,'("Warning: Parameter ",a," is being overwritten")') word%str
          end if

          ! try to read
          select case(r%ptype)
          case(PTYPE_INT)
            read(trunk%str,*,iomsg=errmsg2,iostat=iostat) &
              this%ipars(r%pos:r%pos+r%n-1)
          case(PTYPE_REAL)
            read(trunk%str,*,iomsg=errmsg2,iostat=iostat) &
              this%rpars(r%pos:r%pos+r%n-1)
          case default
            error stop 'unknown ptype'
          end select
          if (iostat/=0) then
            errmsg = "Reading error "//trim(errmsg2)
            exit
          else
            ! value read succesfully
            r%is_defined = .true.
            is_defined_here(j) = .true.
          end if
        end associate
      end do

      if (i /= size(lines)+1) then
        print *, 'ERROR - '//errmsg
        error stop
      end if
    end subroutine loadfromfile


    pure subroutine chop_first_word(line, word, trunk)
      character(len=*), intent(in) :: line
      type(string_t), intent(out) :: word, trunk

      character(len=1) :: first_nonblank_char
      integer :: ifch, sp, tab

      ! special case if line is blank
      if (len_trim(adjustl(line))==0) then
        word = string_t('')
        trunk = string_t('')
        return
      end if

      first_nonblank_char = adjustl(line)
      ifch = scan(line, first_nonblank_char)

      sp = scan(line(ifch:), ' ')
      tab = scan(line(ifch:), achar(9))
      sp = minval([sp, tab], dim=1, mask=[sp, tab]>0)
      if (sp==0) then
        sp = len(line)+1
      else
        sp = sp+ifch-1
      end if
      word = string_t(trim(adjustl(line(:sp-1))))
      trunk = string_t(trim(adjustl(line(sp:))))
!print *, 'line <'//line//'>'
!print *, 'word <'//word%str//'>'
!print *, 'trun <'//trunk%str//'>'
    end subroutine chop_first_word


    pure integer function findrule(rules, keyword) result(id)
      type(rule_t), intent(in) :: rules(:)
      character(len=*), intent(in) :: keyword
!
! Helper to return the position of keyword on the line
!
      integer :: i
      id = 0
      do i=1,size(rules)
        if (rules(i)%keyword==keyword) then
          id = i
          exit
        end if
      end do
    end function findrule


    subroutine smartpars_print(this)
      class(smartpars_at), intent(in) :: this
!
! Print - for debugging
!
      integer :: i

      if (.not. allocated(this%ipars)) then
        print '("Object not initialized ")'
        return
      end if

      print '("Is defined ? ",*(l2))', this%rules%is_defined

      print '(a,*(g0,1x))', 'IPARS =', this%ipars(1:this%iused)
      print '(a,*(g0,1x))', 'RPARS =', this%rpars(1:this%rused)
      print '("Numbr of rules is ",i0)', size(this%rules)
      do i=1, size(this%rules)
        associate(a=>this%rules(i))
          write(output_unit,advance='no',fmt='("<",a,"> has_defval? ",l1)') a%keyword, a%has_defval
          if (a%has_defval .and. a%ptype==PTYPE_INT) then
            write(output_unit,advance='no',fmt='(2x,*(g0,1x))') a%idefvals
          end if
          if (a%has_defval .and. a%ptype==PTYPE_REAL) then
            write(output_unit,advance='no',fmt='(2x,*(g0,1x))') a%rdefvals
          end if
          write(output_unit,advance='no',fmt='("  ptype ",i0,"  pos ",i0,"  n ",i0,"  is_defined",l1)') &
            & a%ptype, a%pos, a%n, a%is_defined
          write(output_unit,*)
        end associate
      end do
    end subroutine smartpars_print

  end module smartpars_mod
