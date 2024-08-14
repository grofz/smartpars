!===============================================
! (c) 2024 Z. Grof (UCT Prague) <grofz@vscht.cz>
! ==============================================

! Syntax for addrule
!   call object%addrule(rule_t(...), scalar_component_pointer)
!   call object%addrule(rule_t(...), num, vector_component_pointer)
!
! where rule_t(...) is
!   rule_t(keyword, [idefval=int-scalar | idefvals=int-array] )
!   rule_t(keyword, [rdefval=dp-scalar | rdefvals=dp-array] )
!
!TODO Add documentation and examples

  module smartpars_mod
    use iso_fortran_env, only : DP => real64, error_unit, output_unit
    use file2mat_mod, only : string_t, read_strings
    implicit none (external,type)
    private
    public DP

    integer, parameter :: &
      PTYPE_INT = 1, PTYPE_REAL = 2, BUFFER_MAX_LEN = 100


    type, public :: rule_t
      private
      ! Provided by an user of the type extended from "smartpars_at"
      ! via "rule_t" generic interface
      character(len=:), allocatable :: keyword
      integer, allocatable :: idefvals(:)
      real(DP), allocatable :: rdefvals(:)
      ! Will be controlled automatically
      logical :: has_defval=.false.
      integer :: n
      integer :: ptype ! integer or real
      integer :: pos
      logical :: is_defined=.false.
    end type
    interface rule_t
      module procedure rule_new_nodef, rule_new_is, rule_new_rs, &
        & rule_new_iarr, rule_new_rarr
    end interface


    type, public, abstract :: smartpars_at
      private
      integer, allocatable :: ipars(:)
      real(DP), allocatable :: rpars(:)
      type(rule_t), allocatable :: rules(:)
      integer :: iused=0, rused=0
      logical :: addrule_lock=.true. ! unlocked during initialization only
    contains
      procedure(localize_ai) , deferred :: localize
      procedure :: init
      generic :: assignment(=) => copy
      procedure :: undefine_all
      procedure :: setdefvals
      procedure :: loadfromfile
      procedure :: write2unit
      procedure :: all_defined
      procedure :: is_initialized
      procedure :: print => smartpars_print
      generic :: addrule => &
          & addrule_is, addrule_iarr, addrule_rs, addrule_rarr
      procedure, private :: &
          & addrule_is, addrule_iarr, addrule_rs, addrule_rarr
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



! ===================
! rule_t constructors
! ===================
    pure function rule_new_nodef(keyword) result(new)
      character(len=*), intent(in) :: keyword
      type(rule_t) :: new
      new = rule_new(keyword)
    end function

    pure function rule_new_rs(keyword, rdefval) result(new)
      character(len=*), intent(in) :: keyword
      real(DP), intent(in) :: rdefval
      type(rule_t) :: new
      new = rule_new(keyword, rd=rdefval)
    end function

    pure function rule_new_is(keyword, idefval) result(new)
      character(len=*), intent(in) :: keyword
      integer, intent(in) :: idefval
      type(rule_t) :: new
      new = rule_new(keyword, id=idefval)
    end function

    pure function rule_new_rarr(keyword, rdefvals) result(new)
      character(len=*), intent(in) :: keyword
      real(DP), intent(in) :: rdefvals(:)
      type(rule_t) :: new
      new = rule_new(keyword, rds=rdefvals)
    end function

    pure function rule_new_iarr(keyword, idefvals) result(new)
      character(len=*), intent(in) :: keyword
      integer, intent(in) :: idefvals(:)
      type(rule_t) :: new
      new = rule_new(keyword, ids=idefvals)
    end function

    ! Private constructor processing all cases
    pure function rule_new(keyword, id, ids, rd, rds) result(new)
      character(len=*), intent(in) :: keyword
      integer, intent(in), optional :: id , ids(:)
      real(DP), intent(in), optional :: rd, rds(:)
      type(rule_t) :: new

      allocate(character(len=len(keyword)) :: new%keyword)
      new%keyword = keyword
      if (present(id) .or. present(rd) .or. present(ids) .or. &
        & present(rds)) then
        new%has_defval = .true.
        if (present(id)) new%idefvals = [id]
        if (present(ids)) new%idefvals = ids
        if (present(rd)) new%rdefvals = [rd]
        if (present(rds)) new%rdefvals = rds
      else
        new%has_defval = .false.
      end if
    end function rule_new



! =========================
! smartpars_at constructors
! =========================
    pure subroutine addrule_is(this, rule, is)
      class(smartpars_at), intent(inout), target :: this
      type(rule_t), intent(in) :: rule
      integer, pointer, intent(inout) :: is
      call addrule_main(this, rule, is=is)
    end subroutine

    pure subroutine addrule_rs(this, rule, rs)
      class(smartpars_at), intent(inout), target :: this
      type(rule_t), intent(in) :: rule
      real(DP), pointer, intent(inout) :: rs
      call addrule_main(this, rule, rs=rs)
    end subroutine

    pure subroutine addrule_iarr(this, rule, n, iarr)
      class(smartpars_at), intent(inout), target :: this
      type(rule_t), intent(in) :: rule
      integer, intent(in) :: n
      integer, pointer, intent(inout) :: iarr(:)
      call addrule_main(this, rule, n=n, iarr=iarr)
    end subroutine

    pure subroutine addrule_rarr(this, rule, n, rarr)
      class(smartpars_at), intent(inout), target :: this
      type(rule_t), intent(in) :: rule
      integer, intent(in) :: n
      real(DP), pointer, intent(inout) :: rarr(:)
      call addrule_main(this, rule, n=n, rarr=rarr)
    end subroutine


    ! Private procedure processing all cases
    pure subroutine addrule_main(this, rule, n, is, rs, iarr, rarr)
      class(smartpars_at), intent(inout), target :: this
      type(rule_t), intent(in) :: rule
      integer, intent(in), optional :: n
      integer, pointer, intent(inout), optional :: is, iarr(:)
      real(DP), pointer, intent(inout), optional :: rs, rarr(:)

      type(rule_t) :: rule0

      ! Verify the lock and presence of just one optional argument
      if (this%addrule_lock) &
        & error stop 'addrule can be called during initialization only'
      if (count(&
        & [present(is), present(iarr), present(rs), present(rarr)])/=1) &
        & error stop 'internal error 921 - only single arg allowed'
      if ((present(iarr) .or. present(rarr)) .and. .not. present(n)) &
        & error stop 'internal error 923 - n not present'
      if ((present(is) .or. present(rs)) .and. present(n)) &
        & error stop 'internal error 924 - n is present'

      ! Define remaining components of given rule
      rule0 = rule
      rule0%is_defined = .false.

      if (present(n)) then
        rule0%n = n
      else
        rule0%n = 1
      end if

      if (present(is) .or. present(iarr)) then
        rule0%ptype = PTYPE_INT
        rule0%pos = this%iused+1
        this%iused = this%iused + rule0%n
      else if (present(rs) .or. present(rarr)) then
        rule0%ptype = PTYPE_REAL
        rule0%pos = this%rused+1
        this%rused = this%rused + rule0%n
      else
        error stop 'internal error 908'
      end if

      ! Either build rules table or make links to internal arrays
      if (.not. this%is_initialized()) then
        this%rules = [this%rules, rule0]
      else
        if (present(is)) then
          is => this%ipars(rule0%pos)
        else if (present(rs)) then
          rs => this%rpars(rule0%pos)
        else if (present(iarr)) then
          iarr => this%ipars(rule0%pos:rule0%pos+rule0%n-1)
        else if (present(rarr)) then
          rarr => this%rpars(rule0%pos:rule0%pos+rule0%n-1)
        else
          error stop 'internal error 906'
        end if
      end if
    end subroutine addrule_main


    subroutine init(this)
      class(smartpars_at), intent(out) :: this
      integer :: maxipars, maxrpars
!
! Initialize a new object
!
      integer :: i, nvals
      character(len=BUFFER_MAX_LEN) :: buffer


      ! make rules table and find out the size of arrays needed
      allocate(this%rules(0))
      this%iused = 0
      this%rused = 0
      this%addrule_lock = .false.
      call this%localize()

      ! now allocate internal arrays and link them with user space pointers
      allocate(this%ipars(this%iused), this%rpars(this%rused))
      this%iused = 0
      this%rused = 0
      call this%localize()
      this%addrule_lock = .true.

      ! verify parameter size is same as size of its default values
      do i=1, size(this%rules)
        associate(r=>this%rules(i))
          if (.not. r%has_defval) cycle
          select case (r%ptype)
          case(PTYPE_INT)
            if (allocated(r%idefvals)) then
              if (size(r%idefvals) == r%n) cycle
              nvals = size(r%idefvals)
            end if
          case(PTYPE_REAL)
            if (allocated(r%rdefvals)) then
              if (size(r%rdefvals) == r%n) cycle
              nvals = size(r%rdefvals)
            end if
          case default
            error stop 'internal error 946 - invalid ptype'
          end select
          ! if we get here, something is wrong
          write(buffer, '("Parameter ",a,": no. of defvals ",i0,&
            & " is not same as parameter size ",i0)') r%keyword, nvals, r%n
          error stop trim(buffer)
        end associate
      end do

    end subroutine init


    subroutine copy(new, old)
      class(smartpars_at), intent(inout) :: new
      class(smartpars_at), intent(in) :: old
!
! Copy constructor - default assignment can not be used
!
      if (.not. old%is_initialized()) &
        & error stop 'smartpars_copy - old is not initialized'
      call new%init()
      new%ipars = old%ipars
      new%rpars = old%rpars
      new%rules%is_defined = old%rules%is_defined
    end subroutine copy



! ======================
! smartpars_at utilities
! ======================
    pure function is_initialized(this)
      class(smartpars_at), intent(in) :: this
      logical :: is_initialized

      is_initialized = allocated(this%rpars) .and. allocated(this%ipars)
    end function is_initialized


    pure function all_defined(this) result(yes)
      class(smartpars_at), intent(in) :: this
      logical :: yes
!
! Return true if all parameters are defined
!
      if (.not. this%is_initialized()) &
        & error stop 'all_defined - object is not initialized'
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
      if (.not. this%is_initialized()) &
        & error stop 'setdefvals - object is not initialized'

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
              error stop 'internal error 1023 - unknown ptype'
            end select
          end do
          r%is_defined = .true.
        end associate
      end do
    end subroutine setdefvals


    subroutine write2unit(this, fid, include_undefined)
      class(smartpars_at), intent(in) :: this
      integer, intent(in) :: fid
      logical, intent(in), optional :: include_undefined
!
! Write <keyword> <value(s)> of all defined parameters to unit "fid"
!
      integer :: i, maxlen
      logical :: include_undefined0

      if (.not. this%is_initialized()) &
        & error stop 'write2unit - object is not initialized'

      ! Skip undefined values unless told otherwise
      include_undefined0 = .false.
      if (present(include_undefined)) include_undefined0=include_undefined

      ! Get maximum keyword length
      maxlen = 0
      do i=1, size(this%rules)
        associate (l=>len(this%rules(i)%keyword))
          if (l > maxlen) maxlen = l
        end associate
      end do

      do i=1, size(this%rules)
        associate(r=>this%rules(i))
          if (.not. r%is_defined .and. .not. include_undefined0) cycle
          write(fid,'(a)',advance='no') &
              & r%keyword//repeat(' ',1+maxlen-len(r%keyword))
          if (r%is_defined) then
            select case(r%ptype)
            case(PTYPE_INT)
              write(fid,'(*(g0,1x))') this%ipars(r%pos:r%pos+r%n-1)
            case(PTYPE_REAL)
              write(fid,'(*(g0,1x))') this%rpars(r%pos:r%pos+r%n-1)
            case default
              error stop 'internal error 1029 - unknown ptype'
            end select
          else
            write(fid,'("NOT DEFINED")')
          end if
        end associate
      end do
    end subroutine write2unit


    pure subroutine undefine_all(this)
      class(smartpars_at), intent(inout) :: this
!
! Mark all parameters as undefined
!
      if (.not. this%is_initialized()) &
        & error stop 'undefine_all - object is not initialized'
      this%rules%is_defined = .false.
    end subroutine undefine_all



! ==============
! load from file
! ==============

    subroutine loadfromfile(this, file, overwrite, errmsg)
      class(smartpars_at), intent(inout) :: this
      character(len=*), intent(in) :: file
      logical, intent(in), optional :: overwrite
      character(len=:), allocatable, intent(out), optional :: errmsg
!
! Load from file. Providing "overwrite=.T." allows to overwrite
! parameters that wer already defined.
!
! If optional "errmsg" parameter is provided
! - in case of an error, "errmsg" will be allocated with error
!   nessage on return
! - if all ok, "errmsg" will be left not-allocated
! If "errmsg" is not provided, the subroutine stops in case of error
!
      type(string_t), allocatable :: lines(:)
      type(string_t) :: word, trunk
      integer :: i, j, iostat
      character(len=BUFFER_MAX_LEN) :: errmsg0, iomsg
      logical :: overwrite0, exist
      character(len=1) first_char
      logical, allocatable :: is_defined_here(:)

      ! verify object has been initialized
      if (.not. this%is_initialized()) &
        & error stop 'loadfromfile - object is not initialized'

      ! defined values are protected (unless "overwrite=T")
      overwrite0 = .false.
      if (present(overwrite)) overwrite0 = overwrite

      allocate(is_defined_here(size(this%rules)), source=.false.)

      errmsg0 = ''
      MAIN: block
        ! check file exists
        inquire(file=file, exist=exist)
        if (.not. exist) then
          write(errmsg0,'("File """,a,""" does not exist")') trim(file)
          exit MAIN
        end if

        ! parse lines from file
print *, 'Loading parameters from "'//trim(file)//'" ...'
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
            write(errmsg0, '("Keyword """,a,""" on line ",i0," not recognized")') word%str, i
            exit MAIN
          end if

          associate(r => this%rules(j))
            ! check if already defined
            if (is_defined_here(j)) then
              write(errmsg0,'("Duplicity of parameter ",a," in the file")') word%str
              exit MAIN
            else if (r%is_defined .and. .not. overwrite0) then
              write(errmsg0,'("Parameter ",a," already defined and overwrite is not allowed")') word%str
              exit MAIN
            else if (r%is_defined) then
write(output_unit,'("Warning: Parameter ",a," is being overwritten")') word%str
            end if

            ! try to read
            select case(r%ptype)
            case(PTYPE_INT)
              read(trunk%str,*,iomsg=iomsg,iostat=iostat) &
                this%ipars(r%pos:r%pos+r%n-1)
            case(PTYPE_REAL)
              read(trunk%str,*,iomsg=iomsg,iostat=iostat) &
                this%rpars(r%pos:r%pos+r%n-1)
            case default
              error stop 'internal error 1058 - unknown ptype'
            end select
            if (iostat/=0) then
              write(errmsg0,'("Reading parameter ",a," on line ",i0," lead to I/O error: """,a,"""")') r%keyword, i, trim(iomsg)
              exit MAIN
            else
              ! value read succesfully
              r%is_defined = .true.
              is_defined_here(j) = .true.
            end if
          end associate
        end do

        if (present(errmsg)) then
          if (allocated(errmsg)) deallocate(errmsg)
        end if
        return ! normal exit

      end block MAIN

      ! error treatment
      if (present(errmsg)) then
        errmsg = trim(errmsg0)
      else
        error stop 'loadfromfile - '//trim(errmsg0)
      end if

    end subroutine loadfromfile


    ! Private helper for loadfromfile
    subroutine chop_first_word(line, word, trunk)
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
 print *, 'aa ',[sp, tab]
      if (sp==0 .and. tab==0) then
        sp = 0
      else
        sp = minval([sp, tab], dim=1, mask=[sp, tab]>0)
      end if
      if (sp==0) then
        sp = len(line)+1
      else
        sp = sp+ifch-1
      end if
      print *, 'sp=',sp, 'len=',len(line), 'ifch=',ifch
      word = string_t(trim(adjustl(line(:sp-1))))
      trunk = string_t(trim(adjustl(line(sp:))))
    end subroutine chop_first_word


    ! Private helper for loadfromfile
    pure integer function findrule(rules, keyword) result(id)
      type(rule_t), intent(in) :: rules(:)
      character(len=*), intent(in) :: keyword

      integer :: i

      id = 0
      do i=1,size(rules)
        if (rules(i)%keyword==keyword) then
          id = i
          exit
        end if
      end do
    end function findrule



! =============
! for debugging
! =============

    subroutine smartpars_print(this)
      class(smartpars_at), intent(in) :: this

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

!EOF smartpars.f90
