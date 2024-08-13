  module smartpars_mod
    use iso_fortran_env, only : DP => real64, error_unit
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
      integer :: idefval
      real(DP) :: rdefval
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
      ! Set default values
      procedure :: setdefvals
      ! Load values from file
    ! procedure :: loadfromfile
      ! Write values to unit
      procedure :: write2unit
      ! Test if all non-default values were given
    ! procedure :: arevalidvalues
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
      subroutine localize_ai(this)
        import smartpars_at
        implicit none
        class(smartpars_at), intent(inout), target :: this
      end subroutine
    end interface

  contains

    function rule_new(keyword, idefval, rdefval) result(new)
      character(len=*), intent(in) :: keyword
      integer, intent(in), optional :: idefval
      real(DP), intent(in), optional :: rdefval
      type(rule_t) :: new

      allocate(character(len=len(keyword)) :: new%keyword)
      new%keyword = keyword
      if (present(idefval) .or. present(rdefval)) then
        new%has_defval = .true.
      else
        new%has_defval = .false.
      end if
      if (present(idefval)) new%idefval = idefval
      if (present(rdefval)) new%rdefval = rdefval
    end function rule_new


    subroutine init(this, maxipars, maxrpars)
      class(smartpars_at), intent(out) :: this
      integer, intent(in) :: maxipars, maxrpars
!
! Initialize a new object
!
      integer :: ierr

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
    subroutine addrule_int(this, rule, ivar)
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
        write(error_unit,'(a)') 'Error - out of bounds of ipars'
        error stop
      end if

      this%rules = [this%rules, rule0]
      ivar => this%ipars(rule0%pos)
    end subroutine addrule_int

    subroutine addrule_intarr(this, rule, n, iarr)
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
        write(error_unit,'(a)') 'Error - out of bounds of ipars'
        error stop
      end if

      this%rules = [this%rules, rule0]
      iarr => this%ipars(rule0%pos:rule0%pos+rule0%n-1)
    end subroutine addrule_intarr

    subroutine addrule_real(this, rule, rvar)
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
        write(error_unit,'(a)') 'Error - out of bounds of rpars'
        error stop
      end if

      this%rules = [this%rules, rule0]
      rvar => this%rpars(rule0%pos)
    end subroutine addrule_real

    subroutine addrule_realarr(this, rule, n, rarr)
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
        write(error_unit,'(a)') 'Error - out of bounds of rpars'
        error stop
      end if

      this%rules = [this%rules, rule0]
      rarr => this%rpars(rule0%pos:rule0%pos+rule0%n-1)
    end subroutine addrule_realarr


    subroutine setdefvals(this, overwrite)
      class(smartpars_at), intent(inout) :: this
      logical, intent(in), optional :: overwrite
!
!
!
      logical :: overwrite0
      integer :: i, j

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
              this%ipars(j) = r%idefval
            case(PTYPE_REAL)
              this%rpars(j) = r%rdefval
            case default
              error stop 'unknown ptype'
            end select
          end do
        end associate
      end do
    end subroutine setdefvals


    subroutine write2unit(this, fid)
      class(smartpars_at), intent(in) :: this
      integer, intent(in) :: fid

      integer :: i, maxlen

      maxlen = 0
      do i=1, size(this%rules)
        associate (l=>len(this%rules(i)%keyword))
          if (l > maxlen) maxlen = l
        end associate
      end do

      do i=1, size(this%rules)
        associate(r=>this%rules(i))
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

      print '(a,*(g0,1x))', 'IPARS =', this%ipars(1:this%iused)
      print '(a,*(g0,1x))', 'RPARS =', this%rpars(1:this%rused)
      print '("Numbr of rules is ",i0)', size(this%rules)
      do i=1, size(this%rules)
        associate(a=>this%rules(i))
          print '("<",a,"> has_defval? ",l1,"  i=",g0,"  r=",g0, "  ptype ",i0,"  pos ",i0,"  n ",i0,"  is_defined",l1)', &
            a%keyword, a%has_defval, a%idefval, a%rdefval, &
            a%ptype, a%pos, a%n, a%is_defined
        end associate
      end do
    end subroutine smartpars_print

  end module smartpars_mod
