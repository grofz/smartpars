  module localpars_mod
    use smartpars_mod, only : smartpars_at, rule_t, DP
    implicit none (type, external)
    private

    ! Extended type should contain pointers to real/integer
    ! scalars or rank-1 arrays. These pointers will be linked
    ! during initialization with internals of the object.
    ! User can access and modify these pointers at will
    type, public, extends(smartpars_at) :: localpars_t
      integer, pointer  :: pis, piarr3(:)
      real(DP), pointer :: prs, prarr2(:)
      integer, pointer  :: sis, siarr3(:)
      real(DP), pointer :: srs, srarr2(:)
    contains
      procedure :: localize
    end type

  contains

    ! User should provide this procedure defining the individual
    ! parameters. It will be run during initialization. User is
    ! not suppossed to run it directly
    pure subroutine localize(this)
      class(localpars_t), intent(inout), target :: this

      ! scalar parameters without default values
      call this%addrule(rule_t('pis'), this%pis)
      call this%addrule(rule_t('prs'), this%prs)

      ! vector parameters without default values,
      ! vector size must be provided
      call this%addrule(rule_t('piarr3'), 3, this%piarr3)
      call this%addrule(rule_t('prarr2'), 2, this%prarr2)

      ! parameters with default values,
      ! the number of default values must correspond to size of vectors
      call this%addrule(rule_t('sis', 15), this%sis)
      call this%addrule(rule_t('srs', 30.0_DP), this%srs)
      call this%addrule(rule_t('siarr3',[10,20,30]), 3, this%siarr3)
      call this%addrule(rule_t('srarr2',[42._DP, 69._DP]), 2, this%srarr2)
    end subroutine localize
  end module localpars_mod
