  module localpars_mod
    use smartpars_mod, only : smartpars_at, rule_t, DP
    implicit none (type, external)
    private

    type, public, extends(smartpars_at) :: localpars_t
      integer, pointer :: model
      integer, pointer :: typ
      integer, pointer :: vals(:)
      real(DP), pointer :: dif1
      real(DP), pointer :: koef(:)
    contains
      procedure :: localize => localpars_localize
    end type

  contains

!
! Example procedure that has to be provided
!
    subroutine localpars_localize(this)
      class(localpars_t), intent(inout), target :: this

      call this%addrule(rule_t('model'), this%model)
      call this%addrule(rule_t('typ', idefval=15), this%typ)
      call this%addrule(rule_t('vals'), 3, this%vals)
      call this%addrule(rule_t('dif1', rdefval=3.14_DP), this%dif1)
      call this%addrule(rule_t('koefs'), 2, this%koef)
    end subroutine localpars_localize
  end module localpars_mod
