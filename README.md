# smartpars

Fortran package to work with parameters

## Getting Started

### Define user's parameter type

For each parameter, the following must be specified
- integer / real
- scalar / vector (for vectors number of components must be specified)
- name (keyword in file or code)
- default value for the parameter

Use the following code template to define the new user type "localpars_t"

```fortran
 module localpars_mod
    use smartpars_mod, only : smartpars_at, rule_t, DP
    implicit none (type, external)
    private

    ! ***Step 1***
    ! We define the following parameters:
    ! pis, prs, sis, srs
    ! ...are scalar integer and real parameters
    ! piarr3, prarr2, siarr3, srarr2 
    ! ...are vector integer and real parameter
    !
    ! These pointers will be linked
    ! during initialization with internals of the object.
    ! User then can access and modify these pointers at will
    !
    type, public, extends(smartpars_at) :: localpars_t
      integer, pointer  :: pis, piarr3(:)
      real(DP), pointer :: prs, prarr2(:)
      integer, pointer  :: sis, siarr3(:)
      real(DP), pointer :: srs, srarr2(:)
    contains
      procedure :: localize
    end type

  contains

    ! ***Step 2***
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
```

### Use user parameter type

Declare and initialize structures "pars" 

```fortran
    use localpars_mod, only : pars_t => localpars_t
    :
    type(pars_t) :: pars, pars2
    :
    call pars%init()
```

### Set default values to components with default values

```fortran
    call pars%setdefvals()
```

### Load values of components from the file

```fortran
    character(len=:), allocatable :: errmsg
    :
    call pars%loadfromfile('myfile.txt',errmsg=errmsg,overwrite=.true.)
    if (allocated(errmsg)) then
      print *, 'ERROR -'//errmsg
      stop 1
    else
      print *, 'Loading ok...'
    end if
```

### Check if all components are defined

```fortran
    print *, 'All defined? ', pars%all_defined()
```

### Dump values to specified unit

```fortran
    call pars%write2unit(output_unit, include_undefined=.true.)
```

### Copy to another structure and show

```fortran
    pars2 = pars
    call pars2%print()
```

### Use parameter values in the code

You can directly access the pointers in own code

```fortran
  x = pars%pis
  pars%pis = 11
```

