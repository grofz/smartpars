  program test_localpars
    use iso_fortran_env, only : output_unit
    use localpars_mod, only : pars_t => localpars_t
    implicit none

    type(pars_t) :: pars, pars2
    character(len=:), allocatable :: errmsg

    print *, 'Hello from main test'
    call pars%init()
    errmsg='none'
    call pars%setdefvals()
    call pars%loadfromfile('myfile.txt',errmsg=errmsg,overwrite=.true.)
    if (allocated(errmsg)) then
      print *, 'ERROR -'//errmsg
      stop 1
    else
      print *, 'Loading ok...'
    end if
    print *, 'All defined? ', pars%all_defined()
    call pars%write2unit(output_unit, include_undefined=.true.)

    print *, 'Copy? '
    pars2 = pars
    call pars2%write2unit(output_unit)
    call pars2%print()
    stop 0


    print *, 'old pars after assignment'
    call pars%print()
    print *, 'new pars '
    call pars2%print()

    print *
    call pars%write2unit(output_unit)
    print *
    call pars2%write2unit(output_unit)

  end program test_localpars
