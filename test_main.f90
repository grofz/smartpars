  program test_localpars
    use iso_fortran_env, only : output_unit
    use localpars_mod, only : pars_t => localpars_t
    implicit none

    type(pars_t) :: pars, pars2

    print *, 'Hello from main test'
    call pars%init(10,10)
    call pars%setdefvals()

    print *, size(pars%vals)


    print *, 'old pars before assignment'
    call pars%print()
    call pars2%print()

    pars2 = pars
    pars2%model = 30

    print *, 'old pars after assignment'
    call pars%print()
    print *, 'new pars '
    call pars2%print()

    print *
    call pars%write2unit(output_unit)
    print *
    call pars2%write2unit(output_unit)

  end program test_localpars
