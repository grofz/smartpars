  program test_localpars
    use iso_fortran_env, only : output_unit
    use localpars_mod, only : pars_t => localpars_t
    implicit none

    type(pars_t) :: pars, pars2

    print *, 'Hello from main test'
    call pars%init(10,10)
    call pars%loadfromfile('myfile.txt')
    call pars%setdefvals()
    print *, 'All defined? ', pars%all_defined()
    call pars%write2unit(output_unit)
    !call pars%print()

    print *
    print *, 'model ', pars%model
    print *, 'typ   ', pars%typ
    print *, 'vals  ', pars%vals
    print *, 'dif1  ', pars%dif1
    print *, 'koefs  ', pars%koef
    stop 0

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
