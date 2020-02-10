! Test driver for grid_gen.f90 module
program test

    use grid_gen
    
    implicit none
    
    ! Locals
    character(len=256)  :: sInputFile
    type(grid_space)    :: tGrid1
    character(len=256)  :: sOutputFile
    type(grid_space)    :: tGrid2
    integer             :: iRetCode
    
    ! Test 1: read file, then write it with a different name. Should give the same map under Surfer
    sInputFile = ".\\out.grd"
    iRetCode = tGrid1 % fileRead(10, sInputFile)
    print *, "Test 1, step 1: Ret code = ", iRetCode
    sOutputFile = ".\\out1.grd"
    iRetCode = tGrid1 % fileWrite(10, sOutputFile)
    print *, "Test 1, step 2: Ret code = ", iRetCode

end program test

