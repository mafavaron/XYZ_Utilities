! Test driver for grid_gen.f90 module
program test

    use grid_gen
    
    implicit none
    
    ! Locals
    character(len=256)  :: sInputFile
    type(grid_space)    :: tGrid1
    type(grid_space)    :: tGrid2
    integer             :: iRetCode
    
    ! Test 1: read file
    sInputFile = ".\\out.grd"
    iRetCode = tGrid1 % fileRead(10, sInputFile)
    print *, "Test 1: Ret code = ", iRetCode

end program test

