        rvX(i) = 0.1d0 * (i-1) - 10.d0
! Test driver for grid_gen.f90 module
program test

    use grid_gen
    
    implicit none
    
    ! Locals
    character(len=256)          :: sInputFile
    type(grid_space)            :: tGrid1
    character(len=256)          :: sOutputFile
    type(grid_space)            :: tGrid2
    integer                     :: iRetCode
    real(8), dimension(101)     :: rvX
    real(8), dimension(101)     :: rvY
    real(8), dimension(10201)   :: rvConc
    integer                     :: i
    integer                     :: j
    
    ! Test 1: read file, then write it with a different name. Should give the same map under Surfer
    sInputFile = ".\\out.grd"
    iRetCode = tGrid1 % fileRead(10, sInputFile)
    print *, "Test 1, step 1: Ret code = ", iRetCode
    sOutputFile = ".\\out1.grd"
    iRetCode = tGrid1 % fileWrite(10, sOutputFile)
    print *, "Test 1, step 2: Ret code = ", iRetCode
    
    ! Test 2: get grid from XYZ information, then write it in a way allowing test from Surfer
    do i = 1, 101
        rvX(i) = 0.2d0 * (i-1) - 10.d0
        rvY(i) = 0.2d0 * (i-1) - 10.d0
    end do
    print *, "Test 2: X and Y range = ", minval(rvX), maxval(rvY)

end program test

