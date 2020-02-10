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
    real(8), dimension(10201)   :: rvX
    real(8), dimension(10201)   :: rvY
    real(8), dimension(10201)   :: rvConc
    integer                     :: i
    integer                     :: j
    integer                     :: k
    
    ! Test 1: read file, then write it with a different name. Should give the same map under Surfer
    sInputFile = ".\\out.grd"
    iRetCode = tGrid1 % fileRead(10, sInputFile)
    print *, "Test 1, step 1: Ret code = ", iRetCode
    sOutputFile = ".\\out1.grd"
    iRetCode = tGrid1 % fileWrite(10, sOutputFile)
    print *, "Test 1, step 2: Ret code = ", iRetCode
    
    ! Test 2: get grid from XYZ information, then write it in a way allowing test from Surfer
    k = 0
    do i = 1, 101
        do j = 1, 101
            k = k + 1
            rvX(k) = 0.2d0 * (i-1) - 10.d0
            rvY(k) = 0.2d0 * (j-1) - 10.d0
            rvConc(k) = 100.d0 * exp(-(-(rvY(k)-5.d0)/5.d0 - rvX(k)/7.d0)**2)
        end do
    end do
    iRetCode = tGrid2 % build(rvX, rvY, rvConc)
    print *
    print *, "Test 2: Data number = ", k
    print *, "Test 2: Ret code from 'build'     = ", iRetCode
    sOutputFile = ".\\out2.grd"
    iRetCode = tGrid2 % fileWrite(10, sOutputFile)
    print *, "Test 2: Ret code from 'fileWrite' = ", iRetCode

end program test

