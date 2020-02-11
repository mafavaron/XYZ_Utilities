! Test driver for grid_gen.f90 module
program test

    use grid_gen
    use compare
    
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
    real(8)                     :: rFB
    real(8)                     :: rNMSE
    real(8)                     :: rGM
    real(8)                     :: rGV
    real(8)                     :: rFAC2
    real(8)                     :: rNAD
    
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
            rvConc(k) = 100.d0 * exp(-((rvY(k)-5.d0)/5.d0)**2 - (rvX(k)/7.d0)**2)
        end do
    end do
    iRetCode = tGrid2 % build(rvX, rvY, rvConc)
    print *
    print *, "Test 2: Data number = ", k
    print *, "Test 2: Ret code from 'build'     = ", iRetCode
    sOutputFile = ".\\out2.grd"
    iRetCode = tGrid2 % fileWrite(10, sOutputFile)
    print *, "Test 2: Ret code from 'fileWrite' = ", iRetCode

    ! Test 3: get grid from XYZ information (different than in test 2), then write it in a way allowing test from Surfer
    k = 0
    do i = 1, 101
        do j = 1, 101
            k = k + 1
            rvX(k) = 0.2d0 * (i-1) - 10.d0
            rvY(k) = 0.2d0 * (j-1) - 10.d0
            rvConc(k) = 100.d0 * exp(-((rvY(k))/5.d0)**2 - ((rvX(k)-5.d0)/7.d0)**2)
        end do
    end do
    iRetCode = tGrid2 % build(rvX, rvY, rvConc)
    print *
    print *, "Test 3: Data number = ", k
    print *, "Test 3: Ret code from 'build'     = ", iRetCode
    sOutputFile = ".\\out2.grd"
    iRetCode = tGrid2 % fileWrite(10, sOutputFile)
    print *, "Test 3: Ret code from 'fileWrite' = ", iRetCode
    
    ! Test 4: comparer, two identical series
    call random_number(rvX)
    rvY   = rvX
    rFB   = FB(rvX, rvY)
    rNMSE = NMSE(rvX, rvY)
    rGM   = GM(rvX, rvY)
    rGV   = GV(rvX, rvY)
    rFAC2 = FAC2(rvX, rvY)
    rNAD  = NAD(rvX, rvY)
    print *
    print *, "Test 4: Comparisons, two identical series"
    print *, "  FB    = ", rFB
    print *, "  NMSE  = ", rNMSE
    print *, "  GM    = ", rGM
    print *, "  GV    = ", rGV
    print *, "  FAC2  = ", rFAC2
    print *, "  NAD   = ", rNAD

end program test

