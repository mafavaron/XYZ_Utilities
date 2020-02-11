program compare_series

    use compare
    
    implicit none
    
    ! Routine arguments
    integer                             :: iRetCode
    character(len=256)                  :: sInFileA
    character(len=256)                  :: sInFileB
    character(len=256)                  :: sOutFile
    real(8), dimension(:), allocatable  :: rvA
    real(8), dimension(:), allocatable  :: rvB
    real(8)                             :: rFB
    real(8)                             :: rNMSE
    real(8)                             :: rGM
    real(8)                             :: rGV
    real(8)                             :: rFAC2
    real(8)                             :: rNAD
    
    ! Get parameters
    if(command_argument_count() /= 3) then
        print *, "compare_series - Utility for comparing two time series of equal length"
        print *
        print *, "Usage:"
        print *
        print *, "  compare_series <File_Series_A> <File_Series_B> <Results_File>"
        print *
        print *, "Copyright 2020 by Servizi Territorio srl"
        print *, "                  All rights reserved"
        print *
        stop
    end if
    call get_command_argument(1, sInFileA)
    call get_command_argument(2, sInFileB)
    call get_command_argument(3, sOutFile)
    
    ! Read the two series
    iRetCode = read_series(10, sInFileA, rvA)
    if(iRetCode /= 0) then
        print *, "Error: Input file A not read - Return code = ", iRetCode
        stop
    end if
    iRetCode = read_series(10, sInFileB, rvB)
    if(iRetCode /= 0) then
        print *, "Error: Input file B not read - Return code = ", iRetCode
        stop
    end if
    
    ! Check the two series have equal lengths (they should, for all
    ! following calculations to make sense)
    if(size(rvA) /= size(rvB)) then
        print *, "Error: The two data series have different lengths"
        stop
    end if
    
    ! Compute the standard indices and write them to output
    rFB   = FB(rvA, rvB)
    rNMSE = NMSE(rvA, rvB)
    rGM   = GM(rvA, rvB)
    rGV   = GV(rvA, rvB)
    rFAC2 = FAC2(rvA, rvB)
    rNAD  = NAD(rvA, rvB)
    open(10, file=sOutFile, status='unknown', action='write')
    write(10, "('Report for series from files:')")
    write(10, "('  Series A from ', a)") trim(sInFileA)
    write(10, "('  Series B from ', a)") trim(sInFileB)
    write(10, "(' ')")
    write(10, "('Standard comparison indicators:')")
    write(10, "('FB   = ', f11.5)") rFB
    write(10, "('NMSE = ', f11.5)") rNMSE
    write(10, "('GM   = ', f11.5)") rGM
    write(10, "('GV   = ', f11.5)") rGV
    write(10, "('FAC2 = ', f11.5)") rFAC2
    write(10, "('NAD  = ', f11.5)") rNAD
    close(10)

contains

    function read_series(iLUN, sFileName, rvX) result(iRetCode)
    
        ! Routine arguments
        integer, intent(in)                             :: iLUN
        character(len=*), intent(in)                    :: sFileName
        real(8), dimension(:), allocatable, intent(out) :: rvX
        integer                                         :: iRetCode
        
        ! Locals
        integer             :: iNumLines
        integer             :: iLine
        integer             :: iErrCode
        character(len=256)  :: sBuffer
        
        ! Assume success
        iRetCode = 0
        
        ! Clean-out
        if(allocated(rvX)) deallocate(rvX)
        
        ! Count data
        iNumLines = -1  ! Take header line into account
        open(iLUN, file=sFileName, status='old', action='read', iostat=iErrCode)
        if(iErrCode /= 0) then
            iRetCode = 1
            return
        end if
        do
            read(iLUN, "(a)", iostat=iErrCode) sBuffer
            if(iErrCode /= 0) exit
            iNumLines = iNumLines + 1
        end do
        if(iNumLines <= 0) then
            iRetCode = 2
            close(iLUN)
            return
        end if
        
        ! Reserve workspace and read actual data into it, assuming a CondDecode-produced series
        allocate(rvX(iNumLines))
        rewind(iLUN)
        read(iLUN, "(a)") sBuffer   ! Skip header line
        do iLine = 1, iNumLines
            read(iLUN, "(a)") sBuffer
            read(sBuffer(21:), *) rvX(iLINE)
        end do
        
        ! Leave
        close(iLUN)
        
    end function read_series

end program compare_series
