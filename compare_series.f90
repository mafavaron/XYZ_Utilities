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
        
    end function read_series

end program compare_series
