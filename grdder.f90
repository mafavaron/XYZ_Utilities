! Extract a spatial rectangular subset from an XYZ file
!
! By: Patrizia Favaron

program grdder
    
    use grid_gen
    
    implicit none
    
    ! Locals
    integer                             :: iRetCode
    real(8)                             :: rX
    real(8)                             :: rY
    real(8)                             :: rXmin
    real(8)                             :: rYmin
    real(8)                             :: rXmax
    real(8)                             :: rYmax
    integer                             :: iNumLines
    integer                             :: iLine
    character(len=256)                  :: sInFile
    character(len=256)                  :: sOutFile
    character(len=256)                  :: sBuffer
    real(8), dimension(:), allocatable  :: rvX
    real(8), dimension(:), allocatable  :: rvY
    real(8), dimension(:), allocatable  :: rvConc
    
    ! Get parameters
    if(command_argument_count() /= 6) then
        print *, "grdder - Procedure for extracting a rectangular block"
        print *, "         from an XYZ CSV file and converting it to"
        print *, "         a Surfer GRD file"
        print *
        print *, "Usage:"
        print *
        print *, "  grdder <InFile> <Xmin> <Ymin> <Xmax> <Ymax> <GrdFile>"
        print *
        print *, "Copyright 2020 by Servizi Territorio srl"
        print *, "                  All rights reserved"
        print *
        stop
    end if
    call get_command_argument(1, sInFile)
    call get_command_argument(2, sBuffer)
    read(sBuffer, *, iostat=iRetCode) rXmin
    if(iRetCode /= 0) then
        print *, "Error: Xmin is invalid"
        stop
    end if
    call get_command_argument(3, sBuffer)
    read(sBuffer, *, iostat=iRetCode) rYmin
    if(iRetCode /= 0) then
        print *, "Error: Ymin is invalid"
        stop
    end if
    call get_command_argument(4, sBuffer)
    read(sBuffer, *, iostat=iRetCode) rXmax
    if(iRetCode /= 0) then
        print *, "Error: Xmax is invalid"
        stop
    end if
    call get_command_argument(5, sBuffer)
    read(sBuffer, *, iostat=iRetCode) rYmax
    if(iRetCode /= 0) then
        print *, "Error: Ymax is invalid"
        stop
    end if
    call get_command_argument(6, sOutFile)
    
    ! Perform the desired selection
    
    ! Get input file
    open(10, file=sInFile, status='old', action='read', iostat=iRetCode)
    if(iRetCode /= 0) then
        print *, 'Error: Input file not opened'
        stop
    end if
    read(10, "(a)", iostat=iRetCode) sBuffer
    if(iRetCode /= 0) then
        print *, 'Error: Input file is empty'
        stop
    end if
    iNumLines = 0
    do
        read(10, "(a)", iostat=iRetCode) sBuffer
        if(iRetCode /= 0) exit
        read(sBuffer, *, iostat=iRetCode) rX, rY
        if(iRetCode /= 0) then
            print *, 'Error: Input file is invalid'
            stop
        end if
        if(rXmin <= rX .and. rX <= rXmax .and. rYmin <= rY .and. rY <= rYmax) then
            iNumLines = iNumLines + 1
        end if
    end do
    if(iNumLines <= 0) then
        print *, 'Error: Input file contains no useful lines'
        stop
    end if
    
    ! Reserve workspace and get actual lines
    rewind(10)
    allocate(rvX(iNumLines), rvY(iNumLines), rvConc(iNumLines))
    iLine = 0
    do
        read(10, "(a)", iostat=iRetCode) sBuffer
        if(iRetCode /= 0) exit
        read(sBuffer, *) rX, rY
        if(rXmin <= rX .and. rX <= rXmax .and. rYmin <= rY .and. rY <= rYmax) then
            iLine = iLine + 1
            read(sBuffer, *) rvX(iLine), rvY(iLine), rvConc(iLine)
        end if
    end do
    close(10)
    
    open(11, file=sOutFile, status='unknown', action='write', iostat=iRetCode)
    if(iRetCode /= 0) then
        print *, 'Error: Output file not opened'
        stop
    end if
    close(11)
    
    ! Leave
    deallocate(rvX, rvY, rvConc)
    
end program grdder
