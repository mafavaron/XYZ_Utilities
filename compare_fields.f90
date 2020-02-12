program compare_fields

    use compare
    use grid_gen
    
    implicit none
    
    ! Routine arguments
    integer                             :: iRetCode
    character(len=256)                  :: sInFileA
    character(len=256)                  :: sInFileB
    character(len=256)                  :: sOutFile
    type(grid_space)                    :: tGridA
    type(grid_space)                    :: tGridB
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
        print *, "compare_fields - Utility for comparing two fields of equal number of points"
        print *
        print *, "Usage:"
        print *
        print *, "  compare_fields <File_Series_A> <File_Series_B> <Results_File>"
        print *
        print *, "Copyright 2020 by Servizi Territorio srl"
        print *, "                  All rights reserved"
        print *
        stop
    end if
    call get_command_argument(1, sInFileA)
    call get_command_argument(2, sInFileB)
    call get_command_argument(3, sOutFile)
    
    ! Read the two fields
    iRetCode = tGridA % fileRead(10, sInFileA)
    if(iRetCode /= 0) then
        print *, "Error: Input file A not read - Return code = ", iRetCode
        stop
    end if
    iRetCode = tGridB % fileRead(10, sInFileB)
    if(iRetCode /= 0) then
        print *, "Error: Input file B not read - Return code = ", iRetCode
        stop
    end if
    
    ! Get the data vectors from fields
    iRetCode = tGridA % getData(rvA)
    if(iRetCode /= 0) then
        print *, "Error: Data not extracted from grid A - Return code = ", iRetCode
        stop
    end if
    iRetCode = tGridB % getData(rvB)
    if(iRetCode /= 0) then
        print *, "Error: Data not extracted from grid B - Return code = ", iRetCode
        stop
    end if
    
    ! Check the two fields have equal sizes (they should, for all
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

end program compare_fields
