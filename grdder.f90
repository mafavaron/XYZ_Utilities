! Extract a spatial rectangular subset from an XYZ file
!
! By: Patrizia Favaron

program grdder

	implicit none
	
	! Locals
	integer				:: iRetCode
	real(8)				:: rX
	real(8)				:: rY
	real(8)				:: rXmin
	real(8)				:: rYmin
	real(8)				:: rXmax
	real(8)				:: rYmax
	character(len=256)	:: sInFile
	character(len=256)	:: sOutFile
	character(len=256)	:: sBuffer
	
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
	
	! -1- Access files
	open(10, file=sInFile, status='old', action='read', iostat=iRetCode)
	if(iRetCode /= 0) then
		print *, 'Error: Input file not opened'
		stop
	end if
	open(11, file=sOutFile, status='unknown', action='write', iostat=iRetCode)
	if(iRetCode /= 0) then
		print *, 'Error: Output file not opened'
		stop
	end if
	
	! -1- Transfer the first line unchanged
	read(10, "(a)", iostat=iRetCode) sBuffer
	if(iRetCode /= 0) then
		print *, 'Error: Input file is empty'
		stop
	end if
	write(11, "(a)") trim(sBuffer)
	
	! -1- Main loop: actual data transfer
	do
		read(10, "(a)", iostat=iRetCode) sBuffer
		if(iRetCode /= 0) exit
		read(sBuffer, *, iostat=iRetCode) rX, rY
		if(iRetCode /= 0) then
			print *, 'Error: Input file is invalid'
			stop
		end if
		if(rXmin <= rX .and. rX <= rXmax .and. rYmin <= rY .and. rY <= rYmax) then
			write(11, "(a)") trim(sBuffer)
		end if
	end do
	
	! -1- Leave
	close(11)
	close(10)

end program grdder
