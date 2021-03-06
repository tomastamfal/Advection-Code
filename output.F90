!**********************************************************************************************************
!**                       ROSSBI : Rotating Systems Simulation Code for Bi-fluids                        **
!**                                   Developped by Clement SURVILLE                                     **
!**                                       Version 2.3.2 July 2016                                        **
!**********************************************************************************************************

!**********************************************************************************************************
!            Output_data.F90:  Outputs the intensive variables in .data files
!**********************************************************************************************************

#include "choice.def"
subroutine output

!**********************************************************************************************************
	use Parameters
	use Variables	




!**********************************************************************************************************
	implicit none
	integer :: i, j
	real(8) :: read_buff
	character(len=3)  :: cjunk
	character(len=25) :: cfile
	character(len=25) :: fm1 = '(e20.10)'


!**********************************************************************************************************
!print *, 'Outputting data'

!******************************** Time output ********************************************
	open(20, file ='time.data', status ='old')
	do i = 1, ifile-1
		read(20, fm1) read_buff
	enddo
	write(20, fm1) time
	close(20)


!**********************************************************************************************************
#ifdef ONE
!**********************************************************************************************************

	write(cjunk,'(i3.3)') ifile

	cfile = cjunk(1:3)//'_gas.data.txt'
	open(10, file = cfile, status='replace')
	do i = 1, x_end
			write(10,*) rho_center(i)
	enddo

	close(10)



#endif !ONE


!**********************************************************************************************************
#ifdef TWO
!**********************************************************************************************************

	write(cjunk,'(i3.3)') ifile

	cfile = cjunk(1:3)//'_gas.data.txt'
	open(10, file = cfile, status='replace')
	do i = 1, x_end 
			write(10,*) rho_center(i,:)
	enddo

	close(10)



#endif !TWO


    
return
!**********************************************************************************************************
end subroutine output
!**********************************************************************************************************
