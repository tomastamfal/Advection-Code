!*********************************************************************************************************
!**                                     Developped by Tomas Tamfal 					**
!**                                       Version 0.1 January 2018	                               	**
!*********************************************************************************************************


#include "choice.def"

program advection
!**********************************************************************************************************

        use parameters
        use variables

        integer :: i,j,k,kk

#ifdef TWO
        real(8) :: tmp_x, tmp_y
#endif !TWO


        ifile = 1

        !**************************** Creating mesh **********************************************
        call mesh   
	!**************************** Creating density *******************************************
        call initialsetup
	!**************************** Creating first output **************************************
        call output  

!**********************************************************************************************************
#ifdef ONE
!**********************************************************************************************************
        !**************************** Main program ***********************************************
        time = 0D0
        do k = 1,maxsteps

        !**************************** Calculating Runge Kutta **********************************
                call reconstruct_x(rho_center)
                call periodic(U_left)
                call periodic(U_right)

                call fluxcalc_x(U_left, U_right)
                call periodic(F_left)
                call periodic(F_right)


                do i =1,x_end
                        rho_hstep(i) = rho_center(i) - ((0.5D0 * dt/dx(5)) * (F_right(i)-F_left(i)) )
                enddo
                call periodic(rho_hstep)

                call reconstruct_x(rho_hstep)
                call periodic(U_left)
                call periodic(U_right)

                call fluxcalc_x(U_left, U_right)
                call periodic(F_left)
                call periodic(F_right)
                
                call riemann_x(F_left, F_right)
                call periodic(F_riemann_left)
                call periodic(F_riemann_right)


                do i =1,x_end
                        rho_center(i) = rho_center(i) - ( (dt/dx(5))  * (F_riemann_right(i)-F_riemann_left(i)) )
                enddo
                call periodic(rho_center) 
          
           

        !**************************** Updating the time ******************************************
        	call timestep
                time = time + dt 
        !**************************** Creating output every "freq_output" ************************
                if (MOD(k , freq_output) == 0) then
                        ifile = ifile + 1
                        call output
                endif
        enddo
#endif !ONE






!**********************************************************************************************************
#ifdef TWO
!**********************************************************************************************************
        !**************************** Main program ***********************************************
        time = 0D0
        do k = 1,maxsteps
                if (MOD(k,2) ==0) then
                !**************************** Calculating Runge Kutta **********************************
                        call reconstruct_x(rho_center)
                        call periodic(U_left_x)
                        call periodic(U_right_x)

                        call reconstruct_y(rho_center)
                        call periodic(U_left_y)
                        call periodic(U_right_y)



                        call fluxcalc_x(U_left_x, U_right_x)
                        call periodic(F_left_x)
                        call periodic(F_right_x)

                        call fluxcalc_y(U_left_y, U_right_y)
                        call periodic(F_left_y)
                        call periodic(F_right_y)

                        do i =1,x_end
                                do j=1,y_end-1 
                                        tmp_x = 0.5D0 * dt/(dy(5)*dx(5)) * (F_right_x(i,j)-F_left_x(i,j))
                                        tmp_y = 0.5D0 * dt/(dy(5)*dx(5)) * (F_right_y(i,j)-F_left_y(i,j)) 

                                        rho_hstep(i,j) = rho_center(i,j) - tmp_x -tmp_y
                                enddo
                        enddo
                        call periodic(rho_hstep)


                        call reconstruct_x(rho_hstep)
                        call periodic(U_left_x)
                        call periodic(U_right_x)

                        call reconstruct_y(rho_hstep)
                        call periodic(U_left_y)
                        call periodic(U_right_y)


                        call fluxcalc_x(U_left_x, U_right_x)
                        call periodic(F_left_x)
                        call periodic(F_right_x)

                        call fluxcalc_y(U_left_y, U_right_y)
                        call periodic(F_left_y)
                        call periodic(F_right_y)

                        
                        call riemann_x(F_left_x, F_right_x)
                        call periodic(F_riemann_left_x)
                        call periodic(F_riemann_right_x)
                        
                        call riemann_y(F_left_y, F_right_y)
                        call periodic(F_riemann_left_y)
                        call periodic(F_riemann_right_y)


                        do i =1,x_end
                                do j=1,y_end 
                                        tmp_x = dt/(dy(5)*dx(5)) * (F_riemann_right_x(i,j)-F_riemann_left_x(i,j))
                                        tmp_y = dt/(dy(5)*dx(5)) * (F_riemann_right_y(i,j)-F_riemann_left_y(i,j)) 

                                        rho_center(i,j) = rho_center(i,j) - tmp_x -tmp_y
                                enddo
                        enddo
                        call periodic(rho_center) 
          
                else
                !**************************** Calculating Runge Kutta **********************************
                        call reconstruct_y(rho_center)
                        call periodic(U_left_y)
                        call periodic(U_right_y)

                        call reconstruct_x(rho_center)
                        call periodic(U_left_x)
                        call periodic(U_right_x)


                        call fluxcalc_y(U_left_y, U_right_y)
                        call periodic(F_left_y)
                        call periodic(F_right_y)


                        call fluxcalc_x(U_left_x, U_right_x)
                        call periodic(F_left_x)
                        call periodic(F_right_x)


                        do i =1,x_end
                                do j=1,y_end-1 
                                        tmp_x = 0.5D0 * dt/(dy(5)*dx(5)) * (F_right_x(i,j)-F_left_x(i,j))
                                        tmp_y = 0.5D0 * dt/(dy(5)*dx(5)) * (F_right_y(i,j)-F_left_y(i,j)) 

                                        rho_hstep(i,j) = rho_center(i,j) - tmp_x -tmp_y
                                enddo
                        enddo
                        call periodic(rho_hstep)

                        call reconstruct_y(rho_hstep)
                        call periodic(U_left_y)
                        call periodic(U_right_y)

                        call reconstruct_x(rho_hstep)
                        call periodic(U_left_x)
                        call periodic(U_right_x)


                        call fluxcalc_y(U_left_y, U_right_y)
                        call periodic(F_left_y)
                        call periodic(F_right_y)


                        call fluxcalc_x(U_left_x, U_right_x)
                        call periodic(F_left_x)
                        call periodic(F_right_x)

                        call riemann_y(F_left_y, F_right_y)
                        call periodic(F_riemann_left_y)
                        call periodic(F_riemann_right_y)
                       
                        call riemann_x(F_left_x, F_right_x)
                        call periodic(F_riemann_left_x)
                        call periodic(F_riemann_right_x)
                        

                        do i =1,x_end
                                do j=1,y_end-1 
                                        tmp_x = dt/(dy(5)*dx(5)) * (F_riemann_right_x(i,j)-F_riemann_left_x(i,j))
                                        tmp_y = dt/(dy(5)*dx(5)) * (F_riemann_right_y(i,j)-F_riemann_left_y(i,j)) 

                                        rho_center(i,j) = rho_center(i,j) - tmp_x -tmp_y
                                enddo
                        enddo
                        call periodic(rho_center) 

                endif          

        !**************************** Updating the time ******************************************
        	call timestep
                time = time + dt 
        !**************************** Creating output every "freq_output" ************************
                if (MOD(k , freq_output) == 0) then
                        ifile = ifile + 1
                        call output
                endif
        enddo
#endif !TWO







end program advection
