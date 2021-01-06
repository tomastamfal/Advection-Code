!*********************************************************************************************************
!**                                     Developped by Tomas Tamfal 					**
!**                                       Version 0.1 January 2018	                               	**
!*********************************************************************************************************


#include "choice.def"
subroutine reconstruct_y(u)

!**********************************************************************************************************
	use Parameters
	use Variables	

!**********************************************************************************************************
	implicit none
	integer :: i, j
	real(8) :: r, psi, slope, a , b, phi, slope_left, slope_right, minmodslope, maxslope


#ifdef ONE
        real(8) :: u
#endif !ONE


#ifdef TWO
        real(8) :: u(1:x_end,1:y_end) 
#ifdef Fconst
            do i=1,x_end
                do j=1,y_end
                    U_left_y(i,j)  = u(i,j)   
                    U_right_y(i,j) = u(i,j) 
                enddo          
                enddo
#endif !Fconst



#ifdef Fcentraldiff
            do i =2,x_end-1
                do j = 2, y_end-1 
                        slope_left      = (u(i,j) - u(i,j-1) ) / dx(5)
                        slope_right     = (u(i,j+1) - u(i,j) ) / dx(5)

                        slope           = (slope_left + slope_right ) / 2D0

                        U_left_y(i,j)  = u(i,j) - slope *dx(5)/2D0
                        U_right_y(i,j) = u(i,j) + slope *dx(5)/2D0
                enddo
            enddo 
#endif !Fcentraldiff
 

#ifdef upstream
            do i =2,x_end-1
                do j = 2, y_end-1
                        slope      =  (u(i,j) - u(i,j-1) ) / (dx(5))

                        U_left_y(i,j)  = u(i,j) - slope *dx(5)/2D0
                        U_right_y(i,j) = u(i,j) + slope *dx(5)/2D0
                enddo 
            enddo 
#endif !upstream


#ifdef downstream
            do i =2,x_end-1
                do j = 2, y_end-1
                        slope      =  (u(i,j+1) - u(i,j) ) / (dx(5))

                        U_left_y(i,j)  = u(i,j) - slope *dx(5)/2D0
                        U_right_y(i,j) = u(i,j) + slope *dx(5)/2D0
                enddo 
            enddo 

#endif !downstream




#ifdef Flim
            do i =2,x_end-1
                do j = 2,y_end-1
                        a = u(i,j) - u(i,j-1)
                        b = u(i,j+1) - u(i,j)
                        
                        slope_left      = a / dy(5)
                        slope_right     = b / dy(5)

                        slope           = (slope_left + slope_right ) / 2D0
                        if (b==0D0) then 
                                U_left_y(i,j)  = u(i,j) 
                                U_right_y(i,j) = u(i,j)   
                        elseif (a==0D0) then 
                                U_left_y(i,j)  = u(i,j) 
                                U_right_y(i,j) = u(i,j)    
                        else 
                                r = a / b

                                if (r < 0) then
                                        phi = 0D0
                                        slope = 0D0

                                else 
#ifdef Minmod
                                        phi = 1
                                        slope = SIGN(1D0,slope_left) * min(ABS(slope_left), ABS(slope_right))
#endif !Minmod

#ifdef new 
                                        phi = 1
                                        minmodslope = SIGN(1D0,slope_left) * min(ABS(slope_left), ABS(slope_right))    
                                        maxslope    = 2D0*minmodslope 
                                        slope       = (slope_left + slope_right ) / 2D0
                                        slope       = SIGN(1D0,slope) * min(ABS(slope), ABS(maxslope))
  

                                             
#endif !new
                                endif
                                U_left_y(i,j)  = u(i,j) - phi*slope*dy(5)/2D0
                                U_right_y(i,j) = u(i,j) + phi*slope*dy(5)/2D0
                        endif
                    enddo
                enddo
                

#endif !Flim
#endif !TWO








return
end subroutine reconstruct_y

