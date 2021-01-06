!*********************************************************************************************************
!**                                     Developped by Tomas Tamfal 					**
!**                                       Version 0.1 January 2018	                               	**
!*********************************************************************************************************


#include "choice.def"
subroutine reconstruct_x(u)

!**********************************************************************************************************
	use Parameters
	use Variables	

!**********************************************************************************************************
	implicit none
	integer :: i, j
	real(8) :: r, psi, slope, a , b, phi, slope_left, slope_right, minmodslope , maxslope
#ifdef ONE
        real(8) :: u(1:x_end)
#endif !ONE
#ifdef TWO
        real(8) :: u(1:x_end,1:y_end)
#endif !TWO


!**********************************************************************************************************
#ifdef ONE
!**********************************************************************************************************

#ifdef Fconst
            do i=1,x_end
                    U_left(i)  = u(i)   
                    U_right(i) = u(i)           
            enddo
#endif !Fconst



#ifdef Fcentraldiff
            do i =2,x_end-1
                slope_left      = (u(i) - u(i-1) ) / dx(5)
                slope_right     = (u(i+1) - u(i) ) / dx(5)

                slope           = (slope_left + slope_right ) / 2D0

                U_left(i)  = u(i) - slope *dx(5)/2D0
                U_right(i) = u(i) + slope *dx(5)/2D0
            enddo 
#endif !Fcentraldiff
 

#ifdef upstream
            do i =2,x_end-1
                slope      =  (u(i) - u(i-1) ) / (dx(5))

                U_left(i)  = u(i) - slope *dx(5)/2D0
                U_right(i) = u(i) + slope *dx(5)/2D0
            enddo 
#endif !upstream


#ifdef downstream
            do i =2,x_end-1
                slope      =  (u(i+1) - u(i) ) / (dx(5))

                U_left(i)  = u(i) - slope *dx(5)/2D0
                U_right(i) = u(i) + slope *dx(5)/2D0
            enddo 
#endif !downstream




#ifdef Flim
            do i =2,x_end-1
                a = u(i) - u(i - 1)
                b = u(i + 1) - u(i)
                
                slope_left      = a / dx(5)
                slope_right     = b / dx(5)

                slope           = (slope_left + slope_right ) / 2D0
                if (b==0D0) then 
                        U_left(i)  = u(i) 
                        U_right(i) = u(i)   
                elseif (a==0D0) then 
                        U_left(i)  = u(i) 
                        U_right(i) = u(i)    
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
                                        slope       = min(slope, maxslope)
                                                      
#endif !new
                        endif
                        U_left(i)  = u(i) - phi*slope*dx(5)/2D0
                        U_right(i) = u(i) + phi*slope*dx(5)/2D0
                endif
            enddo
                

#endif !Flim
#endif !ONE









!**********************************************************************************************************
#ifdef TWO
!**********************************************************************************************************

#ifdef Fconst
            do i=1,x_end
                do j=1,y_end
                    U_left_x(i,j)  = u(i,j)   
                    U_right_x(i,j) = u(i,j) 
                enddo          
                enddo
#endif !Fconst



#ifdef Fcentraldiff
            do i =2,x_end-1
                do j = 2, y_end-1 
                        slope_left      = (u(i,j) - u(i-1,j) ) / dx(5)
                        slope_right     = (u(i+1,j) - u(i,j) ) / dx(5)

                        slope           = (slope_left + slope_right ) / 2D0

                        U_left_x(i,j)  = u(i,j) - slope *dx(5)/2D0
                        U_right_x(i,j) = u(i,j) + slope *dx(5)/2D0
                enddo
            enddo 
#endif !Fcentraldiff
 

#ifdef upstream
            do i =2,x_end-1
                do j = 2, y_end-1
                        slope      =  (u(i,j) - u(i-1,j) ) / (dx(5))

                        U_left_x(i,j)  = u(i,j) - slope *dx(5)/2D0
                        U_right_x(i,j) = u(i,j) + slope *dx(5)/2D0
                enddo 
            enddo 
#endif !upstream


#ifdef downstream
            do i =2,x_end-1
                do j = 2, y_end-1
                        slope      =  (u(i+1,j) - u(i,j) ) / (dx(5))

                        U_left_x(i,j)  = u(i,j) - slope *dx(5)/2D0
                        U_right_x(i,j) = u(i,j) + slope *dx(5)/2D0
                enddo 
            enddo 

#endif !downstream




#ifdef Flim
            do i =2,x_end-1
                do j = 2,y_end-1
                        a = u(i,j) - u(i - 1,j)
                        b = u(i + 1,j) - u(i,j)
                        
                        slope_left      = a / dx(5)
                        slope_right     = b / dx(5)

                        slope           = (slope_left + slope_right ) / 2D0
                        if (b==0D0) then 
                                U_left_x(i,j)  = u(i,j) 
                                U_right_x(i,j) = u(i,j)   
                        elseif (a==0D0) then 
                                U_left_x(i,j)  = u(i,j) 
                                U_right_x(i,j) = u(i,j)    
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
                                U_left_x(i,j)  = u(i,j) - phi*slope*dx(5)/2D0
                                U_right_x(i,j) = u(i,j) + phi*slope*dx(5)/2D0
                        endif
                    enddo
                enddo
                

#endif !Flim
#endif !TWO








return
end subroutine reconstruct_x

