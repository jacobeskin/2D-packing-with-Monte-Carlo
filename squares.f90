module squares
use mtdefs
use mtmod
! Module for defining the squares and setting up the initial configuration
  type :: square
     integer :: x, y, posx, posy ! 4 components, length, width and 2D position
  end type square

  contains

!---------------------------------------------------------------------------
!---------------------------------------------------------------------------
!---------------------------------------------------------------------------
!---------- Subroutine for initial configuration ---------------------------
!---------------------------------------------------------------------------
!---------------------------------------------------------------------------
 
    subroutine init_conf(n, sqrs)
      ! Makes the initial configuration
      implicit none
      integer, intent(in) :: n
      type(square), intent(out) :: sqrs(n)
      integer :: i, j, k, m
      
      ! Coordinate values for the corners of the squares, e.g
      ! ulx = upper left x- coordinate
      integer :: ulx, uly, urx, ury, brx, bry, blx, bly 

      ! Initializing the random number generator
      

      ! Generating the squares, random hight x and width y
      do i = 1,n
         sqrs(i)%x = igrnd(2,3)
         sqrs(i)%y = igrnd(2,3)
      end do

      ! Positioning the first one to a grid randomly. The position 
      !coordinates are the bottom left coordinates of the squares
      sqrs(1)%posx = igrnd(1,n) ! x-coordinate
      sqrs(1)%posy = igrnd(1,n) ! y-coordinate

      ! Positioning the rest, lets call the new sqare as sqr-i
      i = 2
      do 
         m = 0

         ! Bottom left coordinates, chosen randomly
         sqrs(i)%posx = igrnd(1,n)
         sqrs(i)%posy = igrnd(1,n)
         blx = sqrs(i)%posx 
         bly = sqrs(i)%posy
         ! Upper left coordinates
         ulx = sqrs(i)%posx
         uly = sqrs(i)%posy+sqrs(i)%y
         ! Bottom right coordinates
         brx = sqrs(i)%posx+sqrs(i)%x
         bry = sqrs(i)%posy
         ! Upper right coordinates
         urx = brx
         ury = uly

         ! Checking for overlap, lets call the already positioned squares sqr-j
         do j = 1,i-1

            ! 1st case: pos. coord. of sqr-i below pos. coord. of sqr-j
            if (bly<=sqrs(j)%posy) then
               
               ! If pos. coord. of i on left side of j:s
               if (blx<=sqrs(j)%posx) then
                  ! and if upper right coordinates are bigger than of j
                  ! pos.coord.
                  if ((urx>sqrs(j)%posx).and.(ury>sqrs(j)%posy)) then
                     ! we have overlap and need a new position
                     m = 1
                  end if
               end if

               ! If pos. coord. of i is between the bottom corners of j
               if ((blx>sqrs(j)%posx).and.(blx<(sqrs(j)%posx+sqrs(j)%x))) then
                  ! and uly is above j bottom y- coordinate
                  if (uly>sqrs(j)%posy) then
                     ! we have overlap and need a new position
                     m = 1
                  end if
               end if

               ! If position coordinates are the same
               if ((bly==sqrs(j)%posy).and.(blx==sqrs(j)%posx)) then
                  ! we have overlap and need a new position
                  m = 1
               end if

            end if

            ! 2nd case: pos. coord of sqr-i above pos. coord. of sqr-j
            if (bly>sqrs(j)%posy) then
              
               ! If pos. coord. of i is on the left side of j:s
               if (blx<=sqrs(j)%posx) then
                  ! and brx is below upper level of j and on the right side 
                  !of j:s pos. coord.
                  if ((brx>sqrs(j)%posx).and.(bry<(sqrs(j)%posy+sqrs(j)%y))) &
                       &then 
                     m = 1 ! we have overlap and need a new position
                  end if
               end if

               ! If pos. coord. of i is on the right side of j:s
               if (blx>sqrs(j)%posx) then
                  ! and is inside the sqr-i
                  if ((blx<(sqrs(j)%posx+sqrs(j)%x)).and.&
                       (bly<(sqrs(j)%posy+sqrs(j)%y)))& 
                  &then 
                     m = 1 ! we have overlap and need a new position
                  end if
               end if

            end if

         end do

         ! If no overlap, lets move to the next square
         if (m==0) then
            i = i+1 
         else if (m==1) then ! If overlap, new position
            cycle
         end if

         ! If we have positioned all the squares, we are done!
         if (i>n) then
            exit
         end if

      end do
    end subroutine init_conf

!---------------------------------------------------------------------------
!---------------------------------------------------------------------------
!---------------------------------------------------------------------------
!---------- Subroutine for new configuaritions -----------------------------
!---------------------------------------------------------------------------
!---------------------------------------------------------------------------

    subroutine new_conf(n,sqrs_old,sqrs_new)
      ! Subroutine for generating a new configuration of squares
      implicit none
      
      integer, intent(in) :: n
      type(square), intent(in) :: sqrs_old(n)
      type(square) :: sqrs(n)
      type(square), intent(out) :: sqrs_new(n) 
      integer :: i, j, k, l, m, x ,y 
      
      ! Coordinate values for the corners of the squares, e.g
      ! ulx = upper left x- coordinate
      integer :: ulx, uly, urx, ury, brx, bry, blx, bly 
      
      sqrs = sqrs_old
      
      ! Initializing the random number generator
      
      
      ! Choosing how to make the new configuration, 1 is movement, 2 is rotation
      ! and 3 is switching of squares, and then checking for overlap
      do
         m = 0
         
         ! Choosing the operation
         i = igrnd(1,3)


         ! If moving or rotating
         if ((i==1).or.(i==2)) then
            
            ! Choosing the square 
            j = igrnd(1,n)
            
            ! Now if moving, move one square at a time
            if (i==1) then
               ! Choose what way to move
               k = igrnd(1,4)
               ! And move, 1 is up, 2 rigth, 3 is down, 4 is left
               if (k==1) then
                  sqrs(j)%posy = sqrs(j)%posy+1
               else if (k==2) then
                  sqrs(j)%posx = sqrs(j)%posx+1
               else if (k==3) then
                  sqrs(j)%posy = sqrs(j)%posy-1
               else if (k==4) then
                  sqrs(j)%posx = sqrs(j)%posx-1
               end if
               ! or if rotating, then rotate 90 degrees clockwise
            else if (i==2) then
               sqrs(j)%posy = sqrs(j)%posy-sqrs(j)%x
            end if

            ! Check for overlap
            ! Bottom left coordinates
            blx = sqrs(j)%posx 
            bly = sqrs(j)%posy
            ! Upper left coordinates
            ulx = sqrs(j)%posx
            uly = sqrs(j)%posy+sqrs(j)%y
            ! Bottom right coordinates
            brx = sqrs(j)%posx+sqrs(j)%x
            bry = sqrs(j)%posy
            ! Upper right coordinates
            urx = brx
            ury = uly

            do l = 1,n
               
               if (l==j) then 
                  cycle
               end if
               
               ! 1st case: pos. coord. of sqr-j below pos. coord. of sqr-l
               if (bly<=sqrs(l)%posy) then
                  
                  ! If pos. coord. of j on left side of l:s
                  if (blx<=sqrs(l)%posx) then
                     ! and if upper right coordinates are bigger than of l
                     ! pos.coord.
                     if ((urx>sqrs(l)%posx).and.(ury>sqrs(l)%posy)) then
                        ! we have overlap 
                        m = 1
                     end if
                  end if
                  
                  ! If pos. coord. of j between the bottom corners of l
                  if ((blx>sqrs(l)%posx).and.(blx<(sqrs(l)%posx+sqrs(l)%x))) & 
                       &then
                     ! and uly is above l bottom y- coordinate
                     if (uly>sqrs(l)%posy) then
                        ! we have overlap 
                        m = 1
                     end if
                  end if

                  ! If position coordinates are the same
                  if ((bly==sqrs(l)%posy).and.(blx==sqrs(l)%posx)) then
                     ! we have overlap 
                     m = 1
                  end if
                  
               end if
               
               ! 2nd case: pos. coord of sqr-j above pos. coord. of sqr-l
               if (bly>sqrs(l)%posy) then
                  
                  ! If pos. coord. of j is on the left side of l:s
                  if (blx<=sqrs(l)%posx) then
                     ! and brx is below upper level of l and on the right side 
                     !of l:s pos. coord.
                     if((brx>sqrs(l)%posx).and.(bry<(sqrs(l)%posy+sqrs(l)%y))) &
                          &then 
                        m = 1 ! we have overlap 
                     end if
                  end if

                  ! If pos. coord. of j is on the right side of l:s
                  if (blx>sqrs(l)%posx) then
                     ! and is inside the sqr-j
                     if ((blx<(sqrs(l)%posx+sqrs(l)%x)).and.&
                          (bly<(sqrs(l)%posy+sqrs(l)%y)))& 
                          &then 
                        m = 1 ! we have overlap 
                     end if
                  end if
                  
               end if
               
            end do
            
         end if
         
         ! If swapping 2 squares
         if (i==3) then
            ! Choose squares, not caring if we choose the same square twice...
            k = igrnd(1,n)
            l = igrnd(1,n)
            
            ! Swap them, save first coordinates of the first square
            x = sqrs(k)%posx  
            y = sqrs(k)%posy
            sqrs(k)%posx = sqrs(l)%posx
            sqrs(k)%posy = sqrs(l)%posy
            sqrs(l)%posx = x
            sqrs(l)%posy = y 
            
            ! Check for overlap, same as above but for 2 squares, i={k,l}
            i = k
            do 
               ! Bottom left coordinates
               blx = sqrs(i)%posx 
               bly = sqrs(i)%posy
               ! Upper left coordinates
               ulx = sqrs(i)%posx
               uly = sqrs(i)%posy+sqrs(i)%y
               ! Bottom right coordinates
               brx = sqrs(i)%posx+sqrs(i)%x
               bry = sqrs(i)%posy
               ! Upper right coordinates
               urx = brx
               ury = uly
               
               do j = 1,n

                  if (j==i) then 
                     cycle
                  end if

                  ! 1st case: pos. coord. of sqr-i below pos. coord. of sqr-j
                  if (bly<=sqrs(j)%posy) then
                     
                     ! If pos. coord. of i on left side of j:s
                     if (blx<=sqrs(j)%posx) then
                        ! and if upper right coordinates are bigger than of j
                        ! pos.coord.
                        if ((urx>sqrs(j)%posx).and.(ury>sqrs(j)%posy)) then
                           ! we have overlap and need a new position
                           m = 1
                        end if
                     end if

                     ! If pos. coord. of i is between the bottom corners of j
                     if((blx>sqrs(j)%posx).and.(blx<(sqrs(j)%posx+sqrs(j)%x)))&
                          &then
                        ! and uly is above j bottom y- coordinate
                        if (uly>sqrs(j)%posy) then
                           ! we have overlap and need a new position
                           m = 1
                        end if
                     end if

                     ! If position coordinates are the same
                     if ((bly==sqrs(j)%posy).and.(blx==sqrs(j)%posx)) then
                        ! we have overlap and need a new position
                        m = 1
                     end if

                  end if

                  ! 2nd case: pos. coord of sqr-i above pos. coord. of sqr-j
                  if (bly>sqrs(j)%posy) then
                     
                     ! If pos. coord. of i is on the left side of j:s
                     if (blx<=sqrs(j)%posx) then
                        ! and brx is below upper level of j and on the 
                        !right side of j:s pos. coord.
                        if ((brx>sqrs(j)%posx).and.&
                             &(bry<(sqrs(j)%posy+sqrs(j)%y))) then 
                           m = 1 ! we have overlap and need a new position
                        end if
                     end if

                     ! If pos. coord. of i is on the right side of j:s
                     if (blx>sqrs(j)%posx) then
                        ! and is inside the sqr-i
                        if ((blx<(sqrs(j)%posx+sqrs(j)%x)).and.&
                             (bly<(sqrs(j)%posy+sqrs(j)%y))) then 
                           m = 1 ! we have overlap and need a new position
                        end if
                     end if
                     
                  end if
                  
               end do

               ! Checking if we have overlap and if we are done
               if (((m==0).and.(i==l)).or.(m==1)) then
                  exit
               else if ((m==0).and.(i==k)) then
                  i = l
               end if

            end do
            
         end if

         ! If we have overlap, reject the configuration and start all over, 
         ! otherwise we are done!
         if (m==1) then
            sqrs = sqrs_old
            cycle
         else if (m==0) then
            exit
         end if
     
      end do

      sqrs_new = sqrs
      
    end subroutine new_conf

!---------------------------------------------------------------------------
!---------------------------------------------------------------------------
!---------------------------------------------------------------------------
!---------- Subroutine for calculating footprint ---------------------------
!---------------------------------------------------------------------------
!---------------------------------------------------------------------------

    subroutine footprint(n,sqrs,fp)
      ! Calculate footprint
      implicit none
      
      integer, intent(in) :: n
      type(square), intent(in) :: sqrs(n)
      integer, intent(out) :: fp
      integer :: i, minx, miny, maxx, maxy 

      ! Calculating the initial by determining the coordinates of two 
      ! opposite corners, taking the lower left corner and upper right 
      ! corner of the first square as first trial bounds for the area
      minx = sqrs(1)%posx
      miny = sqrs(1)%posy
      maxx = sqrs(1)%posx+sqrs(1)%x
      maxy = sqrs(1)%posy+sqrs(1)%y
      ! Search for the lowest and highest components
      do i = 2,n
         ! Lowest x- component
         if (minx>sqrs(i)%posx) then
            minx = sqrs(i)%posx
         end if
         ! Lowest y- component
         if (miny>sqrs(i)%posy) then
            miny = sqrs(i)%posy
         end if
         ! Highest x- component
         if (maxx<(sqrs(i)%posx+sqrs(i)%x)) then
            maxx = sqrs(i)%posx+sqrs(i)%x
         end if
         ! Highest y- component
         if (maxy<(sqrs(i)%posy+sqrs(i)%y)) then
            maxy = sqrs(i)%posy+sqrs(i)%y
         end if  
 
      end do

      ! So the footprint is
      fp = (maxx-minx)*(maxy-miny)
    
    end subroutine footprint
end module squares
