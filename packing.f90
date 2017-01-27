program packing
! 2D packing problem
  use squares ! Module defining the type "squares"
  ! Modules for the random number generation 
  use mtdefs
  use mtmod

  implicit none

  type(square), allocatable :: sqrs(:), sqrs_old(:), sqrs_new(:) !Squares
  integer :: n, ios, iarg
  integer :: i, j, j_max, fp, fp_old, fp_new, x, z, fp_first
  integer, allocatable :: y(:,:)
  character(len=80) :: arg
  real :: c, c_0, alpha, u, xx=1
  
  ! Read the number of squares to be generated from the command line
  iarg = command_argument_count()
  if (iarg/=1) then 
     print *, "Wrong number of input arguments!"
     stop
  end if

  call get_command_argument(1,arg)
  read(arg,*) n
  
  ! Allocating the number of squares 
  allocate(sqrs(n))
  allocate(sqrs_new(n))
  allocate(sqrs_old(n))

  ! Allocating the array for keeping track of fp vs simulation steps
  allocate(y(29920,29920)) 

  ! Initializing the random number generator
  call sgrnd(getseed(info=1))

!-------------------------------------
!-------------------------------------
!----------Starting the simulation----
!-------------------------------------
!-------------------------------------

!----------STEPS 1->2---------- 

  c = 100 ! Setting the initial value for control parameter

  ! Generating the initial configuration
  call init_conf(n,sqrs)

  ! Writing the initial configuration on a file
  open(unit=1, file='InitConfig', iostat = ios, status='unknown')
  do i = 1,n
     write(1,'("move",i4, 4x, i4)') sqrs(i)%posx, sqrs(i)%posy
     write(1,'(i4, 4x, i4)') sqrs(i)%posx, sqrs(i)%posy+sqrs(i)%y
     write(1,'(i4, 4x, i4)') sqrs(i)%posx+sqrs(i)%x, sqrs(i)%posy+sqrs(i)%y
     write(1,'(i4, 4x, i4)') sqrs(i)%posx+sqrs(i)%x, sqrs(i)%posy
     write(1,'(i4, 4x, i4)') sqrs(i)%posx, sqrs(i)%posy
  end do
  close(unit=1,status='keep')
  
  ! Calculate the initial footprint
  call footprint(n,sqrs,fp)
  fp_old = fp
  fp_first = fp

  ! Starting the array for plotting fp vs simulation steps
  z = 1
  y(1,1) = 1
  y(1,2) = fp
  
!----------Steps 3----------

  j = 1 ! Setting up the simulation step count
  
  do
!----------Steps 4->7----------
     do
        ! Generate new configuration 
        sqrs_old = sqrs
        call new_conf(n,sqrs_old,sqrs_new)
        sqrs = sqrs_new

        ! Calculate the new foorprint
        call footprint(n,sqrs,fp)
        fp_new = fp
        
        ! If new footprint is larger
        if (fp_new>fp_old) then
           u = grnd()
           x = fp_old-fp_new
           x = x*xx ! Implicit type conversion, otherwise get error...
           if (u>exp(x/c)) then
              ! Restart from step 4
              sqrs = sqrs_old
              cycle
           else if (u<exp(x/c)) then
              fp_old = fp_new
           end if
        end if
        
        ! If the new footprint is smaller
        if (fp_new<fp_old) then
           fp_old=fp_new
        end if
        
        j = j+1 ! Updating the simulation step count

        if (j<1000000) then
           cycle ! Not enough simulation steps, back to step 4
        else if (j>=1000000) then
           exit  ! Enough simulation steps, continue to step 9
        end if
           
     end do

!----------Step 8----------

     ! Update control parameter
     c = c*0.999

!----------Step 9----------

     ! If we are under the minimum value of the control parameter
     ! then stop, else go to up to step 3
     if (c<=0.00000000001) then
        exit
     else if (c>0.00000000001) then
        ! Updating array y
        z = z+1
        y(z,1) = z
        y(z,2) = fp_new
        cycle
     end if

  end do

!-------------------------------------
!-------------------------------------
!----------Simulation complete--------
!-------------------------------------
!-------------------------------------

  ! Writing the final configuration on file 
  open(unit=2, file='FinalConfig', iostat = ios, status='unknown')
  do i = 1,n
     write(2,'("move",i10, 4x, i10)') sqrs(i)%posx, sqrs(i)%posy
     write(2,'(i10, 4x, i10)') sqrs(i)%posx, sqrs(i)%posy+sqrs(i)%y
     write(2,'(i10, 4x, i10)') sqrs(i)%posx+sqrs(i)%x, sqrs(i)%posy+sqrs(i)%y
     write(2,'(i10, 4x, i10)') sqrs(i)%posx+sqrs(i)%x, sqrs(i)%posy
     write(2,'(i10, 4x, i10)') sqrs(i)%posx, sqrs(i)%posy
  end do
  close(unit=2,status='keep')

  ! Writing the array y for plotting fp vs simulation step
  open(unit=3, file='fpVSsim', iostat = ios, status='unknown')
  do i = 1,z
     write(3,'(i10, 4x, i10)'), y(i,1), y(i,2) 
  end do
  close(unit=3,status='keep')
  
  print *,
  print *, "Initial footprint was ", fp_first
  print *, "Final footprint is ", fp_new
  print *,

end program packing
     

     
        
        

     
  

                 
              
           
          
 
        
     
        
     
  
