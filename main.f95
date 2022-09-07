program SYCLIST
  ! Main part of the programm. Calls various modules.
  ! &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

  use DataStructure, only: verbose,Del_DataStructure
  use VariousParameters, only:Comp_Mode,init_AoV,ivdist,iangle,All_Data_Array,Z_Number,mass_Number_array, &
                              omega_Number_array,IMF_type
  use random, only:init_random,Init_Kroupa_IMF
  use ReadData, only:init_Huang,init_HG,init_Correction,init_VcritOmega,init_SurfaceOmega, &
                     init_Correct_fact,init_angle_external
  use InOut, only:Intro,AskChange,IsochroneMode
  use InterpolationLoop, only:MainLoop
  use Configuration_File, only:Config,Write_Config

  implicit none

  integer:: argtot,iargc,i,j,k
  character(256):: argv

  argtot = iargc()
  if (argtot == 0) then
    verbose = .false.
  else if (argtot == 1) then
    call getarg(1,argv)
    if (trim(argv) == '0') then
      verbose = .false.
    else if (trim(argv) == '1') then
      verbose = .true.
    else
      stop 'Wrong argument for verbose mode'
    endif
  endif

  ! Search for a configuration file
  call Config

  ! Reading data
  call init_Huang
  call init_HG
  call init_Correction
  call init_VcritOmega
  call init_SurfaceOmega
  call init_Correct_fact

  ! Introduction and choice of parameters
  call Intro
  call AskChange
  call init_AoV
  if (iangle == 4) then
    call init_angle_external
  endif
  if (IMF_type == 2) then! Initialise the Kroupa IMF
    call Init_Kroupa_IMF()
  endif

  if (Comp_Mode == 2) then
    call IsochroneMode
  endif

  ! Initialises the seed for random numbers generation
  call init_random

  ! Beginning of the main loop
  call MainLoop

  ! Save the configuration file
  call Write_Config

  ! Delete the arrays:
  do i=1,Z_Number
    do j=1,mass_Number_array(i)
      do k=1,omega_Number_array(i,j)
        call Del_DataStructure(All_Data_Array(i,j,k))
      enddo
    enddo
  enddo

  print*, char(7)

end program SYCLIST
