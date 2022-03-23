! &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
module Constant
  ! Contains constants
  ! &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

  implicit none

  real(kind=8),parameter::pi = 3.1415926535897932384626433d0, & ! Pi
    L_sun = 3.846d33, &                   ! Solar luminosity
    sigma_SB = 5.670373d-5, &             ! Stefan-Boltzman constant
    G_Newton = 6.67384d-8, &              ! Gravitational constant
    M_sun = 1.9891d33, &                  ! Solar mass
    Z_sun = 0.014d0, &                    ! Solar metallicity
    R_sun = 6.961d10, &                   ! Solar radius
    sL_sun = 10.61d0                      ! Solar log spectroscopic luminosity

end module Constant
! &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

! &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
module VariousParameters
  ! Contains various parameters (among of them, some can be changed later
  ! &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

  use DataStructure, only: type_DataStructure

  implicit none

  integer,public,save:: star_number=1000                        ! Current numbe of star in the cluster.
  integer,public,save:: table_format = 1                        ! Table format (1) GENEC, (2) Starevol
  integer,public,save::i_metallicity=0                          ! Metallicity distribution: dirac (0)
  integer,public,save::ivdist=1                                 ! Velocity distribution: uniform (0),
                                                                ! huang(1), huang-gies(2), dirac_om(3)
  integer,public,save::iangle=2                                 ! uniforme dist. (1), vsini (2), Dirac (3)
                                                                ! or not (0) of the angle of view
  integer,public,save::inoise=1                                 ! noise adjunction (1) or not (0)
                                                                ! (data without noise are saved whatever
                                                                ! the case)
  integer,public,save::grav_dark=2                              ! gravity darkening law: (1) vonZeipel (1924),
                                                                !                        (2) Espinosa-Lara & Reutord (2011)
  integer,public,save::limb_dark=1                              ! no limb-darkening (0), with LD (1)
  integer,public,save::Current_Number                           ! Current star number
  integer,public,save::SN_Number                                ! SN number (star more massive than m_norm_sup)
  integer,public,save::Small_Number                             ! small mass star number (< m_norm_inf).
  integer,public,save::Cepheid_Number                           ! Cepheid number
  integer,public,save::FastRot_Number                           ! Fast rotators number
  integer,public,save::Z_Number                                 ! Metallicity number
  integer,public,save::IMF_type = 1                             ! IMF. Salpeter (1), Kroupa (2)
  integer,public,save::Comp_Mode = 0                            ! Various computation mode : (1) cluster, (2)
                                                                ! isochrone, (3) Population mode, (4) Single model
  integer,public,save::Colour_Calibration_mode = 2              ! Colour-Teff calibration (1) as in Paper I 2011,
                                                                ! (2) Worthey & Lee, ApJS 193 1 (2011).

  integer,pointer::mass_Number_array(:)                         ! Array containing the number of mass for each Z.
  integer,pointer::omega_Number_array(:,:)                      ! Array containg for each Z and mass the number
                                                                ! of different rotation velocities
  real(kind=8),public,save::Fixed_AoV_latitude=22.d0            ! Fixed angle of view from pole to equator
  real(kind=8),public,save::Fixed_AoV                           ! Same angle, in colatitude.
  real(kind=8),public,parameter::m_IMF_inf_Grids2012=0.8d0, m_IMF_sup_Grids2012=120.d0
  real(kind=8),public,parameter::m_IMF_inf_BeGrids=1.7d0, m_IMF_sup_BeGrids=15.d0
  real(kind=8),public,parameter::m_IMF_inf_NamiGrids=0.5d0, m_IMF_sup_NamiGrids=3.5d0
  real(kind=8),public,save::m_IMF_inf=m_IMF_inf_BeGrids, m_IMF_sup=m_IMF_sup_BeGrids  ! IMF min and max mass
  real(kind=8),public,save::fixed_metallicity = 0.014d0         ! Metallicity used in case of Dirac distribution
  real(kind=8),public,save::om_ivdist=0.40d0                    ! In case of Dirac omega-distribution
  real(kind=8),public,save::binary_prob=0.30d0                  ! Binarity probability
  real(kind=8),public,save::Star_Z                              ! Current stellar metallicity
  real(kind=8),public,save::Star_mass                           ! Current stellar mass
  real(kind=8),public,save::Star_omega                          ! Current stellar velocity
  real(kind=8),public,save::Star_AoV                            ! Current stellar angle of view
  real(kind=8),public,parameter::log_age_max=11.0d0             ! Maximal log(age) for cluster allowed with the
  real(kind=8),public,save::sigma_mv=0.150d0, sigma_bv=0.10d0   ! standard deviation in MV et B-V
                                                                ! current tables.
  real(kind=8),public,save::age_log                             ! wanted log(age).
  real(kind=8),public,save::Cluster_mass                        ! Total cluster mass
  real(kind=8),dimension(:,:,:),public,pointer,save::omega_List=>null() ! List of the rotation velocities.
  real(kind=8),dimension(:,:),public,pointer,save::mass_List=>null() ! List of the masses.
  real(kind=8),dimension(:),public,pointer,save::Z_List=>null() ! List of the metallicities.

  logical,public,save::PMS=.false.
  logical,public,save::Compute                                  ! Used in the main loop

  type(type_DataStructure),dimension(:,:,:),public,pointer,save::All_Data_Array=>null() ! Array containing
                                                                ! in each coordinate (ie, Z,m and omega)
                                                                ! the data structure.
  character(9),public:: grid = "BeGrids"
  character(256),public::Std_Path,Std_Path_tables
                                                                ! directory of following files:
  character(*),parameter,public::HuangFile = 'HuangDist.dat', & ! File of the Huang distribution
    HGFile = 'HGDist.dat', &       ! File of the Huang & Gies distribution
    LumFileVZ = 'CorrLumVZ.dat', &     ! File of the luminosity corrections according to von Zeipel 1924
    TeffFileVZ = 'CorrTeffVZ.dat', &   ! File of the Teff corrections according to von Zeipel 1924
    LumFileELR = 'CorrLumELR.dat', &     ! File of the luminosity corrections according to ELR 2011
    TeffFileELR = 'CorrTeffELR.dat', &   ! File of the Teff corrections according to ELR 2011
    GravFileVZ = 'CorrGravVZ.dat', &   ! File of the Teff corrections according to von Zeipel 1924
    GravFileELR = 'CorrGravELR.dat', &   ! File of the Teff corrections according to ELR 2011
    LumGridVZ = 'CorrLumVZ_LD.dat', &   !new! Grid of files for lum corrections (vZ with LD)
    TeffGridVZ = 'CorrTeffVZ_LD.dat', & !new! Grid of files for Teff corrections (vZ with LD)
    LumGridELR = 'CorrLumELR_LD.dat', &   !new! Grid of files for lum corrections (ELR with LD)
    TeffGridELR = 'CorrTeffELR_LD.dat', & !new! Grid of files for Teff corrections (ELR with LD)
    VCrit_to_Omega_File = 'Vcrit2omega.dat', &  ! Vcrit to omega translation
    OmegaSurface_File = 'Surface_Omega.dat', & ! S vs omega/omega_crit data file.
    DataColor = 'DataColours.dat'   ! File of the data for color corrections.
  character(*),parameter,public::ListFileGrids2012='ListFileL.txt', &  ! File containing the path of the large grid files
                                 ListFileBeGrids='ListFileB.txt', &    ! File containing the path of the Be grid files
                                 ListFileFastBe='ListFileF.txt',&      ! File containing the path of the fast Be grid files
                                 ListFileNamiGrids='ListFileN.txt'     ! File containing the path of the Nami grid files

  character(*),parameter,public::GridsFormat = '(i3,1x,e22.15,1x,f11.6,2(1x,f9.6),2(1x,e14.7),1p,&
     &8(1x,e14.7),1x,e10.3,1x,0pf7.4,1x,f9.6,1x,f8.3,2(1x,f9.6),2(1x,e14.7),1p,8(1x,e14.7),&
     &5(1x,e10.3),3(1x,e9.2),0p,2(1x,f9.6),1x,1pe14.7,1x,e17.10)'

  public:: init_AoV

contains

  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  subroutine init_AoV()
    ! Transforms the given angle of view in colatitudinal angle.
    ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    use Constant, only:pi

    implicit none

    Fixed_AoV = pi/2.d0-Fixed_AoV_latitude*(pi/180.d0)

    return

  end subroutine init_AoV
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

end module VariousParameters
! &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

! &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
module LoopVariables
  ! Module containing variables used only in the main loop, not useful elsewhere
  ! &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

  use DataStructure, only: type_DataStructure,type_TimeModel

  implicit none

  integer, dimension(2,2), public, save::omega_Position         ! Position of the current velocity in the
                                                                ! table.
  integer, dimension(2), public, save::mass_Position            ! Position of the current mass in the
                                                                ! table.
  integer, public, save::Z_Position                             ! Position of the current metallicity in the
                                                                ! table.

  real(kind=8), dimension(2,2), public, save::omega_factor      ! Interpolation factor in omega.
  real(kind=8), dimension(2), public, save::mass_factor         ! Interpolation factor in mass.
  real(kind=8), public, save::Z_factor                          ! Interpolation factor in metllicity.

  type(type_DataStructure), public, save::Interpolated_Model
  type(type_TimeModel), dimension(:),public, pointer, save:: CurrentTime_Model=>null() ! Array containing
                                                              ! the data of the model at the given time.

end module LoopVariables
! &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&