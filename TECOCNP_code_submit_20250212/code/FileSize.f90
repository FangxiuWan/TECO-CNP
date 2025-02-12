MODULE IntersVariables
IMPLICIT NONE
SAVE
TYPE :: ResultType2
INTEGER :: nleap_rere
INTEGER :: force_nhours_rere
INTEGER :: output_ndays_rere
INTEGER :: nyear_rere
END TYPE ResultType2

INTEGER,PARAMETER :: SGL = SELECTED_REAL_KIND(p=6)  !SINGLE
INTEGER,PARAMETER :: DBL = SELECTED_REAL_KIND(p=13) !DOUBLE
INTEGER,PARAMETER :: intn = SELECTED_INT_KIND(9) 
INTEGER,PARAMETER :: pools = 9
REAL,PARAMETER :: fbmC = 0.55
INTEGER start_year,end_year

REAL (SGL) :: Fptase_slow,Fptase_pass,N_loss_mic
REAL (SGL):: GPP,NEE,Reco,NPP,xnpNPP,NPP_noadd,NPP_FORNSC,NEE_new
REAL (SGL):: Acan1,Acan2,fslt,fshd
REAL (SGL):: Rgrowth,Rnitrogen,Rphosphorus,Rmain,Rauto,Rhetero
REAL (SGL):: GrowthP,xnpGrowthP,bmleaf,bmstem,bmroot,bmRe,bmplant,xnpGrowthP_temp
REAL (SGL):: NSC,fnsc,storage,add,accumulation,store
REAL (SGL):: LAI,ht,L_tau_new
! allocation ratio to Leaf, stem, and Root
REAL (SGL):: alpha_L,alpha_W,alpha_R,alpha_Re,a_L,a_S,a_R,a_Re,NPP_L,NPP_W,NPP_R,NPP_Re
REAL (SGL):: OutC(pools),OutN(pools),OutP(pools)
REAL (SGL):: QC(pools),QN(pools),QP(pools),CN(pools),CP(pools),CN0(pools),CP0(pools)
REAL (SGL):: N_miner,N_immob,N_leach,N_vol,N_net,N_uptake,N_fixation,N_transfer,N_deficit
REAL (SGL):: P_transfer,P_deficit,P_deficit_test,P_demand_test
REAL (SGL):: xde,xnp_leaf_limit,xnp_stem_limit,xnp_root_limit,xNPuptake,LfactorN,LfactorP
! This for new xde
REAL (SGL):: xde2,OutC2(pools),OutN2(pools),OutP2(pools),N_miner2,N_immob2,N_net2
REAL (SGL):: P_miner2,P_immob2,P_net2
REAL (SGL):: P_miner22,P_immob22,P_net22,N_miner22,N_immob22,N_net22,xde22
!-
REAL (SGL):: P_leaf,P_wood,P_root,P_re,N_leaf,N_wood,N_root,N_re,NSP,NSN
REAL (SGL):: N_demand,P_demand
REAL (SGL):: Dsoillab,P_net,Fptase,P_loss,P_uptake,P_miner,P_immob,asorb,bss
REAL (SGL):: QPlab,QPsorb,QPss,QNminer,QPocc
REAL (SGL):: S_omega,W,Sw,wsc(10),wcl(10),omega,zwt,S_t(5),st !wcl, volum ratio
REAL (SGL):: gamma_T,gamma_W
REAL (SGL):: GDD5
REAL (SGL):: tauC(pools),exitK(pools)
REAL (SGL):: x_stem_limit,x_root_limit,x_leaf_limit,xuptake
REAL (SGL) :: etaL,etaW,etaR,etaRe,f_F2M,f_C2M,f_C2S,f_M2S,f_M2P,f_S2M,f_S2P,f_P2M
REAL (SGL) :: etaL2,etaW2,etaR2,etaRe2
REAL (SGL) :: s_w_min,wsmin,wsmax
REAL tau_L,tau_W,tau_R,tau_Re
REAL tau_F,tau_C,tau_Micr,tau_Slow,tau_Pass
! canopy
REAL Acanop,Hcanop
REAL evap,transp        !wan- recalculater soil evaporation accord. to Weng & Luo 2008, eq.A9.
REAL (SGL):: Aleaf(2),Tlk1,Tlk2
REAL Rsoilabs!Total radiation absorbed by soil 
INTEGER (SGL) itime,idays,Flag_N,Flag_P,Flag_C,Flag_Growth,Flag_GPP,Flag_QP,Flag_NSC,rejectnow
REAL Tairmax
INTEGER, Dimension(1) :: Tairmax_Loc
INTEGER Tairmax_Loc_value
INTEGER onset !flag of phenological stage
REAL (SGL) :: aN,aP,bN,bP
INTEGER, DIMENSION(:), allocatable :: year_all 
INTEGER, DIMENSION(:), allocatable :: uni_Year
INTEGER num_cols
INTEGER force_nhours,output_ndays,nobs,ndays,nyear,lastyear_days,lastyear_hours !joutput = the number of output varible
INTEGER, DIMENSION(:), allocatable :: leap_year
TYPE (ResultType2) :: rere
REAL (SGL) :: P_reserve, N_reserve,NSPmax,NSPmin
REAL (SGL) :: rootSurfaceP,rate_maxN,rate_maxP
INTEGER, PARAMETER :: npara_all = 61
REAL (SGL) :: site_paras(npara_all),InitialCNP(pools,3) !change for paras
INTEGER iyear,resoforce_hr !INTEGER resoforce_hr =temporal resolution
INTEGER n_leap_year,nspinup,ispinup
INTEGER CYCLE_CNP,NutrientsUp,MCMC,NDSPINUP,SensTest,EQstomata,NPaddition
CHARACTER(len=99) fileplace,filesignal
REAL infilt,fwsoil,topfws
REAL f_rootp
REAl Traceresults(10,10)
REAL Rh_pools(5)            !fine lit.,coarse lit.,Micr,Slow,Pass
REAL ExcessC, EC_store, EC_out, exitEC, Rauto_new
REAL XX_rec,XX_med_rec,gscx_rec,gscx_med_rec,XX_med,g1_med,Gscx_med
REAL,DIMENSION(3)::newsoilNCmax,newsoilNCmin,newsoilNC
!REAL, DIMENSION(8760) :: 

REAL, DIMENSION(:), ALLOCATABLE :: MaxUpCapP_rec,bP_rec,aP_rec,fwsoil_rec,omega_rec
REAL, DIMENSION(:), ALLOCATABLE :: P_loss_rec,Fptase_rec,asorb_rec,Dsoillab_rec
REAL, DIMENSION(:), ALLOCATABLE :: P_miner2_rec,P_immob2_rec,P_net2_rec
REAL, DIMENSION(:), ALLOCATABLE :: Dsoillab_rec2, P_uptake_rec,P_in_out_rec,P_leach_rec
REAL, DIMENSION(:), ALLOCATABLE :: FdiffP_rec,delta_QLabP_root_rec,D_rec
REAL, DIMENSION(:), ALLOCATABLE :: swc_p_rec,tf_rec,rdiff_rec,tf1_rec,tf2_rec
REAL, DIMENSION(:), ALLOCATABLE :: f_rootp_rec,QC3_rec,LfactorP_rec
REAL, DIMENSION(:), ALLOCATABLE :: P_deficit_rec,P_demand_rec,P_deficit_rec_test,P_demand_rec_test
REAL, DIMENSION(:), ALLOCATABLE :: N_demand_rec,N_uptake_rec,N_deficit_rec
REAL, DIMENSION(:), ALLOCATABLE :: N_fixation_x_rec
REAL, DIMENSION(:,:), ALLOCATABLE :: Xss_new_step_rec


END MODULE IntersVariables

!-----------------------------

MODULE DaysHours
    !Module to decalre Translate fraction data to share between routines
    USE IntersVariables
    IMPLICIT NONE

    TYPE :: ResultType
    INTEGER :: Days
    INTEGER :: Hours
    END TYPE ResultType

    CONTAINS

    FUNCTION check_leap(year)
        IMPLICIT NONE
        INTEGER year,check_leap,n

        if (mod(year,4) == 0 .and. mod(year,100)/=0) then ! 能被4整除且不能被100整除为闰年
                write(*,*) " leap year",year 
                check_leap = 1
        elseif (mod(year,400) == 0) then ! 能被400整除也为闰年
                write(*,*) " leap year",year 
                check_leap = 1
        else
                check_leap =0
        end if
    END FUNCTION check_leap


    FUNCTION YearDayHour() RESULT(rere1)
        IMPLICIT NONE
        INTEGER nLeapYear,n,m,n_Year,n_i_Year,i_Year,nleap1,year,check_leap1
        INTEGER force_nhours1,output_ndays1,nyear1
        TYPE(ResultType2) :: rere1

        output_ndays1 = 0
        force_nhours1 = 0

        nleap1 = 0
        !start_year = 2001
        !end_year   = 2001
        nyear1 = end_year - start_year + 1
        allocate(uni_Year(nyear1))

        uni_Year = 0

        uni_Year(1) = start_year
        n_i_Year = 2 ! start from 2

        DO n = 1,nyear1
            i_year = start_year + n
            uni_year(n_i_Year) = i_Year
            n_i_Year = n_i_Year+1
            
        ENDDO
        write(*,*)'uni_Year',uni_Year,nyear1
        !stop
        
        DO n = 1,nyear1
            year = uni_Year(n)
            check_leap1 = check_leap(year)
            if (check_leap1 .EQ. 1) then
                    nleap1 = nleap1+1
                    ndays = 366
            else
                    !write(*,*) " no leap year",year 
                    ndays = 365
            end if
            force_nhours1 = force_nhours1+ndays*24.
            output_ndays1 = output_ndays1 +ndays
            IF (n .eq. nyear1)THEN
                lastyear_days = ndays
                lastyear_hours = ndays*24
                ! The loop index will take the next value after the end of loop range
            ENDIF
        ENDDO
        write(*,*)'nleap1',nleap1

        rere1%nleap_rere = nleap1
        rere1%force_nhours_rere = force_nhours1
        rere1%output_ndays_rere = output_ndays1
        rere1%nyear_rere        = nyear1

    END FUNCTION YearDayHour

    FUNCTION Days_cal(simu_year) RESULT(result)
        IMPLICIT NONE
        INTEGER simu_year,m,Days1,Hours1,check_leap1
        TYPE (ResultType) :: result

        Days1 = 365
        
        check_leap1 = check_leap(simu_year)
        IF (check_leap1 .EQ. 1) THEN
            Days1 = 366
        ENDIF

        Hours1 = Days1*24
        result%Days = Days1
        result%Hours = Hours1

    END FUNCTION Days_cal

END MODULE DaysHours

MODULE FileSize
    !------------------------------------------------------------------
    ! Module for managing global variables and subroutines:
    ! 1. `Allosize` - Allocate memory
    ! 2. `Deallo_mod` - Deallocate memory
    ! 3. `FilesForOutput` - Configure output files
    ! (Fangxiu, 20230626)
    !-------------------------------------------------------------------
    USE IntersVariables
    USE DaysHours
    IMPLICIT NONE
    SAVE
    
    REAL (SGL), dimension(:,:), allocatable ::  forcing_data !change for the forcing iiterms
    !REAL, dimension(:),   allocatable ::  output_data  !hourly output record, same raws of force data
    REAL (SGL), dimension(:,:), allocatable ::  output_daily
    REAL (SGL), dimension(:,:), allocatable ::  output_yr
    REAL (SGL), dimension(:,:), allocatable ::  outputd_ccycle_Cpools
    REAL (SGL), dimension(:,:), allocatable ::  outputd_outc
    REAL (SGL), dimension(:,:), allocatable ::  outputd_outn
    REAL (SGL), dimension(:,:), allocatable ::  outputd_outp
    REAL (SGL), dimension(:,:), allocatable ::  outputd_Pcycle_Ppools
    REAL (SGL), dimension(:,:), allocatable ::  outputd_Pcycle_CPratios
    REAL (SGL), dimension(:,:), allocatable ::  outputd_Ncycle_Npools
    REAL (SGL), dimension(:,:), allocatable ::  outputd_Ncycle_CNratios
    REAL (SGL), dimension(:,:), allocatable ::  outputd_Pdynamic
    REAL (SGL), dimension(:,:), allocatable ::  outputd_Ndynamic
    REAL (SGL), dimension(:,:), allocatable ::  output_P_record 
    REAL (SGL), dimension(:,:), allocatable ::  output_N_record
    REAL (SGL), dimension(:,:), allocatable ::  output_OutC_record
    REAL (SGL), dimension(:,:), allocatable ::  output_OutN_record
    REAL (SGL), dimension(:,:), allocatable ::  output_OutP_record
    REAL (SGL), dimension(:,:), allocatable ::  output_x_record
    REAL (SGL), dimension(:,:), allocatable ::  output_x
    REAL (SGL), dimension(:,:), allocatable ::  Limite_NP
    !REAL (SGL), dimension(:,:), allocatable ::  Limite_NP_loops

    !REAL, dimension(:,:), allocatable ::  output_LabilePDynamic

    !for GPP modify
    REAL (SGL), dimension(:,:), allocatable ::  output_Aleaf_record   !hourly
    REAL (SGL), dimension(:,:), allocatable ::  outputd_Aleaf

    !for SpinUp/SAS xia et al., 2012
    REAL (SGL), dimension(:), allocatable :: LoopNPP_d,LoopNPP_yr,LoopNPP_yr_noadd
    REAL (SGL), dimension(:), allocatable :: LoopNEE_d,LoopNEE_yr
    REAL (SGL) :: DeltNPP,LastLoopNPP,EndLoopNPP
    REAL (SGL), dimension(:,:), allocatable :: LoopQC
    !REAL (SGL), dimension(:,:), allocatable :: LoopQC_yrmean
    REAL (SGL), dimension(:,:), allocatable :: Dynamic_trace_Xct
    REAL (SGL), dimension(:,:), allocatable :: Dynamic_trace_Xpt
    REAL (SGL), dimension(:,:), allocatable :: Dynamic_trace_EcoTau
    REAL (SGL), dimension(:,:), allocatable :: Dynamic_trace_BaseTau
    REAL (SGL), dimension(:,:), allocatable :: Dynamic_trace_deltaQC
    REAL (SGL), dimension(:,:), allocatable :: Rh_pools_record

    REAL (SGL), dimension(:,:), allocatable :: Dynamic_trace_Xct_day
    REAL (SGL), dimension(:,:), allocatable :: Dynamic_trace_Xpt_day
    REAL (SGL), dimension(:,:), allocatable :: Dynamic_trace_EcoTau_day
    REAL (SGL), dimension(:,:), allocatable :: Dynamic_trace_BaseTau_day
    REAL (SGL), dimension(:,:), allocatable :: Dynamic_trace_deltaQC_day


    REAL (SGL), dimension(:), allocatable :: LoopQC9
    REAL (SGL), dimension(:,:), allocatable :: Loop_variables

    REAL (SGL), dimension(:,:), allocatable :: LoopQN
    REAL (SGL), dimension(:,:), allocatable :: LoopQP
    REAL (SGL), dimension(:,:), allocatable :: Loop_variablesNNN
    REAL (SGL), dimension(:,:), allocatable :: Loop_variablesPPP
    
    !    !environmental scalar
    REAL (SGL), dimension(:,:), allocatable :: Somega_output
    REAL (SGL), dimension(:,:), allocatable :: ST_NP_output
    REAL (SGL),dimension(:,:), allocatable :: ksi_record
    REAL (SGL),DIMENSION (:,:),allocatable :: ksi_output_daily
    REAL (SGL),DIMENSION (:,:),allocatable :: ksi_np_output_daily
    REAL (SGL), dimension(:,:), allocatable ::NPPallo
    REAL (SGL), dimension(:,:), allocatable ::output_nppallo_record

    REAL (SGL), dimension(:,:), allocatable ::  QC_record

    
    ! for MCMC
    REAL (SGL), dimension(:,:), allocatable ::  output_daily_mcmc
    REAL (SGL), dimension(:,:), allocatable ::  obs_TianTong
    !for hourly record
    REAL (SGL), dimension(:,:), allocatable ::  output_mcmc         !hourly for mcmc
    REAL (SGL), dimension(:,:), allocatable ::  output_record        !hourly for no mcmc
    
    INTEGER, PARAMETER :: joutput = 12 !the colnums of output varsiables, daily
    INTEGER, PARAMETER :: jrecord = 18 !the colnums of hourly simitations
    INTEGER, PARAMETER :: jforce=11    !force_nhours=8760, iforce = force_nhours
    INTEGER, PARAMETER :: jobs=6       !the colnums of the observation for MCMC
    INTEGER, PARAMETER :: jmcmc=3      !the colnums of the constraind results after MCMC
 
    CONTAINS
    
    SUBROUTINE AlloSize()

    ! Subroutine for allocate size for variables

    IMPLICIT NONE
    INTEGER m

    !Consdering the leap year
        ispinup = 0
        rere = YearDayHour()
        force_nhours = rere%force_nhours_rere
        output_ndays = rere%output_ndays_rere
        nyear        = rere%nyear_rere

        ! ALLO _REC 
        allocate(MaxUpCapP_rec(force_nhours))
        allocate(bP_rec(force_nhours))
        allocate(aP_rec(force_nhours))
        allocate(fwsoil_rec(force_nhours))
        allocate(omega_rec(force_nhours))
        allocate(P_loss_rec(force_nhours))
        allocate(Fptase_rec(force_nhours))
        allocate(asorb_rec(force_nhours))
        allocate(P_miner2_rec(force_nhours))
        allocate(P_immob2_rec(force_nhours))
        allocate(P_net2_rec(force_nhours))
        allocate(Dsoillab_rec(force_nhours))
        allocate(Dsoillab_rec2(force_nhours))
        allocate(P_uptake_rec(force_nhours))
        allocate(P_in_out_rec(force_nhours))
        allocate(FdiffP_rec(force_nhours))
        allocate(delta_QLabP_root_rec(force_nhours))
        allocate(D_rec(force_nhours))
        allocate(swc_p_rec(force_nhours))
        allocate(tf_rec(force_nhours))
        allocate(rdiff_rec(force_nhours))
        allocate(tf1_rec(force_nhours))
        allocate(tf2_rec(force_nhours))
        allocate(f_rootp_rec(force_nhours))
        allocate(QC3_rec(force_nhours))
        allocate(LfactorP_rec(force_nhours))
        allocate(P_deficit_rec(force_nhours))
        allocate(P_deficit_rec_test(force_nhours))
        allocate(P_demand_rec(force_nhours))
        allocate(P_demand_rec_test(force_nhours))
        allocate(N_demand_rec(force_nhours))
        allocate(N_uptake_rec(force_nhours))
        allocate(N_deficit_rec(force_nhours))
        allocate(N_fixation_x_rec(force_nhours))
        allocate(P_leach_rec(force_nhours))
        allocate(Xss_new_step_rec(force_nhours,9))

        !write(*,*)'SHAPE',shape(asorb_rec)
        !stop

        allocate(forcing_data(11,force_nhours))
        write(*,*)'forcing_data size:',shape(forcing_data),nyear

        nobs = output_ndays  !nobs = the raws of observation

        !output
        !allocate(output_data(force_nhours))
        allocate(output_daily(28,output_ndays))  !note the dimension1 of output_daily isnt the joutput!
        allocate(output_yr(14,nyear))
        allocate(outputd_outc(pools,output_ndays))
        allocate(outputd_outn(pools,output_ndays))
        allocate(outputd_outp(pools,output_ndays))
        allocate(outputd_ccycle_Cpools(pools,output_ndays)) !pools -- 8 carbon pools
        allocate(outputd_Pcycle_Ppools(pools,output_ndays))
        allocate(outputd_Ncycle_Npools(pools,output_ndays))
        allocate(outputd_Pcycle_CPratios(pools,output_ndays))
        allocate(outputd_Ncycle_CNratios(pools,output_ndays))
        allocate(outputd_Pdynamic(15,output_ndays))
        allocate(outputd_Ndynamic(9,output_ndays))
        allocate(output_x(pools,output_ndays))

      
        allocate(QC_record(pools,force_nhours))
        allocate(Dynamic_trace_Xct(pools,force_nhours))
        allocate(Dynamic_trace_Xpt(pools,force_nhours))
        allocate(Dynamic_trace_EcoTau(pools,force_nhours))
        allocate(Dynamic_trace_BaseTau(pools,force_nhours))
        allocate(Dynamic_trace_deltaQC(pools,force_nhours))
        allocate(Rh_pools_record(5,force_nhours))

        allocate(Dynamic_trace_Xct_day(pools,output_ndays))
        allocate(Dynamic_trace_Xpt_day(pools,output_ndays))
        allocate(Dynamic_trace_EcoTau_day(pools,output_ndays))
        allocate(Dynamic_trace_BaseTau_day(pools,output_ndays))
        allocate(Dynamic_trace_deltaQC_day(pools,output_ndays))

        !for spinup/SAS
        IF (NDSPINUP .EQ. 1)  THEN
            allocate(LoopNPP_d(nspinup))
            allocate(LoopNPP_yr(nspinup))
            allocate(LoopNPP_yr_noadd(nspinup))
            allocate(LoopNEE_d(nspinup))
            allocate(LoopNEE_yr(nspinup))
            allocate(LoopQC(pools,nspinup))
            !allocate(LoopQC_yrmean(pools,nspinup))
            allocate(LoopQC9(nspinup))
            allocate(Loop_variables(14,nspinup))
            allocate(LoopQN(14,nspinup))
            allocate(Loop_variablesNNN(14,nspinup))
            allocate(LoopQP(22,nspinup))
            allocate(Loop_variablesPPP(22,nspinup))
            !allocate(Limite_NP_loops(7,nspinup))
        ENDIF
        
        allocate(Somega_output(24,output_ndays))
        allocate(ST_NP_output(15,output_ndays))  !wan 0519 change to force_nhours
        allocate(NPPallo(13,output_ndays))
        allocate(ksi_output_daily(6,output_ndays))
        allocate(ksi_np_output_daily(1,output_ndays))
        
        

        !for GPP modify
        allocate(output_Aleaf_record(12,force_nhours))
        allocate(outputd_Aleaf(5,output_ndays))
    
        !for mcmc
        allocate(output_daily_mcmc(jmcmc,nobs))
        !allocate(obs_TianTong(jobs,nobs))
        allocate(obs_TianTong(4,8760))

        !for hourly record
        allocate(ksi_record(6,force_nhours))

        allocate(output_record(30,force_nhours))
        allocate(output_mcmc(jmcmc,force_nhours))
        allocate(output_P_record(12,force_nhours))
        allocate(output_N_record(9,force_nhours))
        allocate(output_nppallo_record(13,force_nhours))
        allocate(output_OutC_record(pools,force_nhours))
        allocate(output_OutN_record(pools,force_nhours))
        allocate(output_OutP_record(pools,force_nhours))
        allocate(output_x_record(pools,force_nhours))
        allocate(Limite_NP(9,force_nhours))
        

        write(*,*)'allsize called sucessfully'
        print *, shape(outputd_Ndynamic),shape(forcing_data),shape(output_daily)
        write(*,*) 'force_nhours,output_ndays',force_nhours,output_ndays

    END SUBROUTINE AlloSize


    SUBROUTINE Deallo_mod
    !-----------------------------------
    !Subroutine for deallocate the space
    !-----------------------------------
    IMPLICIT NONE
        deallocate(forcing_data)
        !deallocate(output_data)
        deallocate(output_daily)  !note the dimension1 of output_daily isnt the joutput!
        deallocate(outputd_outc)
        deallocate(outputd_ccycle_Cpools) !8 -- 8 carbon pools
        deallocate(outputd_Pcycle_Ppools)
        deallocate(outputd_Ncycle_Npools)
        deallocate(outputd_Pcycle_CPratios)
        deallocate(outputd_Ncycle_CNratios)
        deallocate(outputd_Pdynamic)
        deallocate(outputd_Ndynamic)
        deallocate(output_x)
        
        !for spinup/SAS
        deallocate(LoopNPP_d)
        deallocate(LoopNPP_yr)
        deallocate(LoopNPP_yr_noadd)
        deallocate(LoopNEE_d)
        deallocate(LoopNEE_yr)
        deallocate(LoopQC)
        deallocate(LoopQC9)
        deallocate(Loop_variables)


        deallocate(LoopQN)
        deallocate(Loop_variablesNNN)
        deallocate(LoopQP)
        deallocate(Loop_variablesPPP)
        

        deallocate(Somega_output)
        deallocate(ST_NP_output)  !wan 0519 change to force_nhours
        deallocate(NPPallo) 

        !for GPP modify
        deallocate(output_Aleaf_record)
        deallocate(outputd_Aleaf)
    
        !for mcmc
        deallocate(output_daily_mcmc)
        deallocate(obs_TianTong)

        !for hourly record
        deallocate(output_record)
        deallocate(output_mcmc)
        deallocate(output_P_record)
        deallocate(output_N_record)
        deallocate(output_nppallo_record)
        deallocate(output_OutC_record)
        deallocate(output_x_record)
        deallocate(Limite_NP)

        write(*,*)'deallsize called sucessfully'

    END SUBROUTINE Deallo_mod

END MODULE FileSize

MODULE outputs_mod
    USE FileSize
    CONTAINS
    SUBROUTINE ForRecords(AcanL)
        IMPLICIT NONE
        INTEGER m
        REAL Ta, AcanL(5)
        INTEGER,PARAMETER:: cc = 1
    
        !  Results output (hourly record)
        !WRITE(*,*)'RECORDING, ispinup =',ispinup
    
        IF(MCMC.eq.1)THEN
                output_mcmc(1,itime)=GPP
                output_mcmc(2,itime)=Reco
                output_mcmc(3,itime)=NEE
                IF(mod(itime,24).eq.0)THEN
                    !WRITE(*,*)'WRITE to output_daily...'
                    idays=idays+cc
                    output_daily_mcmc(1,idays)=sum(output_mcmc(1,(itime-23):itime)) !daily total GPP
                    output_daily_mcmc(2,idays)=sum(output_mcmc(2,(itime-23):itime)) !daily total Reco
                    output_daily_mcmc(3,idays)=sum(output_mcmc(3,(itime-23):itime)) !daily total NEE
                ENDIF
        ELSE  
                output_record(1,itime) = GPP
                output_record(2,itime) = Reco
                output_record(3,itime) = NEE
                output_record(4,itime) = LAI
                output_record(5,itime) = bmplant
                output_record(6,itime) = NPP
                output_record(7,itime) = xnpNPP
                output_record(8,itime) = Rhetero
                output_record(9,itime) = Rauto
                output_record(10,itime) = Rmain
                output_record(11,itime) = Rgrowth
                output_record(12,itime) = Rnitrogen
                output_record(13,itime) = Rphosphorus
                output_record(14,itime) = GrowthP
                output_record(15,itime) = xnpGrowthP_temp
                output_record(16,itime) = fnsc
                output_record(17,itime) = storage
                output_record(18,itime) = accumulation
                output_record(19,itime) = NPP_noadd
                output_record(20,itime) = bmleaf
                output_record(21,itime) = bmstem
                output_record(22,itime) = bmroot
                output_record(23,itime) = bmRe
                output_record(24,itime) = NSC
                output_record(25,itime) = ExcessC
                output_record(26,itime) = EC_store
                output_record(27,itime) = Rauto_new
                output_record(28,itime) = add
                output_record(29,itime) = store
                output_record(30,itime) = NEE_new

                Rh_pools_record(1:5,itime) = Rh_pools(1:5)
    
                ksi_record (1,itime)  = S_t(1)
                ksi_record (2, itime) = S_omega
                ksi_record (3, itime) = gamma_T
                ksi_record (4, itime) = gamma_W
                ksi_record (5, itime) = L_tau_new
                ksi_record (6, itime) = S_t(1)*S_omega
    
                
                !write(*,*)'ksi_record',itime,ksi_record(:,itime)
            
                output_OutC_record(:,itime)=OutC
                output_OutN_record(:,itime)=OutN
                output_OutP_record(:,itime)=OutP
    
                !WRITE(*,*)'OutC8',output_OutC_record(8,itime)
    
                !IF(itime .EQ. 8760)THEN
                !    WRITE(*,*)'Output one year of passive pool',sum(output_OutC_record(8,:))
                !ENDIF
                output_nppallo_record(1,itime)=alpha_L
                output_nppallo_record(2,itime)=alpha_W
                output_nppallo_record(3,itime)=alpha_R
                output_nppallo_record(4,itime)=alpha_Re
                output_nppallo_record(5,itime)=a_L
                output_nppallo_record(6,itime)=a_S
                output_nppallo_record(7,itime)=a_R
                output_nppallo_record(8,itime)=a_Re
                output_nppallo_record(9,itime)=add
                output_nppallo_record(10,itime)=NPP_L
                output_nppallo_record(11,itime)=NPP_W
                output_nppallo_record(12,itime)=NPP_R
                output_nppallo_record(13,itime)=NPP_Re
            
                output_Aleaf_record(1,itime)= Aleaf(1)
                output_Aleaf_record(2,itime)= Aleaf(2)
                !output_Aleaf_record(3,itime) =Tlk1
                !output_Aleaf_record(4,itime) =Tlk2
                !output_Aleaf_record(5,itime) =fslt
                !output_Aleaf_record(6,itime) =Acan1
                !output_Aleaf_record(7,itime) =Acan2
                !output_Aleaf_record(8:12,itime) =AcanL
                !output_Aleaf_record(13,itime) = XX_rec
                !output_Aleaf_record(14,itime) = XX_med_rec
                !output_Aleaf_record(15,itime) = gscx_rec
                !output_Aleaf_record(16,itime) = gscx_med_rec
                IF(CYCLE_CNP .gt. 1.)THEN
    
                    output_N_record(1,itime)=N_miner2
                    output_N_record(2,itime)=N_immob2
                    output_N_record(3,itime)=N_leach
                    output_N_record(4,itime)=N_vol       !gas loss, always be 1.1E-39 something wrong
                    output_N_record(5,itime)=N_net2 
                    output_N_record(6,itime)=N_uptake    ! many zero
                    output_N_record(7,itime)=N_fixation
                    output_N_record(8,itime)=N_transfer
                    output_N_record(9,itime)=N_demand
    
                    Limite_NP(1,itime)   = xde       ! N,P limitation on decomposition
                    Limite_NP(2,itime)   = xnp_leaf_limit
                    Limite_NP(3,itime)   = xnp_stem_limit
                    Limite_NP(4,itime)  = xnp_root_limit
                    Limite_NP(5,itime) =  xNPuptake
                    Limite_NP(6,itime) =  LfactorN
                    Limite_NP(7,itime) =  LfactorP
                    Limite_NP(8,itime) = output_N_record(9,itime)  !N_demand
                    ksi_record(6:,itime) =S_t(1)*S_omega*xde
                ENDIF
                IF(CYCLE_CNP .gt. 2.)THEN
    
                    output_x_record(1,itime)=P_leaf
                    output_x_record(2,itime)=P_wood
                    output_x_record(3,itime)=P_root
                    output_x_record(4,itime)=N_leaf
                    output_x_record(5,itime)=N_wood
                    output_x_record(6,itime)=N_root
                    output_x_record(7,itime)=NSN
                    output_x_record(8,itime)=NSP
    
                    output_P_record(1,itime)= Dsoillab
                    output_P_record(2,itime)= P_net2
                    output_P_record(3,itime)= Fptase
                    output_P_record(4,itime)= P_loss  !=P_leach
                    output_P_record(5,itime)= P_uptake
                    output_P_record(6,itime)= asorb
                    output_P_record(7,itime)= bss
                    output_P_record(8,itime)=P_miner2  !min.
                    output_P_record(9,itime)=P_immob2   !imm.
                    output_P_record(10,itime)=P_demand
                    output_P_record(11,itime)=P_demand_test
                    output_P_record(12,itime)=P_deficit
    
                    Limite_NP(9,itime) = output_P_record(10,itime) !P_demand
    
                ENDIF
                QC_record(:,itime) = QC
                
                !idays=365
                !write(*,'(I3)')idays
    
    !                 results output (daily, sum or mean values)
                IF(mod(itime,24).eq. 0)THEN
                    !Ta = sum(forcing_data(4,(itime-23):itime))/24
                    !IF(Ta.gt.0.0)GDD5 = GDD5+Ta-5.0
                    !IF(Ta.lt.0.0)GDD5 = 0
                    !WRITE(*,*)'WRITE to output_daily...CYCLE_CNP', CYCLE_CNP,idays,itime,idays
                    !WRITE(*,*) 'idays, itime, ispinup',idays, itime, ispinup
                    idays=idays+cc
                    output_daily(1,idays)=sum(output_record(1,(itime-23):itime)) !daily total Reco
                    output_daily(2,idays)=sum(output_record(2,(itime-23):itime)) !daily total Reco
                    output_daily(3,idays)=sum(output_record(3,(itime-23):itime)) !daily total NEE
                    output_daily(4,idays)=output_record(4,itime) ! LAI
                    output_daily(5,idays)=output_record(5,itime) ! BIOMASS
                    output_daily(6,idays)=NSC
                    output_daily(7,idays)=sum(output_record(6,(itime-23):itime)) !NPP
                    output_daily(8,idays)=bmleaf
                    output_daily(9,idays)=bmstem
                    output_daily(10,idays)=bmroot
                    output_daily(11,idays) = ht*100!sum(output_record(7,(itime-23):itime))
                    output_daily(12,idays) = zwt
                    output_daily(13,idays)=sum(output_record(7,(itime-23):itime)) !xnpNPP   0525, now NPP = xnpNPP
                    output_daily(14,idays)=sum(output_record(8,(itime-23):itime)) !Rhetero
                    output_daily(15,idays)=sum(output_record(9,(itime-23):itime)) !Rauto
                    output_daily(16,idays)=sum(output_record(10,(itime-23):itime)) !Rmain
                    output_daily(17,idays)=sum(output_record(11,(itime-23):itime)) !Rgrowth
                    output_daily(18,idays)=sum(output_record(12,(itime-23):itime)) !Rnitrogen
                    output_daily(19,idays)=sum(output_record(13,(itime-23):itime)) !Rphosphorus
                    output_daily(20,idays)=sum(output_record(14,(itime-23):itime)) !GrowthP
                    output_daily(21,idays)=sum(output_record(15,(itime-23):itime)) !xnpGrowthP
                    output_daily(22,idays)=sum(output_record(16,(itime-23):itime))/24. !fnsc
                    output_daily(23,idays)=output_record(17,itime) !storage 
                    output_daily(24,idays)=output_record(18,itime) !accumulation
                    output_daily(25,idays)=sum(output_record(19,(itime-23):itime)) !NPP_noadd
                    output_daily(26,idays)=sum(output_record(26,(itime-23):itime)) !EC_store
                    output_daily(27,idays)=sum(output_record(27,(itime-23):itime)) !Rauto_new
                    output_daily(28,idays)=sum(output_record(30,(itime-23):itime)) !daily total NEE_new
    
                    outputd_ccycle_Cpools(1:pools,idays)= QC
                    !WRITE(*,*)'idays,QC(8)',idays,QC(8)

                    
           
                    DO m=1,pools
                        outputd_outc(m,idays) = sum(output_OutC_record(m,(itime-23):itime))
                    ENDDO
                    DO m=1,pools
                        outputd_outn(m,idays) = sum(output_OutN_record(m,(itime-23):itime))
                    ENDDO
                    DO m=1,pools
                        outputd_outp(m,idays) = sum(output_OutP_record(m,(itime-23):itime))
                    ENDDO
                    !need to consider the sum result or the instantaneous value
                    Somega_output(1,idays) = S_omega   ! in TCS_CNP MODULE, FOR OutC, decomposition
                    Somega_output(2,idays) = W       ! in plantgrowth module, for  GrowthP, growth rate
                    Somega_output(3,idays) = Sw      ! in plantgrowth module, for  GrowthP, growth rate
                    Somega_output(4:13,idays)=wsc(:)  !soil water content mm, see water table module in soilwater function, wan 2023/3/21
                    Somega_output(14:23,idays)=wcl(:) !relative soilwater content mm3/mm3,wan 2023/3/21
                    Somega_output(24,idays)=omega
                    ST_NP_output(1:5,idays) = S_t       ! in TCS_CNP MODULE, FOR OutC, decomposition
                    ST_NP_output(6,idays)   = St        ! in plantgrowth module, for  GrowthP, growth rate
                    ST_NP_output(7,idays)   = xde       ! N,P limitation on decomposition
                    ST_NP_output(8,idays)   = xnp_leaf_limit
                    ST_NP_output(9,idays)   = xnp_stem_limit
                    ST_NP_output(10,idays)  = xnp_root_limit
                    ST_NP_output(11,idays) =  xNPuptake
                    ST_NP_output(12,idays) =  LfactorN
                    ST_NP_output(13,idays) =  LfactorP
                    ST_NP_output(14,idays) = sum(output_N_record(9,(itime-23):itime))  !N_demand
                    ST_NP_output(15,idays) = sum(output_P_record(10,(itime-23):itime)) !P_demand
                   
                    ksi_output_daily (1,idays) = sum(ksi_record(1,(itime-23):itime))/24. !S_t(1)
                    ksi_output_daily (2,idays) = sum(ksi_record(2,(itime-23):itime))/24. !S_omega
                    ksi_output_daily (3,idays) = sum(ksi_record(3,(itime-23):itime))/24. !gamma_T
                    ksi_output_daily (4,idays) = sum(ksi_record(4,(itime-23):itime))/24. !gamma_W
                    ksi_output_daily (5,idays) = sum(ksi_record(5,(itime-23):itime))/24. !L_tau_new
                    ksi_output_daily (6,idays) = sum(ksi_record(6,(itime-23):itime))/24. !S_t(1)*S_omega
    
                    outputd_Aleaf(1,idays)=sum(output_Aleaf_record(1,(itime-23):itime))
                    outputd_Aleaf(2,idays)=sum(output_Aleaf_record(2,(itime-23):itime))
                    outputd_Aleaf(3,idays)=sum(output_Aleaf_record(3,(itime-23):itime))/24.
                    outputd_Aleaf(4,idays)=sum(output_Aleaf_record(4,(itime-23):itime))/24.
                    !outputd_Aleaf(5,idays)=sum(output_Aleaf_record(5,(itime-23):itime))
    
                    !-----
                    NPPallo(1,idays)=sum(output_nppallo_record(1,(itime-23):itime))/24.
                    NPPallo(2,idays)=sum(output_nppallo_record(2,(itime-23):itime))/24.
                    NPPallo(3,idays)=sum(output_nppallo_record(3,(itime-23):itime))/24.
                    NPPallo(4,idays)=sum(output_nppallo_record(4,(itime-23):itime))/24.
                    NPPallo(5,idays)=sum(output_nppallo_record(5,(itime-23):itime))/24.
                    NPPallo(6,idays)=sum(output_nppallo_record(6,(itime-23):itime))/24.
                    NPPallo(7,idays)=sum(output_nppallo_record(7,(itime-23):itime))/24.
                    NPPallo(8,idays)=sum(output_nppallo_record(8,(itime-23):itime))/24.
                    NPPallo(9,idays)=sum(output_nppallo_record(9,(itime-23):itime))  ! add
                    NPPallo(10,idays)=sum(output_nppallo_record(10,(itime-23):itime))
                    NPPallo(11,idays)=sum(output_nppallo_record(11,(itime-23):itime))
                    NPPallo(12,idays)=sum(output_nppallo_record(12,(itime-23):itime))
                    NPPallo(13,idays)=sum(output_nppallo_record(13,(itime-23):itime))
    
                    IF(CYCLE_CNP .gt. 1 )THEN
                            !WRITE(*,*)'NNN dynamic & pools OUTPUT!'
    
                            outputd_Ndynamic(1,idays)=sum(output_N_record(1,(itime-23):itime))
                            outputd_Ndynamic(2,idays)=sum(output_N_record(2,(itime-23):itime))
                            outputd_Ndynamic(3,idays)=sum(output_N_record(3,(itime-23):itime))
                            outputd_Ndynamic(4,idays)=sum(output_N_record(4,(itime-23):itime))
                            outputd_Ndynamic(5,idays)=sum(output_N_record(5,(itime-23):itime))
                            outputd_Ndynamic(6,idays)=sum(output_N_record(6,(itime-23):itime))  !uptake
                            outputd_Ndynamic(7,idays)=sum(output_N_record(7,(itime-23):itime))  !fixation
                            outputd_Ndynamic(8,idays)=sum(output_N_record(8,(itime-23):itime))  !transfer
                            outputd_Ndynamic(9,idays)=QNminer
    
                            !WRITE(*,*)'outputd_Ndynamic(3,idays),N_leach',sum(output_N_record(3,(itime-23):itime))
                            !WRITE(*,*)'N_vol',sum(output_N_record(4,(itime-23):itime))
    
                            outputd_ncycle_Npools (1:pools,idays)=QN
                            !WRITE(*,*)'outputd_ncycle_Npools',QN
                            outputd_Ncycle_CNratios(1:pools,idays)=CN
                            !WRITE(*,*)'outputd_Ncycle_CNratios',CN
                            
                            ksi_np_output_daily(1,idays) =sum(Limite_NP(1,(itime-23):itime))/24 !xde
       
                    ENDIF
                    IF(CYCLE_CNP .gt. 2 )THEN
                            !WRITE(*,*)'PPP dynamic & pools OUTPUT!'
    
                            outputd_Pdynamic(1,idays)=sum(output_P_record(1,(itime-23):itime))    !Dsoillab
                            !WRITE(*,*)'!!!!!!!!!!!!!!!!',outputd_Pdynamic(1,idays)
                            outputd_Pdynamic(2,idays)=sum(output_P_record(2,(itime-23):itime))    !P_net
                            outputd_Pdynamic(3,idays)=sum(output_P_record(3,(itime-23):itime))    !Fptase
                            outputd_Pdynamic(4,idays)=sum(output_P_record(4,(itime-23):itime))    !P_loss
                            outputd_Pdynamic(5,idays)=sum(output_P_record(5,(itime-23):itime))    !P_uptake
                            outputd_Pdynamic(6,idays)=sum(output_P_record(6,(itime-23):itime))    !asorb
                            outputd_Pdynamic(7,idays)=sum(output_P_record(7,(itime-23):itime))    !bss
                            outputd_Pdynamic(8,idays)=sum(output_P_record(8,(itime-23):itime))    !P_miner
                            outputd_Pdynamic(9,idays)=sum(output_P_record(9,(itime-23):itime))    !P_imm
    
                            outputd_Pdynamic(10,idays)=QPlab      !Soil mineral P
                            outputd_Pdynamic(11,idays)=QPsorb
                            outputd_Pdynamic(12,idays)=QPss
                            outputd_Pdynamic(13,idays)=rootSurfaceP
                            outputd_Pdynamic(14,idays)=sum(f_rootp_rec((itime-23):itime))/24
                            outputd_Pdynamic(15,idays)=sum(FdiffP_rec((itime-23):itime))/24
    !                             outputd_Pdynamic(7,idays)=P_biominer !biochemical mineralization
                    
                            outputd_Pcycle_Ppools (1:pools,idays)=QP
                            outputd_Pcycle_CPratios(1:pools,idays)=CP
    
                            output_x(1,idays)=P_leaf
                            output_x(2,idays)=P_wood
                            output_x(3,idays)=P_root
                            output_x(4,idays)=N_leaf
                            output_x(5,idays)=N_wood
                            output_x(6,idays)=N_root
                            output_x(7,idays)=NSN
                            output_x(8,idays)=NSP
    
                    ENDIF
                    !WRITE(*,*)'GPP,Reco,NPP',sum(output_daily(1,:)),sum(output_daily(2,:)),sum(output_daily(7,:)
                ENDIF
        ENDIF
    END SUBROUTINE ForRecords
    
    SUBROUTINE ForYearRecords(start_day,end_day,simu_year)
        IMPLICIT NONE

        REAL GPP_yr,Reco_yr,NEE_yr,NPP_yr,NEEnew_yr
        REAL Rhetero_yr,Rauto_yr,Rmain_yr,Rnitrogen_yr,Rphosphorus_yr,Rauto_new_yr
        INTEGER start_day,end_day,simu_year
        REAL QC_yr(9)

        GPP_yr = sum(output_daily(1,start_day:end_day))
        Reco_yr= sum(output_daily(2,start_day:end_day)) 
        NEE_yr = sum(output_daily(3,start_day:end_day))
        NPP_yr = sum(output_daily(7,start_day:end_day))
        NEEnew_yr = sum(output_daily(28,start_day:end_day))

        QC_yr(:)= outputd_ccycle_Cpools(:,end_day) 

        Rhetero_yr = sum(output_daily(14,start_day:end_day))
        Rauto_yr  = sum(output_daily(15,start_day:end_day))
        Rauto_new_yr  = sum(output_daily(27,start_day:end_day))
        Rmain_yr  = sum(output_daily(16,start_day:end_day))
        Rnitrogen_yr = sum(output_daily(18,start_day:end_day))
        Rphosphorus_yr = sum(output_daily(19,start_day:end_day))
        !write(*,*)'output_daily_GPP',output_daily(1,1)
        IF (MCMC .EQ. 0)THEN
            write(*,*)'GPP_yr,Reco_yr,NEE_yr_noExcessC,BP_yr,NEEnew_yr',GPP_yr,Reco_yr,NEE_yr,NPP_yr,NEEnew_yr
            write(*,*)'Rhetero_yr,Rauto_yr,Rmain_yr',Rhetero_yr,Rauto_yr,Rmain_yr,Rauto_new_yr
            write(*,*)'Rnitrogen_yr,Rphosphorus_yr',Rnitrogen_yr,Rphosphorus_yr
        ENDIF


        output_yr(1,iyear) = simu_year
        output_yr(2,iyear) = GPP_yr
        output_yr(3,iyear) = Reco_yr
        output_yr(4,iyear) = NEE_yr
        output_yr(5,iyear) = NPP_yr
        output_yr(6:14,iyear) = QC_yr(:)

    END SUBROUTINE ForYearRecords


    SUBROUTINE Filespath()

     ! Define the path for output based on simulation type

        IMPLICIT NONE
        CHARACTER(len=99) fold

        ! Determine the folder path based on the simulation conditions
        IF (MCMC .EQ. 0) THEN
            IF (NDSPINUP .EQ. 1) THEN
                fold = 'NDSPINUP/' ! Path for the spin-up procedure
            ELSEIF (SensTest .EQ. 0 .and. MCMC .EQ. 0 .and. &
                &   NDSPINUP .EQ. 0 )THEN
                fold = 'sim/'      ! Path for normal simulation
            ELSEIF (SensTest .EQ. 1 .and. MCMC .EQ. 0 .and. &
                &   NDSPINUP .EQ. 0 )THEN
                fold = 'SensTest/' ! Path for sensitivity testing
            ENDIF
        ELSE
            fold = 'MCMC/'         ! Path for MCMC simulations
        ENDIF

        IF(CYCLE_CNP .eq. 1) THEN
        fileplace = '../output/'//TRIM(ADJUSTL(fold))//'teco_c/'
        filesignal = '_c.csv'
        ELSEIF(CYCLE_CNP .eq. 2)THEN
        fileplace = '../output/'//TRIM(ADJUSTL(fold))//'teco_cn/'
        filesignal = '_cn.csv'
        ELSEIF(CYCLE_CNP .eq. 3)THEN
        fileplace = '../output/'//TRIM(ADJUSTL(fold))//'teco_cnp/'
        filesignal='_cnp.csv'
        ENDIF


    END SUBROUTINE Filespath
    
    
    SUBROUTINE FilesForOutput()

    ! Subroutine for configuring the file path for model output.

        IMPLICIT NONE

        OPEN(211, file=TRIM(ADJUSTL(fileplace))//'teco_simu_cflux'//filesignal) 
        OPEN(212, file=TRIM(ADJUSTL(fileplace))//'teco_8cpools'//filesignal)
        OPEN(213, file=TRIM(ADJUSTL(fileplace))//'teco_outc'//filesignal)
        OPEN(21313, file=TRIM(ADJUSTL(fileplace))//'teco_outn'//filesignal)
        OPEN(2131313, file=TRIM(ADJUSTL(fileplace))//'teco_outp'//filesignal)
        OPEN(214, file=TRIM(ADJUSTL(fileplace))//'NPP_allocation_fraction'//filesignal)
        OPEN(998, file=TRIM(ADJUSTL(fileplace))//'S_omega_record'//filesignal)
        OPEN(997, file=TRIM(ADJUSTL(fileplace))//'ST_NP'//filesignal)
        OPEN(996, file=TRIM(ADJUSTL(fileplace))//'Rh_pools'//filesignal) 
        open(215, file=TRIM(ADJUSTL(fileplace))//'Aleaf'//filesignal)
        OPEN (191919,file=TRIM(ADJUSTL(fileplace))//'YearOutPut'//filesignal)
        open(8881,file=TRIM(ADJUSTL(fileplace))//'QC_record'//filesignal)

        !CHANGE the output filepath for different configuration

        IF(CYCLE_CNP .gt. 1)THEN
            OPEN(771,file=TRIM(ADJUSTL(fileplace))//'CNP_Ncycle_simu_pools'//filesignal)
            OPEN(772,file=TRIM(ADJUSTL(fileplace))//'CNP_Ncycle_simu_ratios'//filesignal)
            OPEN(773,file=TRIM(ADJUSTL(fileplace))//'CNP_NNNcycle_dynamics'//filesignal)
            OPEN(774,file=TRIM(ADJUSTL(fileplace))//'Limit_NP_record'//filesignal)
            OPEN(775,file=TRIM(ADJUSTL(fileplace))//'Limit_NP_loops'//filesignal)
            open(665,file=TRIM(ADJUSTL(fileplace))//'plant_NP_allo_record'//filesignal)
        ENDIF

        IF(CYCLE_CNP .gt. 2)THEN

            OPEN(661,file=TRIM(ADJUSTL(fileplace))//'CNP_Pcycle_simu_pools'//filesignal)
            OPEN(662,file=TRIM(ADJUSTL(fileplace))//'CNP_Pcycle_simu_ratios'//filesignal)
            OPEN(663,file=TRIM(ADJUSTL(fileplace))//'CNP_PPPcycle_dynamics'//filesignal)
        ENDIF
        
        IF(NDSPINUP .EQ. 1) THEN
            OPEN(999,file=TRIM(ADJUSTL(fileplace))//'spinup_LoopVariables'//filesignal)
            IF (CYCLE_CNP .GT. 1) OPEN(9991,file=TRIM(ADJUSTL(fileplace))//'spinup_LoopVariablesNNN'//filesignal)
            IF (CYCLE_CNP .GT. 2) OPEN(9992,file=TRIM(ADJUSTL(fileplace))//'spinup_LoopVariablesPPP'//filesignal)
        ENDIF
        
        open(1212,file=TRIM(ADJUSTL(fileplace))//'simu_cflux_record'//filesignal)

        IF(nyear .gt. 1 .OR. (nyear .eq. 1 .AND. NDSPINUP .eq. 1 ))THEN
            open(216,file=TRIM(ADJUSTL(fileplace))//'simu_cflux_lastyear'//filesignal)
            open(12,file=TRIM(ADJUSTL(fileplace))//'simu_cflux_lastyear_record'//filesignal)
            open(217,file=TRIM(ADJUSTL(fileplace))//'simu_8cpools_lastyear'//filesignal)
            IF(CYCLE_CNP .gt. 1)THEN
                OPEN(7711,file=TRIM(ADJUSTL(fileplace))//'CNP_Ncycle_simu_pools_lastyear'//filesignal)
                OPEN(7722,file=TRIM(ADJUSTL(fileplace))//'CNP_Ncycle_simu_ratios_lastyear'//filesignal)
                OPEN(7733,file=TRIM(ADJUSTL(fileplace))//'CNP_NNNcycle_dynamics_lastyear'//filesignal)
            ENDIF
            IF(CYCLE_CNP .gt. 2)THEN
                OPEN(6611,file=TRIM(ADJUSTL(fileplace))//'CNP_Pcycle_simu_pools_lastyear'//filesignal)
                OPEN(6622,file=TRIM(ADJUSTL(fileplace))//'CNP_Pcycle_simu_ratios_lastyear'//filesignal)
                OPEN(6633,file=TRIM(ADJUSTL(fileplace))//'CNP_PPPcycle_dynamics_lastyear'//filesignal)
            ENDIF
        ENDIF

    END SUBROUTINE FilesForOutput

    SUBROUTINE WriteFiles_noMCMC()
        
    ! Customize the outputs

        IMPLICIT NONE
        INTEGER m,n
        DO m=1,nyear
            WRITE(191919,'(*( G0.8, :, ",", X))')output_yr(:,m)
        ENDDO

        DO m=1,output_ndays
            WRITE(211,'(*( G0.8, :, ",", X))')output_daily(:,m)
            WRITE(212,'(*( G0.8, :, ",", X))')outputd_ccycle_Cpools(:,m)                 

            WRITE(213,'(*( G0.8, :, ",", X))')outputd_outc(:,m)
            WRITE(21313,'(*( G0.8, :, ",", X))')outputd_outn(:,m)
            WRITE(2131313,'(*( G0.8, :, ",", X))')outputd_outp(:,m)
            WRITE(214,'(*( G0.8, :, ",", X))')NPPallo(:,m)
            WRITE(998,'(*( G0.8, :, ",", X))')Somega_output(:,m)
            WRITE(997,'(*( G0.8, :, ",", X))')ST_NP_output(:,m)

            IF(CYCLE_CNP .gt. 1)THEN
                WRITE(771,'(*( G0.8, :, ",", X))')outputd_ncycle_Npools(:,m)
                WRITE(772,'(*( G0.8, :, ",", X))')outputd_Ncycle_CNratios(:,m)    
                WRITE(773,'(*( G0.8, :, ",", X))')outputd_Ndynamic(:,m)
            ENDIF
            IF(CYCLE_CNP .gt. 2)THEN
                WRITE(661,'(*( G0.8, :, ",", X))')outputd_Pcycle_Ppools(:,m)
                WRITE(662,'(*( G0.8, :, ",", X))')outputd_Pcycle_CPratios(:,m)
                WRITE(663,'(*( G0.8, :, ",", X))')outputd_Pdynamic(:,m)
            ENDIF
        ENDDO

        DO n=1,force_nhours
            WRITE(8881,'(*( G0.8, :, ",", X))')QC_record(:,n)
            WRITE(1212,'(*( G0.8, :, ",", X))')output_record(:,n)
            WRITE(996,'(*( G0.8, :, ",", X))')Rh_pools_record(:,n)
            WRITE(215,'(*( G0.8, :, ",", X))')output_Aleaf_record(:,n)
        ENDDO
    !3- N or NP cycle related output - hourly
    IF(CYCLE_CNP .ge. 2)THEN
        DO n=1,force_nhours
            WRITE(665,'(*( G0.8, :, ",", X))')output_x_record(:,n)
            WRITE(774,'(*( G0.8, :, ",", X))')Limite_NP(:,n)
        ENDDO
        CLOSE(665)
        CLOSE(774)
    ENDIF

    IF(nyear .gt. 1 .OR. (nyear .eq. 1 .AND. NDSPINUP .eq. 1 ))THEN
    ! if we have long-term forcing, we want to keep the results of the last year for our convenient 
        DO n =output_ndays-lastyear_days+1,output_ndays
            WRITE(216,'(*( G0.8, :, ",", X))')output_daily(:,n)
            WRITE(217,'(*( G0.8, :, ",", X))')outputd_ccycle_Cpools(:,n)
            IF(CYCLE_CNP .gt. 1)THEN
                    WRITE(7711,'(*( G0.8, :, ",", X))')outputd_ncycle_Npools(:,n)
                    WRITE(7722,'(*( G0.8, :, ",", X))')outputd_Ncycle_CNratios(:,n)    
                    WRITE(7733,'(*( G0.8, :, ",", X))')outputd_Ndynamic(:,n)
            ENDIF
            IF(CYCLE_CNP .gt. 2)THEN
                    WRITE(6611,'(*( G0.8, :, ",", X))')outputd_Pcycle_Ppools(:,n)
                    WRITE(6622,'(*( G0.8, :, ",", X))')outputd_Pcycle_CPratios(:,n)
                    WRITE(6633,'(*( G0.8, :, ",", X))')outputd_Pdynamic(:,n)
            ENDIF 

        ENDDO

        DO n= force_nhours-lastyear_hours+1,force_nhours
            WRITE(12,'(*( G0.8, :, ",", X))')output_record(:,n)
        ENDDO
    ENDIF

    IF (NDSPINUP .EQ. 1 ) THEN
        DO m=1,ispinup   !or nspinup-1?? or  = ispinup ??
            WRITE(999,'(*( G0.8, :, ",", X))') Loop_variables(:,m)
            IF (CYCLE_CNP .GT. 1) WRITE(9991,'(*( G0.8, :, ",", X))')Loop_variablesNNN(:,m)
            IF (CYCLE_CNP .GT. 2) WRITE(9992,'(*( G0.8, :, ",", X))')Loop_variablesPPP(:,m)
        ENDDO
    ENDIF  ! END the output when SpinUp is activated

    END SUBROUTINE WriteFiles_noMCMC
END MODULE outputs_mod






