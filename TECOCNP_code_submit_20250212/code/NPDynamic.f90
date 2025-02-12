!SUBROUTINE: Phosphorus Cycle
!PURPOSE：   Run the phosphorus cycle in TECO
!Code by:    Fangxiu Wan
!Date:       2022-09

!-----------------------------------------------
!Main pools: 8 organic P pools + 1 non-structure P pool, 4 inorgainc P pools,
!P Dynamic of 8 pools followed the C dynamic, but have the resorption/retranslocated additionally
!Process: 1- uptake(demand-uptake),2-transfer, 3-retranslocated,
!         4-immobilization, 5-mineralization, 6-biochemical mineralization
!         7-adsorb, 8-stronly adsorb, 9-occluded
!-----------------------------------------------
MODULE NPmin_imm

    !USE TransFraction
    USE IntersVariables
    IMPLICIT NONE
    CONTAINS
    SUBROUTINE N_mineralization()
    !USE IntersVariables
    IMPLICIT NONE
    !   mineralization = C decomposition rate * C pools * NC ratio.
        N_miner2=OutN(5)+OutN(6)+OutN(7)+OutN(8)+OutN(9)
    !   un-limited situation
        N_miner22=OutN2(5)+OutN2(6)+OutN2(7)+OutN2(8)+OutN2(9)

    END SUBROUTINE N_mineralization

    SUBROUTINE P_mineralization()
    !USE IntersVariables
    IMPLICIT NONE

        P_miner2=OutP(5)+OutP(6)+OutP(7)+OutP(8)+OutP(9)
        P_miner22=OutP2(5)+OutP2(6)+OutP2(7)+OutP2(8)+OutP2(9) 

    END SUBROUTINE P_mineralization

!==============================================

    SUBROUTINE N_immobilization_old(N_imm2)
    !USE IntersVariables
    IMPLICIT NONE
        !REAL N_imm(5)!,N_immob,N_miner,N_net,QNminer
        REAL N_imm2(3),N_imm22(3)
        !REAL CN(8),CN0(8),QC(8)
        INTEGER i
!       TECO-CN version
        !N_imm=0.
        !N_immob=0.
        !IF(QNminer>0)THEN
        !    DO i=5,9
        !        IF(CN(i) .ge. CN0(i))THEN  
        !            N_imm(i-4)=Amin1(QC(i)/CN0(i)-QC(i)/CN(i), 0.05*QNminer)  !wan 2023/3/18 change 0.1 to 0.01
        !        ELSE                                                          !wan 2023/3/18, add this selection
        !            !N_imm(i-3) =Amin1(QC(i)/CN(i)-QC(i)/CN0(i),0.05*QNminer)
        !            N_imm = 0.000001
        !            !write(*,*)'N_immob',N_immob
        !        ENDIF
        !        N_immob=N_immob+N_imm(i-4)
        !    ENDDO
        !ENDIF

!       This is a debug for original TECO-CN
        !IF(N_immob .gt. QNminer)THEN
        !    write(*,*)'N_immob>QNminer'   !N_immob < QNminer at each step, but, N_immob will greater than QNminer
        !    write(*,*)'N_immob,QNminer',N_immob,QNminer
        !    Flag_N = 1
            !STOP
        !ENDIF
        !wan: Above equations get wrong results, now debug it 
        !wan: lower C:N ratio of the receive pool than its donor pools  

        !N_net=N_miner-N_immob  

!       2024 01 25 wan
!       reference Goll et al., 2017; Wang et al., 2007; 2010        
        N_imm2(1) = f_F2M*OutC(5)*(1/CN(5)) &  !fine litter to metabolic soil (fast soil)
                    + f_C2M*OutC(6)*(1/CN(6)) &  !coarse litter to metabolic soil
                    + f_S2M*OutC(8)*(1/CN(8)) &  !slow soil to metabolic soil
                    + f_P2M*OutC(9)*(1/CN(9))    !passive soil to metabolic soil
        N_imm2(2) = f_C2S*OutC(6)*(1/CN(6)) &  !coarse litter to slow soil
                    + f_M2S*OutC(7)*(1/CN(7))  !metabolic soil to slow soil
        N_imm2(3) = f_M2P*OutC(7)*(1/CN(7)) &  !metabolic soil to passive soil
                    + f_S2P*OutC(8)*(1/CN(8))    !slow soil to passive soil

        N_immob2 = N_imm2(1)+N_imm2(2)+N_imm2(3)
        N_net2 = N_miner2 - N_immob2

        ! for xde
        N_imm22(1) = f_F2M*OutC2(5)*(1/CN(5)) &  !fine litter to metabolic soil (fast soil)
                + f_C2M*OutC2(6)*(1/CN(6)) &  !coarse litter to metabolic soil
                + f_S2M*OutC2(8)*(1/CN(8)) &  !slow soil to metabolic soil
                + f_P2M*OutC2(9)*(1/CN(9))    !passive soil to metabolic soil
        N_imm22(2) = f_C2S*OutC2(6)*(1/CN(6)) &  !coarse litter to slow soil
                + f_M2S*OutC2(7)*(1/CN(7))  !metabolic soil to slow soil
        N_imm22(3) = f_M2P*OutC2(7)*(1/CN(7)) &  !metabolic soil to passive soil
                + f_S2P*OutC2(8)*(1/CN(8))    !slow soil to passive soil

        N_immob22 = N_imm22(1)+N_imm22(2)+N_imm22(3)
        N_net22 = N_miner22 - N_immob22
        write(*,*)'N_net22', N_net22

    END SUBROUTINE N_immobilization_old

    SUBROUTINE N_immobilization_old2(N_imm2)
        !USE IntersVariables
        IMPLICIT NONE
            REAL N_imm2(3),N_imm22(3)
            INTEGER i,j

    ! NC ratio of newly formed soil matter. wan 20240906. Ref: CABLE
    ! 1-fast som; 2-slow som; 3-passive som
            DO j = 1,3
                IF(QNminer < 2)THEN
                    newsoilNC(j) = newsoilNCmin(j)+(newsoilNCmax(j)-newsoilNCmin(j))*QNminer/2
                ELSE
                    newsoilNC(j)=newsoilNCmax(j)
                ENDIF
            ENDDO

    !       2024 01 25 wan
    !       reference Goll et al., 2017; Wang et al., 2007; 2010        
            N_imm2(1) = f_F2M*OutC(5)*newsoilNC(1) &  !fine litter to metabolic soil (fast soil)
                        + f_C2M*OutC(6)*newsoilNC(1) &  !coarse litter to metabolic soil
                        + f_S2M*OutC(8)*newsoilNC(1) &  !slow soil to metabolic soil
                        + f_P2M*OutC(9)*newsoilNC(1)    !passive soil to metabolic soil
            N_imm2(2) = f_C2S*OutC(6)*newsoilNC(2) &  !coarse litter to slow soil
                        + f_M2S*OutC(7)*newsoilNC(2)  !metabolic soil to slow soil
            N_imm2(3) = f_M2P*OutC(7)*newsoilNC(3)&   !metabolic soil to passive soil
                        + f_S2P*OutC(8)*newsoilNC(3)  !slow soil to passive soil
    
            N_immob2 = N_imm2(1)+N_imm2(2)+N_imm2(3)
            N_net2 = N_miner2 - N_immob2
    
            ! for xde
            N_imm22(1) = f_F2M*OutC2(5)*newsoilNC(1) &  !fine litter to metabolic soil (fast soil)
                        + f_C2M*OutC2(6)*newsoilNC(1) &  !coarse litter to metabolic soil
                        + f_S2M*OutC2(8)*newsoilNC(1) &  !slow soil to metabolic soil
                        + f_P2M*OutC2(9)*newsoilNC(1)    !passive soil to metabolic soil
            N_imm22(2) = f_C2S*OutC2(6)*newsoilNC(2) &  !coarse litter to slow soil
                        + f_M2S*OutC2(7)*newsoilNC(2)  !metabolic soil to slow soil
            N_imm22(3) = f_M2P*OutC2(7)*newsoilNC(3)&  !metabolic soil to passive soil
                        + f_S2P*OutC2(8)*newsoilNC(3)   !slow soil to passive soil
    
            N_immob22 = N_imm22(1)+N_imm22(2)+N_imm22(3)
            N_net22 = N_miner22 - N_immob22
            !write(*,*)'new,N_net22', N_net22
            !IF(N_net22 .lt. 0) stop
     
        END SUBROUTINE N_immobilization_old2

        SUBROUTINE N_immobilization(N_imm2)
            !USE IntersVariables
            IMPLICIT NONE
                REAL N_imm2(3),N_imm22(3)
                INTEGER i,j
    
        !       2024 01 25 wan
        !       reference Goll et al., 2017; Wang et al., 2007; 2010        
                N_imm2(1) = (f_F2M*OutC(5)+f_C2M*OutC(6)+f_S2M*OutC(8)+f_P2M*OutC(9))&
                            & *(1/CN(7))
                N_imm2(2) = (f_C2S*OutC(6)+f_M2S*OutC(7))*(1/CN(8))
                N_imm2(3) = (f_M2P*OutC(7)+f_S2P*OutC(8))*(1/CN(9)) 
        
                N_immob2 = N_imm2(1)+N_imm2(2)+N_imm2(3)
                N_net2 = N_miner2 - N_immob2
        
                ! for xde
                N_imm22(1) = (f_F2M*OutC2(5)+f_C2M*OutC2(6)+f_S2M*OutC2(8)+f_P2M*OutC2(9))&
                            & *(1/CN(7))
                N_imm22(2) = (f_C2S*OutC2(6)+f_M2S*OutC2(7))*(1/CN(8))
                N_imm22(3) = (f_M2P*OutC2(7)+f_S2P*OutC2(8))*(1/CN(9)) 
        
                N_immob22 = N_imm22(1)+N_imm22(2)+N_imm22(3)
                N_net22 = N_miner22 - N_immob22
                !write(*,*)'QN_soil', QN(7:9),newsoilNC,QNminer,f_F2M*OutC2(5)*newsoilNC(1),f_F2M*OutC2(5)*(1/CN(7))
                !IF(N_net22 .lt. 0) stop
         
            END SUBROUTINE N_immobilization


!P immobilization-----------------------------------------
!Ref   : TECO Nitrogen immobilization, ZhenggangDu et al., 2018-(13)
!INPUT : CP, CP0, QC, QPlab
!OUTPUT: P_immob (gPm-2day-1?)
!CABLE : P immobilization rate is calculated as the N immobilization rate 
!        divided by the N:P ratio of different soil pools. 
    SUBROUTINE P_immobilization_old(P_imm2)
    !USE IntersVariables
    IMPLICIT NONE
        !REAL P_imm(5)!,P_immob,P_miner,P_net,QPlab
        REAL P_imm2(3),P_imm22(3)
        INTEGER i
        ! The original calculation of Shi (2016) and Du (2018)
        !IF(QPlab>0)THEN        
        !    DO i=5,9
        !        IF(CP(i) .ge. CP0(i))THEN   ! P amount is less than the initial setting
        !            P_imm(i-4)=Amin1(QC(i)/CP0(i)-QC(i)/CP(i),0.05*QPlab)
                    !WRITE(*,*)'1,QC(i)/CP0(i)-QC(i)/CP(i),0.1*QPlab',QC(i)/CP0(i)-QC(i)/CP(i),0.1*QPlab
        !        ELSE
                    !P_imm(i-3)=Amin1(QC(i)/CP(i)-QC(i)/CP0(i),0.05*QPlab)
        !            P_imm = 0.000001
                    !WRITE(*,*)'2,QC(i)/CP(i)-QC(i)/CP0(i),0.1*QPlab',QC(i)/CP(i)-QC(i)/CP0(i),0.1*QPlab
        !        ENDIF
        !        P_immob=P_immob+P_imm(i-4)
        !    ENDDO
        !ENDIF
        !P_net = P_miner-P_immob !! net mineralization 
        !Write(*,*)'P_immob',P_immob
        !write(*,*)'P_net',P_net,QPlab

        ! 2024 01 25 wan created this part. 
        ! reference: Goll et al., 2017; Wang et al., 2007; 2010
        ! the simplified equation, but we need sperated immobolization flux for each pools , wan
        !P_immob2 = OutP2(5)*f_F2M+ OutP2(6)*(f_C2M + f_C2S)+OutP2(7)*(f_M2S + f_M2P) &
        !            +OutP2(8)*(f_S2P + f_S2M)+OutP2(9)*f_P2M

        !write(*,*)'1-P_immob2',P_immob2


        P_imm2(1) = f_F2M*OutC(5)*(1/CP(5)) &  !fine litter to metabolic soil (fast soil)
                    + f_C2M*OutC(6)*(1/CP(6)) &  !coarse litter to metabolic soil
                    + f_S2M*OutC(8)*(1/CP(8)) &  !slow soil to metabolic soil
                    + f_P2M*OutC(9)*(1/CP(9))    !passive soil to metabolic soil
        P_imm2(2) = f_C2S*OutC(6)*(1/CP(6)) &  !coarse litter to slow soil
                    + f_M2S*OutC(7)*(1/CP(7))   !metabolic soil to slow soil
        P_imm2(3) = f_M2P*OutC(7)*(1/CP(7)) &  !metabolic soil to passive soil
                    + f_S2P*OutC(8)*(1/CP(8))    !slow soil to passive soil

        P_immob2 = P_imm2(1)+P_imm2(2)+P_imm2(3)
        !write(*,*)'2-P_immob2',P_immob2
        P_net2 = P_miner2 - P_immob2

        P_miner2_rec(itime) = P_miner2
        P_immob2_rec(itime) = P_immob2
        P_net2_rec(itime) = P_net2
        
        !write(*,*)'OutC(5:9)',OutC(5:9)
        !write(*,*)'P_net2,P_miner2,P_immob2',P_net2,P_miner2,P_immob2,CP

! This is for xde. 
! The limitation for decomposition (xde) considering un-limited situation

        P_imm22(1) = f_F2M*OutC2(5)*(1/CP(5)) &  !fine litter to metabolic soil (fast soil)
                    + f_C2M*OutC2(6)*(1/CP(6)) &  !coarse litter to metabolic soil
                    + f_S2M*OutC2(8)*(1/CP(8)) &  !slow soil to metabolic soil
                    + f_P2M*OutC2(9)*(1/CP(9))    !passive soil to metabolic soil
        P_imm22(2) = f_C2S*OutC2(6)*(1/CP(6)) &  !coarse litter to slow soil
                    + f_M2S*OutC2(7)*(1/CP(7))   !metabolic soil to slow soil
        P_imm22(3) = f_M2P*OutC2(7)*(1/CP(7)) &  !metabolic soil to passive soil
                    + f_S2P*OutC2(8)*(1/CP(8))    !slow soil to passive soil

        P_immob22 = P_imm22(1)+P_imm22(2)+P_imm22(3)
        P_net22 = P_miner22 - P_immob22

        !write(*,*)'P_net22',P_net22,P_miner22,P_immob22

    END SUBROUTINE P_immobilization_old

    SUBROUTINE P_immobilization(P_imm2)
        !USE IntersVariables
        IMPLICIT NONE
        !REAL P_imm(5)!,P_immob,P_miner,P_net,QPlab
        REAL P_imm2(3),P_imm22(3)
        INTEGER i

        P_imm2(1) = (f_F2M*OutC(5)+f_C2M*OutC(6)+f_S2M*OutC(8)+f_P2M*OutC(9))&
                    & *(1/CP(7))    !passive soil to metabolic soil
        P_imm2(2) = (f_C2S*OutC(6)+f_M2S*OutC(7))*(1/CP(8))   !metabolic soil to slow soil
        P_imm2(3) = (f_M2P*OutC(7)+f_S2P*OutC(8))*(1/CP(9))    !slow soil to passive soil

        P_immob2 = P_imm2(1)+P_imm2(2)+P_imm2(3)
        P_net2 = P_miner2 - P_immob2

        P_miner2_rec(itime) = P_miner2
        P_immob2_rec(itime) = P_immob2
        P_net2_rec(itime) = P_net2

! This is for xde. 
! The limitation for decomposition (xde) considering un-limited situation

        P_imm22(1) = (f_F2M*OutC2(5)+f_C2M*OutC2(6)+f_S2M*OutC2(8)+f_P2M*OutC2(9))&
                    & *(1/CP(7))    !passive soil to metabolic soil
        P_imm22(2) = (f_C2S*OutC2(6)+f_M2S*OutC2(7))*(1/CP(8))   !metabolic soil to slow soil
        P_imm22(3) = (f_M2P*OutC2(7)+f_S2P*OutC2(8))*(1/CP(9))    !slow soil to passive soil

        P_immob22 = P_imm22(1)+P_imm22(2)+P_imm22(3)
        P_net22 = P_miner22 - P_immob22

        !write(*,*)'P_net22',P_net22,P_miner22,P_immob22

    END SUBROUTINE P_immobilization
    
END MODULE NPmin_imm

MODULE NPleaching
USE IntersVariables
IMPLICIT NONE
CONTAINS
    SUBROUTINE Nleaching(runoff,rdepth,N_deN0,Tsoil,LDON0,N_loss,&
                        Scalar_N_flow,Scalar_N_T)
    !USE IntersVariables
    IMPLICIT NONE 
        REAL runoff,rdepth,N_deN0,Tsoil,LDON0,N_loss
        REAL Scalar_N_flow,Scalar_N_T


        ! Loss of mineralized N and dissolved organic N
        ! Both are proportional to the availability of soil mineral N(gN m-2)
        !Scalar_N_flow=0.5*runoff/rdepth
        Scalar_N_flow=0.05*runoff/rdepth   !wan: 0.5 experience coefficience, f_nleach, use rdepth or h_depth?
        Scalar_N_T=N_deN0*exp((Tsoil-25.)/10.)    !wan: N_deN0 = f_ngas in Du et al., 2018
        N_leach=Scalar_N_flow*QNminer!+Scalar_N_flow*QN(7)*LDON0  !wan: change QN(6)*LDON0 to QNminer ? 
        N_vol  =Scalar_N_T*QNminer
        N_loss =N_leach + N_vol

    END SUBROUTINE Nleaching


    SUBROUTINE Pleaching(runoff,rdepth,Scalar_P_flow)
    IMPLICIT NONE
        REAL runoff,rdepth,Scalar_P_flow,P_leach
        P_leach  = 0.0
    !-----------------------------------------------------
    !P leaching
    !REF: TECO Loss of mineralized N and dissolved organic N,
    !P_leaching: Function of soil mineral P pool and runoff
    !CABLE, Pleach = fleach*labile P pool
        Scalar_P_flow=5.0E-6*runoff/rdepth  ! in Du et al., 2018, use the h_depth, soil depth
        ! 5E-7 0.005 year-1 /8760 Wang et al., 2007 Notion lp
    !     Scalar_P_T=P_deP0*exp((Tsoil-25.)/10.) !for gass loss     
        P_leach=Scalar_P_flow*QPlab!+Scalar_P_flow*QP(7)*1.E-4!LDOP0  
        !Scalar_P_flow*QPlab -- labile P leaching
        !Scalar_P_flow*QN(6)*LDOP0 -- Dissoved P leaching
        P_leach_rec(itime) = P_leach
        !IF (itime .EQ. 8760) write(*,*)'P_leach_rec',sum(P_leach_rec(:))
        P_loss =P_leach

    !      write(*,*)'Step1-6 Qplab,P_loss',Qplab,P_loss

    END SUBROUTINE Pleaching

END MODULE NPleaching


MODULE P_specific_process
    !USE TransFraction
    !USE IntersVariables
    USE NPleaching
    IMPLICIT NONE
    CONTAINS

SUBROUTINE Bioc_miner(N_cost)
USE IntersVariables
IMPLICIT NONE
    REAL costNpup,prodptase,N_cost


!P biochemical mineralization
!Ref   : CABLE, biochemical P mineralization rate(g P m-2 d-1), Wang et al., 2010 D11
!        soil(3) - passive, soil(2) - slow
!        prodptase - biome-specific
!        Phosphatase production will start when λ_pup > λ__Ptase
!INPUT : PARAMETERS - prodptase, costNpup; tauC,QP
!OUTPUT: biochemical P mineralization rate (gPm-2day-1)   ---  gPm-2h-1
!Consder the C cost?

!Parameters
costNpup  = 25  !gN/gP
prodptase = 0.2 !ref: Wang et al., 2007; 2010 Uptase

    Fptase = prodptase & 
                & * max(0.0,(QP(8)/tauC(8))+QP(9)/tauC(9)) &
                & * max(0.0,(costNpup-15.0)) &
                & / (max(0.0,(costNpup-15.0)+150))     !in teco, tauC hourly, so Fptase hourly
      !prodptase  - u_pmax maximum specific biochemical P mineralization rate (d−1)
      !costNpup - N cost of plant root P uptake, gN/gP λ_pup
      !15  -  biome-soecific N cost of phosphatase production,  λ__Ptase
      !150 - Michaelis-Menten constant for biochemical P mineralization, gN/gP = 150, K_ptase

    IF(CP(8) .LT. (1+0.05)*CP0(8) .and. CP(9) .LT. (1+0.05)*CP0(9))THEN
        Fptase_slow = Fptase*OutP(8)/(OutP(8)+OutP(9))
        Fptase_pass= Fptase*OutP(9)/(OutP(8)+OutP(9)) 
    ELSEIF(CP(8) .GE. (1+0.05)*CP0(8) .and. CP(9) .LT. (1+0.05)*CP0(9))THEN
        Fptase_slow = 0.0
        Fptase_pass= Fptase
    ELSEIF(CP(8) .LT. (1+0.05)*CP0(8) .and. CP(9) .GE. (1+0.05)*CP0(9))THEN
        Fptase_slow = Fptase
        Fptase_pass= 0.0
    ELSE
        Fptase = 0.0
        Fptase_slow = 0.0
        Fptase_pass = 0.0
    ENDIF
    
      N_cost = Fptase*costNpup
      !write(*,*)'Fptase,N_cost',Fptase,N_cost
      !write(*,*)'Fptase,prodptase,QP(7:8),costNpup,tauC',&
      !& Fptase,prodptase,QP(7:8),costNpup,tauC
      !write(*,*)'Fptase*OutP(8)/(OutP(7)+OutP(8))',Fptase*OutP(8)/(OutP(7)+OutP(8))

    !Wang et al., 2010 D7
    !P_biominer = Fptase*((tauC(7)*QP(7))/(tauC(7)*QP(7)+tauC(8)*QP(8)))
    !write(*,*)'Fptase',Fptase
END SUBROUTINE Bioc_miner

SUBROUTINE SorbedPDynamic(runoff,rdepth,QPplant,Pwea,Pdep,&
                         & N_cost,P_fert)

    USE IntersVariables
    IMPLICIT NONE
    REAL  runoff,rdepth
    REAL  QPplant
    REAL  xDsoillab,Dsoilsorb,Dsoilss
    REAL  P_imm(5) !P_immob = sum(P_imm)
    REAL  P_imm2(3),P_imm22(3)
    !P biochemical mineralization
    REAL prodptase !Phosphate production, biome-specific in CABLE
      ! same as u_pmax, the maximun specific biochemical P mineralization rate(d-1)
    REAL costNpup,Pup0 !N cost of P uptake 40gN/P for tropical biomes
      ! and 25gN/g P for other biomes
    REAL  Psorbmax,kmlabP,xkplab,xkpsorb,xkpss !biome-specific,Parameters for Langmuir equation
    REAL  kplab,kpsorb,kpss   !rate
    REAL  Pdep,Pwea,P_fert!P input
    REAL  Scalar_P_flow,LDOP0
    REAL  P_biominer 
    REAL  xkoptsoil,xksoil,N_cost
    REAL  kss_2,Qss_2,Dss_2,css2,Dsoillab2  !wan 2023/06/06

CALL Bioc_miner(N_cost)
!CALL Pleaching(runoff,rdepth,Scalar_P_flow,P_leach,P_loss,QPlab)  !wan Aug27, should after the Dsoillab update

!write(*,*)'Fptase',Fptase

!P uptake and demand--------------------------------------
!In new subroutine
!need compare with CABLE
!P transfer, uptake cost,biochemical mineralization，NSP=NSP+P_transfer+P_uptake+P_fixatioin
!OUTPUT: P_leaf,P_wood,P_root,P_uptake


!-------------------------------------------------------
!4 Inorganic P pools----------------------------------------------
!Ref: CABLE
! QP_in(4)  inorganic P pools - labile(QPlab), sorbed(QPsorb), strongly sorbed(QPssorb)
!                               occluded(QPss)
! P input: wethring, deposition, fertilization, labile P -- same as Mineral P
! QPlab = QPlab+P_miner+Pdep+Pwea -(leaching,uptake,sorbed,occluded,immob

      !soil lable P change
      !decompositioin rate (Ref:CABLE)
      !xkoptsoil -- biome/vegetation-specific, xkplab,xkpsorb,xkpss -- soil order-specific

!xk(mp):    modifier of soil litter decomposition rate (dimensionless)
!CABLE      xktemp(npt)  = casabiome%q10soil(veg%iveg(npt))**(0.1*(tsavg(npt)-TKzeroC-35.0))
!CABLE      xkwater(npt) = ((fwps(npt)-wfpscoefb)/(wfpscoefa-wfpscoefb))**wfpscoefe    &
!CABLE               * ((fwps(npt)-wfpscoefc)/(wfpscoefa-wfpscoefc))**wfpscoefd

      !soil scaling factor in TECO: S_omega(soil moisture limitation factor)
      !                             S_t(soil temperature limitation factor)
      !                             S_Nfine,Ncoarse,Nmicr,Nslo,Npass
!     xksoil(npt) = xkoptsoil  * xktemp(npt) * xkwater(npt) 

! before update

    Dsoillab  = 0.0
    Dsoilsorb = 0.0
    Dsoilss  = 0.0
    P_biominer= 0.0
    Psorbmax  = 133 !gPm-2 maximum amount of sorbed P
    kmlabp    = 64  !gPm-2
    !Pwea = 0.005/8760 !covert gPm-2year-1 to gP m-2 hour-1
    !xksoil = 1.0
    xkoptsoil = 0.6 !CABLE
    !xkoptsoil = 0.3 !wan
    xksoil = xkoptsoil * S_t(1) * S_omega   !wan:S_t(1)=S_t(2)=3=4=5

    kpsorb = 0.0067/8760 *xksoil !ref: Wang et al., 2010
    !kpss = 0.0067/8760 *xksoil
    kpss = 0.00067/8760 *xksoil !wan 20240906

    asorb = kpsorb*QPsorb   !wan: new variables for output, k mean turnover rate of P, 
    !asorb, from labile and sorbed P to occ P -- sencondary P. occ means sencondary
    bss =  kpss*QPss     !wan 2023/3/17, add xkpss*
    !bss, from sencondary to labile and sorbed. ss is sencondary Pi
    !update 20240416 there is no P from strongly sorbed to labile
    !bss is the P from ss to occluded
    
    
    ! labile P dynamic, REF: Wang et al., 2010 D8
    
! 2024 01 25 
! this equation still considering the P_net2, since it is for the whole labile P dynamic 

    ! here the Dsoillab =(Dsoillab+Dsoilsorb)
    !Dsoillab = P_net2 + Pwea + Fptase + Pdep + P_fert  &
    !            -P_uptake  &    !-P_loss move to last process
    !            - asorb + bss ! kpss*Psoilocc, occluded P to labile P
                !xkpsorb*kpsorb*Psoilsorb, in casa, the amount of sorbed labileP  !why????
    ! Delete P_net2 for considering utilize the QPlab for plant uptake 
    ! after net mineralization 
    CALL Pleaching(runoff,rdepth,Scalar_P_flow)
    !Dsoillab =  Pwea + Fptase + Pdep + P_fert - P_uptake - asorb - P_loss + P_net2 !+ bss
    Dsoillab=(Pwea+Fptase+Pdep+P_fert+P_net2)-(P_uptake+asorb+P_loss)  !+ bss
    !IF(Dsoillab .NE. Dsoillab2)THEN
    !    write(*,*)'TEST',Dsoillab,Dsoillab2
    !    stop
    !ENDIF

    !IF (itime .LT. 8750 .and. itime .GT. 8745)THEN
    !    write(*,*)'Dsoillab,itime ---- ',Dsoillab,itime
    !    write(*,*)'in',Pwea,Fptase,Pdep,P_net2
    !    write(*,*)'out',P_uptake,asorb,P_loss
    !ENDIF

    xDsoillab = 1.0+ Psorbmax*kmlabp &
                    /((kmlabp+QPlab)**2)  !Spmax - Psorbmax, varied with soil layers
    !kmplab - an empirical parameter for describing the equilibrium between labile P and sorbed P (gPm-2)
    !Dsoilsorb  = xDsoillab
    Dsoillab = Dsoillab/xDsoillab !Wang et al., 2010 D8
    !write(*,*)'Pwea,Fptase,Pdep,P_fert,P_uptake,asorb,bss,P_net2',Pwea,Fptase,Pdep,P_fert,P_uptake,asorb,bss,P_net2

    Dsoilsorb  = 0.0  !follow the equipment, so this is zero
    !Dsoilss  = kpsorb * QPsorb - kpss * QPss !kpsob - the adsorb rate
    Dsoilss   = asorb - bss !- css2

!  wan 2024 01 25
!  ATTENTION : use the updated QPlab: QPlab-1 to update againe.  deactive this part
!   write(*,*)'1-QPlab, Dsoillab',QPlab,Dsoillab
    QPlab = QPlab+Dsoillab 
    !write(*,*)'2-QPlab, Dsoillab',QPlab,Dsoillab

    Fptase_rec(itime) = Fptase
    asorb_rec(itime) =asorb
    P_loss_rec(itime)=P_loss
    Dsoillab_rec(itime)=Dsoillab

    !IF(QPlab .GT. 0.5 .and. QPlab .LT. 0.55)THEN
    !    write(*,*)'P_inout',Fptase,Pwea,P_uptake ,(P_loss+asorb),Pdep
        !stop
    !ENDIF

!   avoid the deficit in QPlab and we give the priorit to immobilization
!   2024 01 25 QPlab-P_net2
    !IF(QPlab .lt. 0)THEN
    !    write(*,*)'here' 
    !        P_uptake = 1E-6
    !        LfactorP = 1E-6  
    !        Dsoillab = Pwea + Fptase + Pdep + P_fert -P_uptake - asorb -P_loss
    !        xDsoillab = 1.0+ Psorbmax*kmlabp &
    !            /((kmlabp+QPlab)**2) 
    !        Dsoillab = Dsoillab/xDsoillab
    !        QPlab = QPlab+Dsoillab 
    !ENDIF

    
    QPsorb = Psorbmax*QPlab/(kmlabP+QPlab)
    QPss = QPss+Dsoilss
    QPocc = QPocc+bss

    !write(*,*)'QPocc',QPocc,bss

    !truly occluded P dynamic
    !css2 = kss_2*QPss 
    !css2, from sencondary to occluded P
    !Dss_2     = css2
    !Qss_2 = Qss_2+ Dss_2

    !write(*,*)'QPss',QPss
    !write(*,*)'!--------Step1-5---------! Dsoillab,xDsoillab,Dsoilss,QPsorb',&
    !&Dsoillab,xDsoillab,Dsoilss,QPsorb
    !write(*,*)'QPlab',QPlab

    IF( QPlab .gt. 500) THEN
    write(*,*)'QPlab wrong, stop'
    stop
    ENDIF

END SUBROUTINE SorbedPDynamic

SUBROUTINE SorbedPDynamicNew()

    USE IntersVariables
    IMPLICIT NONE
    REAL  runoff,rdepth
    REAL  QPplant
    REAL  xDsoillab,Dsoilsorb,Dsoilss
    REAL  P_imm(5) !P_immob = sum(P_imm)
    REAL  P_imm2(3),P_imm22(3)
    !P biochemical mineralization
    REAL prodptase !Phosphate production, biome-specific in CABLE
    ! same as u_pmax, the maximun specific biochemical P mineralization rate(d-1)
    REAL costNpup,Pup0 !N cost of P uptake 40gN/P for tropical biomes
    ! and 25gN/g P for other biomes
    REAL  Psorbmax,kmlabP,xkplab,xkpsorb,xkpss !biome-specific,Parameters for Langmuir equation
    REAL  kplab,kpsorb,kpss   !rate
    REAL  Pdep,Pwea,P_fert!P input
    REAL  Scalar_P_flow,LDOP0
    REAL  P_biominer 
    REAL  xkoptsoil,xksoil
    REAL  kss_2, Qss_2, Dss_2,css2  !wan 2023/06/06
    !REAl  Dsoillab_rec2(8760)
    
    
    Dsoillab  = 0.0
    Dsoilsorb = 0.0
    Dsoilss  = 0.0
    P_biominer= 0.0
    xksoil = 1.0
    !xkoptsoil = 0.6 !CABLE
    
    Psorbmax  = 133 !gPm-2 maximum amount of sorbed P
    kmlabp    = 64  !gPm-2
    !xkoptsoil = 0.6 !CABLE

    xkoptsoil = 0.3 !wan
    !xkplab    = 0.5!xx  !never use 0.00137, CABLE
    xkpsorb   = 2.05e-5!CABLE 
    !xkpss    = 2.74e-5!CABLE 

    xkpss    = 0.24e-5!CABLE 

    !xkpsorb   = 6.05e-6!wan
    !xkpss    = 6.74e-6!wan
    
    xksoil = xkoptsoil * S_t(1) * S_omega   !wan:S_t(1)=S_t(2)=3=4=5
    !xksoil = 1.0   ! for temporary run the code, i need this to be 1 dimension !www
    !kplab   = xksoil * xkplab    !never use in both cable and teco
    kpsorb   = xksoil * xkpsorb
    kpss    = xksoil* xkpss
    
    asorb = kpsorb*QPsorb   !wan: new variables for output, k mean turnover rate of P, 
    !asorb, from labile and sorbed P to occ P -- sencondary P. occ means sencondary
    bss =  kpss*QPss     !wan 2023/3/17, add xkpss*
    !bss, from sencondary to labile and sorbed. occ is sencondary, not occluded
    
    
    ! labile P dynamic, REF: Wang et al., 2010 D8
    
    ! 2024 01 25 
    ! this equation still considering the P_net2, since it is for the whole labile P dynamic 
    
    ! here the Dsoillab =(Dsoillab+Dsoilsorb)
    Dsoillab = P_net2
    !xkpsorb*kpsorb*Psoilsorb, in casa, the amount of sorbed labileP  !why????

    Dsoillab_rec2 = Dsoillab
    
    xDsoillab = 1.0+ Psorbmax*kmlabp/((kmlabp+QPlab)**2)  !Spmax - Psorbmax, varied with soil layers
    !kmplab - an empirical parameter for describing the equilibrium between labile P and sorbed P (gPm-2)
    !Dsoilsorb  = xDsoillab
    Dsoillab = Dsoillab/xDsoillab !Wang et al., 2010 D8
    
    Dsoilsorb  = 0.0  !follow the equipment, so this is zero
    !Dsoilss  = kpsorb * QPsorb - kpss * QPss !kpsob - the adsorb rate
    
    !  wan 2024 01 25
    !  ATTENTION : use the updated QPlab: QPlab-1 to update againe.  deactive this part
    !write(*,*)'1-QPlab, Dsoillab',QPlab,Dsoillab
    QPlab = QPlab+Dsoillab 
    !write(*,*)'P_net2',P_net2,Dsoillab,P_immob2
    QPsorb = Psorbmax*QPlab/(kmlabP+QPlab)

        
    IF(QPlab .lt. 0) Flag_P = 1
    IF( QPlab .gt. 500) THEN
        write(*,*)'QPlab wrong, stop2'
        stop
    ENDIF
    
    END SUBROUTINE SorbedPDynamicNew

END MODULE P_specific_process
    
    