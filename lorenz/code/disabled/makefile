###COMPILE=f77
COMPILE=pgf77 

###COMLIB1= -xlic_lib=sunperf 
#COMLIB1=-lcomplib.sgimath
 COMLIB1=

#COMOTT1=-dalign -O3 -r8
###COMOTT1=-dalign -fast -r8
#COMOTT1=-Ofast -mips4 -IPA -r8
 COMOTT1=  -fast -O3 -r8
### COMOTT1=  -fast  

 COMCONV= -byteswapio 
    cp $COM_DIR/SFMT.f90 .
    cp $COM_DIR/common.f90 .
    cp $COM_DIR/netlib.f .
#    cat $COM_DIR/netlibblas.f >> ./netlib.f
    cp $COM_DIR/common_mtx.f90 .
    cp $COM_DIR/common_letkf.f90 .
    cp $COM_DAS_DIR/common_das_tools.f90 .
    cp $COM_DAS_DIR/common_dyn_system.f90 .
    cp $COM_DAS_DIR/common_trajectory.f90 .
    cp $COM_DAS_DIR/common_obs_operator.f90 .
    cp $COM_DAS_DIR/common_filter.f90 .
    cp $COM_DAS_DIR/common_ta_filter.f90 .
    cp $COM_DAS_DIR/io_tools.f90 .
    cp $COM_DAS_DIR/ensemble_tools.f90 .
    cp $MODEL_DIR/model_core.f90 .

FILES= \
       dyn_geop.o \
       dyn_grtend.o \
       dyn_implic.o \
       dyn_sptend.o \
       dyn_step.o \
       dyn_stloop.o \
       ini_impint.o \
       ini_indyns.o \
       ini_inforc.o \
       ini_iniall.o \
       ini_inphys.o \
       ini_inirdf.o \
       ini_invars.o \
       ini_stepone.o \
       phy_convmf.o \
       phy_fordate.o \
       phy_lscond.o \
       phy_phypar.o \
       phy_radiat.o \
       phy_shtorh.o \
       phy_suflux.o \
       phy_vdifsc.o \
       ppo_diagns.o \
       ppo_restart.o \
       ppo_setctl.o \
       ppo_setctl_daily.o \
       ppo_setgrd.o \
       ppo_tminc.o \
       ppo_tminc_daily.o \
       ppo_tmout.o \
       ppo_tmout_daily.o \
       sfc_anomod.o \
       spe_matinv.o \
       spe_spectral.o \
       spe_subfft_fftpack.o

.SUFFIXES: .f .o

.f.o:
	$(COMPILE) $(COMCONV) $(COMOTT1) -c $<

imp.exe: $(FILES)  at_gcm.o
	$(COMPILE) $(COMCONV) $(COMOTT1) $(FILES) at_gcm.o -o imp.exe $(COMLIB1) 


at_gcm.o               : at_gcm.f
dyn_geop.o             : dyn_geop.f
dyn_grtend.o           : dyn_grtend.f
dyn_implic.o           : dyn_implic.f
dyn_sptend.o           : dyn_sptend.f
dyn_step.o             : dyn_step.f
dyn_stloop.o           : dyn_stloop.f
ini_impint.o           : ini_impint.f
ini_indyns.o           : ini_indyns.f
ini_inforc.o           : ini_inforc.f
ini_iniall.o           : ini_iniall.f
ini_inphys.o           : ini_inphys.f
ini_inirdf.o           : ini_inirdf.f
ini_invars.o           : ini_invars.f
ini_stepone.o          : ini_stepone.f
phy_convmf.o           : phy_convmf.f
phy_lscond.o           : phy_lscond.f
phy_fordate.o          : phy_fordate.f
phy_phypar.o           : phy_phypar.f
phy_radiat.o           : phy_radiat.f
phy_shtorh.o           : phy_shtorh.f
phy_suflux.o           : phy_suflux.f
phy_vdifsc.o           : phy_vdifsc.f
ppo_diagns.o           : ppo_diagns.f
ppo_restart.o          : ppo_restart.f
ppo_setctl.o           : ppo_setctl.f
ppo_setctl_daily.o     : ppo_setctl_daily.f
ppo_setgrd.o           : ppo_setgrd.f
ppo_tminc.o            : ppo_tminc.f
ppo_tminc_daily.o      : ppo_tminc_daily.f
ppo_tmout.o            : ppo_tmout.f
ppo_tmout_daily.o      : ppo_tmout_daily.f
sfc_anomod.o           : sfc_anomod.f
spe_matinv.o           : spe_matinv.f
spe_spectral.o         : spe_spectral.f
spe_subfft_fftpack.o   : spe_subfft_fftpack.f 
