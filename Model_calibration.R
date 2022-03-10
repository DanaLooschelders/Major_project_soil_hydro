# define constants
max_swc_H = max(Hyytiala_calibration$SWC20) # max swc in % for Hyytiala
min_swc_H =min(Hyytiala_calibration$SWC20)

# model calibration
swc_Hyytiala_calibration<-calc_swc_M(Precip=Hyytiala_calibration$Prec, ET=Hyytiala_calibration$Evapotr,
                max_swc=max_swc_H*200, k_winter=0.003, k_summer=0.02, 
                min_swc=min_swc_H*200, T_u=4, 
                T_lf=-7, T_lm=-5, k_m=0.5, T=Hyytiala_calibration$AirT, init_snowsize=0,
                ET_summer=0, ET_winter=0, month=Hyytiala_calibration$Month)

#add column with date
swc_Hyytiala_calibration$date<-Hyytiala_calibration$date
swc_Hyytiala_calibration$date<-as.POSIXct(swc_Hyytiala_calibration$date)
swc_Hyytiala_calibration$obs<-Hyytiala_calibration$SWC20 #add observations to sim data

