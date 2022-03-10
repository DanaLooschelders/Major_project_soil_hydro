# define constants
max_swc_H = max(Hyytiala_validation$SWC20) # max swc in % for Hyytiala
min_swc_H =min(Hyytiala_validation$SWC20)
#model validation without interception
swc_Hyytiala_validation<-calc_swc_M(Precip=Hyytiala_validation$Prec, ET=Hyytiala_validation$Evapotr,
                                    max_swc=max_swc_H*200, k_winter=0.02, k_summer=0.02, 
                                    min_swc=min_swc_H*200, T_u=5, 
                                    T_lf=-4, T_lm=-5, k_m=0.5, T=Hyytiala_validation$AirT, 
                                    init_snowsize=0,
                                    ET_summer=0, ET_winter=0, month=Hyytiala_validation$Month)

#add column with date
swc_Hyytiala_validation$date<-Hyytiala_validation$date
swc_Hyytiala_validation$date<-as.POSIXct(swc_Hyytiala_validation$date)
swc_Hyytiala_validation$obs<-Hyytiala_validation$SWC20 #add observations to sim data

# model validation with interception
swc_Hyytiala_validation_interc<-calc_swc_M(Precip=Hyytiala_validation$Prec+0.5, ET=Hyytiala_validation$Evapotr,
                                     max_swc=max_swc_H*200, k_winter=0.02, k_summer=0.02, 
                                     min_swc=min_swc_H*200, T_u=5, 
                                     T_lf=-4, T_lm=-5, k_m=0.5, T=Hyytiala_validation$AirT, 
                                    init_snowsize=0,
                                     ET_summer=0, ET_winter=0, month=Hyytiala_validation$Month)
unique(Hyytiala_validation$Month)
#add column with date
swc_Hyytiala_validation_interc$date<-Hyytiala_validation$date
swc_Hyytiala_validation_interc$date<-as.POSIXct(swc_Hyytiala_validation$date)
swc_Hyytiala_validation_interc$obs<-Hyytiala_validation$SWC20 #add observations to sim data

plot(swc_Hyytiala_validation$snowaccumulation, type="l")

####for Norunda####
# define constants
max_swc_N = max(Norunda_validation$SWC20, na.rm=T) # max swc in % for Hyytiala
min_swc_N =min(Norunda_validation$SWC20, na.rm=T)

# model validation
swc_Norunda_validation<-calc_swc_M(Precip=Norunda_validation$Prec, ET=Norunda_validation$Evapotr,
                                    max_swc=max_swc_N*200, k_winter=0.02, k_summer=0.02, 
                                    min_swc=min_swc_N*200, T_u=5, 
                                    T_lf=-4, T_lm=-5, k_m=0.5, T=Norunda_validation$AirT, 
                                    init_snowsize=0,
                                    ET_summer=0, ET_winter=0, month=Norunda_validation$Month)
#add column with date
swc_Norunda_validation$date<-Norunda_validation$date
swc_Norunda_validation$date<-as.POSIXct(swc_Norunda_validation$date)
swc_Norunda_validation$obs<-Norunda_validation$SWC20 #add observations to sim data
