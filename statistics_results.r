#RMSE
sqrt(mean((swc$obs - swc$sum/1000)^2))
sqrt(mean((swc_Hyytiala_calibration$obs - swc_Hyytiala_calibration$sum/1000)^2))#0.057
sqrt(mean((swc_Hyytiala_validation$obs - swc_Hyytiala_validation$sum/1000)^2))#0.089
sqrt(mean((swc_Norunda_validation$obs - swc_Norunda_validation$sum/1000)^2, na.rm=T))#0.038
#linear fit
summary(lm(swc$sum/h~swc$obs))
summary(lm(swc_Hyytiala_calibration$sum/h~swc_Hyytiala_calibration$obs)) #0.72
summary(lm(swc_Hyytiala_validation$sum/h~swc_Hyytiala_validation$obs)) #0.73
summary(lm(swc_Norunda_validation$sum/h~swc_Norunda_validation$obs)) #0.72
