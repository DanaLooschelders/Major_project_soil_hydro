#RMSE
sqrt(mean((swc$obs - swc$sum/1000)^2))

#linear fit
summary(lm(swc$sum/h~swc$obs))
