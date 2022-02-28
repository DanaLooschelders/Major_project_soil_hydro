#RMSE
sqrt(mean((swc$obs - swc$sum/h)^2))

#linear fit
summary(lm(swc$sum/h~swc$obs))
