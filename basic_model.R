#basic model from Dana
# groundwater recharge with soil water 𝜃 as state variable
# ∆𝜃 =𝑃−𝐸−𝑄−𝑅
# Q = 𝜃k(𝜃/𝜃max)^2
calc_swc<-function(P, ET, max_swc, k) {
  swc<-data.frame("change"=rep(NA, length(P)), 
                  "sum"=rep(NA, length(P)),
                  "runoff"=rep(NA, length(P))) #output dataframe for the changing soil water
  swc$sum[1]<- P[1] - ET [1] #initial soil water content
  swc$change[1]<-0 #initial change is zero
  for(i in 2:length(swc$sum)){ #soil water content from day 2
    temp_Q<-swc$sum[i-1]*k*(swc$sum[i-1]/max_swc)^2 #calculate value for Q
    if(max_swc > P[i] - temp_Q - ET[i]) # water available for swc is less than max_swc
    {
     swc$change[i]<- P[i] - temp_Q - ET[i] #calculate change in soil water
     swc$sum[i]<-swc$sum[i-1]+swc$change[i] #add change to sum
     if(swc$sum[i]<0){ #if the sums turns negativ
       swc$sum[i]=0 #set sum to zero
     }else if(swc$sum[i]>max_swc){ #if the sum exeeds max_swc 
       swc$sum[i]=max_swc #set sum to swc max
     } else{}#if sum positiv and less than swc max, do nothing
    }else{ #if water available for swc is more than or equal to max swc
      #calculate change until max_swc is reached
      swc$change[i]<-max_swc-swc$sum[i-1] #calculate the maximum possible change 
      swc$sum[i]<-max_swc #sum is then max_swc
      theoretical_change <- P[i] - temp_Q - ET[i] #calculate the theoretical change
      excess_water<-theoretical_change-swc$change[i] #calculate the excess water
      swc$runoff<-P[i] - temp_Q - ET[i] - max_swc + excess_water #excess water goes into runoff
    }
  }
return(swc)}

#test model
swc<-calc_swc(P=Hyytiala1999day$Prec, ET=Hyytiala1999day$Evapotr, 
               max_swc=33.5, k=0.5)
#plot results
plot(swc$change, type="l") 
plot(swc$sum, type="l")
abline(h = 33.5, col="red")
