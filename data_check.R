library(tidyverse)

####check Evapotranspiration####
# check if negative ET occurs during winter
neg_ET_winter <- function(month, daily_ET, counter) {
  rows <- length(daily_ET)
  for (i in 1:rows) {
    if (daily_ET[i] < 0 && month[rows] >= 4 && month[i] <= 9) {
      counter <- counter + 1
    } else { next }
  }
  return(counter)
}

neg_ET_not_winter_1999_day <- neg_ET_winter(H_1999_day$Month, H_1999_day$Evapotr,0)
neg_ET_not_winter_2000_day <- neg_ET_winter(H_2000_day$Month, H_2000_day$Evapotr,0)
neg_ET_not_winter_2001_day <- neg_ET_winter(H_2001_day$Month, H_2001_day$Evapotr,0)

####check precipitation####
#check negative and too high values
any(Hyytiala1999day$Prec<0|Hyytiala1999day$Prec>100)
any(Hyytiala2000day$Prec<0|Hyytiala2000day$Prec>100)
any(Hyytiala2001day$Prec<0|Hyytiala2001day$Prec>100)
