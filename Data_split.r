#Split the data in 2 years of calibration and 1 year of validation
Norunda_calibration<-Norunda_all_day[Norunda_all_day$Year==1999|Norunda_all_day$Year==2001,]
Norunda_validation<-Norunda_all_day[Norunda_all_day$Year==1997,]

Hyytiala_calibration<-Hyytiala_all_day[Hyytiala_all_day$Year==1999|Hyytiala_all_day$Year==2000,]
Hyytiala_validation<-Hyytiala_all_day[Hyytiala_all_day$Year==2001,]

