library(tigris)
options(tigris_class = "sf")
options(tigris_use_cache=TRUE)
txco<-counties(state="48", cb=T)
txco$fips<-as.character(txco$GEOID)
txgrp<-read.csv("tx_county_groups.csv", header=T)
txgrp$fips<-as.character(txgrp$fips)
mdat<-geo_join(txco, txgrp,"fips", "fips" )

library(mapview)
plot(mdat["region"])

# puma<-pumas(state="48", cb=T, year=2018)
# library(sf)
# out<-st_join(mdat,puma)
# plot(out["region"])


load(url("https://github.com/coreysparks/data/blob/master/arf2018.Rdata?raw=true"))
library(dplyr)
arf<-arf2018%>%
  select( f00002, f0002013)%>%
  mutate(cofips=f00002, rucc = f0002013)%>%
  mutate(rucc_c = case_when(as.numeric(rucc)>=8 ~ "Rural", 
                            as.numeric(rucc)>=4 & as.numeric(rucc)<=7 ~ "Nonmet_Urban",
                            as.numeric(rucc)<=3 ~ "Large Metro"))


out2<-geo_join(mdat, arf, "fips", "cofips")
st_write(out2, "/home/corey/Dropbox/COVID_TXMentalHealthSurvey/Datasets/tx_county_classes.shp", driver="ESRI Shapefile" )

plot(out2["rucc_c"], main="Rural Urban Continuum class")
plot(out2["phregion"], main="public health region")
plot(out2["hsregion"], main="health service region")
plot(out2["region"], main="economic region - comptroller")
