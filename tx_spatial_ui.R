library(sf)
library(readxl)
library(dplyr)
txgeo<-st_read("data/tx_county_classes.shp")
txgeo<-txgeo%>%
  arrange(NAME)


txui<- readxl::read_excel("data/weekly-claims-by-county-twc.xlsx")
txui<-txui[-1:-2, 1:31]
nams<-rep("week", 30);nums<-seq(1:30); nams<-paste(nams,sprintf("%02d", nums), sep="_")
names(txui)<-c("county", nams)
head(txui)
txui$county<-ifelse(txui$county=="De Witt", "DeWitt", txui$county)
txui<-txui%>%
  arrange(county)
txui$fips<-txgeo$GEOID

library(tidycensus)

txpop<-get_acs(geography="county", state = 48, year=2018, variables = "B01001_001", output = "wide")

library(tidyr)
library(tigris)
txlong<-txui%>%
  pivot_longer(cols=starts_with("week"),
               names_to="week")

txsp<-geo_join(txgeo, txpop[, c(1,3)], by_sp="GEOID", by_df="GEOID")
txsp<-geo_join(txsp, txlong, by_sp="GEOID", by_df="fips",how="inner" )

dates<-rev( seq.Date(from=as.Date("2020/03/07"), to=as.Date("2020/09/26"), by = "week"))

txsp<-txsp%>%
  arrange(NAME, week)

txsp$date<-rep(dates, 254)
library(ggplot2)

txsp$pcui<-100*(as.numeric(txsp$value)/txsp$B01001_001E)
txsp$pccut<- cut(txsp$pcui,breaks=quantile(txsp$pcui, p=seq(0,1,length.out = 9), na.rm=T), include.lowest = T)

p1<-txsp%>%
 # mutate(pcui=1000*)%>%
#  mutate(pccut = cut())
  #filter(week=="week_1")%>%
  ggplot()+geom_sf(aes(fill=pccut))+
  scale_fill_brewer(palette = "Blues")+
  facet_wrap(~date, nrow=7)+
  labs(caption = "Source: TWC Unemployment Insurance Claims \n Calculations by Corey S. Sparks, Ph.D.")+
  ggtitle(label ="Figure 6. Per capita Unemployment Insurance Claims", subtitle="March 7 to August 15, 2020")

p1<-p1+theme(plot.title = element_text(size=24), plot.subtitle = element_text(size=16))
#p1
ggsave(p1, filename = "images/tx_co_weeklyui.png", dpi = "print", width = 24, height = 24)


p2<-txsp%>%
  arrange(NAME,date)%>%
  filter(NAME=="Bexar")%>%
  ggplot()+geom_line(aes(x=date, y=pcui, color=NAME,group=NAME), size=1)+theme(legend.position = "none")+theme_minimal()+labs(title = "Figure 6. Per capita by Metropolitan Residence",subtitle = "March 7 to August 15, 2020",caption = "Source: TWC Unemployment Insurance Claims \n Calculations by Corey S. Sparks, Ph.D.")

library(plotly)
ggplotly(p2)


txsp%>%filter(NAME=="Hidalgo")%>%ggplot(aes(x=date,y=pcui ))+geom_line()
