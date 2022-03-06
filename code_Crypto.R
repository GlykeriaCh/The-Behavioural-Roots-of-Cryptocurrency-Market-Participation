rm(list=ls(all=TRUE))

library(readstata13)
library(haven)
library(stargazer)

data<-read.csv("~/Desktop/behavioural data/ourdataset.csv")
data<-data[-1]

data1<-read.csv("~/Desktop/behavioural data/ISO3.csv")
data<-merge(data,data1,by="Country")

data2<-read_dta("~/Desktop/behavioural data/folklore.dta")
data<-merge(data,data2,by="ISO3")

data3<-read.csv("~/Desktop/behavioural data/Inflation.csv")
data<-merge(data,data3,by="Country")

data4<-read.csv("~/Desktop/Innovation.csv")
data<-merge(data,data4,by="Country")

data5<-read.csv("~/Desktop/Latitude.csv")
data<-merge(data,data5,by="ISO3")


# 1st Model-Society Variables and Crypto

model1<-lm(Crypto~lngdppc_cu_usd_1960_2017_avg_wdi+totalScore,
           data=data)
summary(model1)

model2<-lm(Crypto~lngdppc_cu_usd_1960_2017_avg_wdi+totalScore+savyr2005,
           data=data)
summary(model2)

model3<-lm(Crypto~lngdppc_cu_usd_1960_2017_avg_wdi+totalScore+savyr2005+
             giniCoefficient,
           data=data)
summary(model3)

model4<-lm(Crypto~lngdppc_cu_usd_1960_2017_avg_wdi+totalScore+savyr2005+
             giniCoefficient+Freedom,
           data=data)
summary(model4)

model5<-lm(Crypto~lngdppc_cu_usd_1960_2017_avg_wdi+totalScore+savyr2005+
             giniCoefficient+Freedom+SCI,
           data=data)
summary(model5)

model6<-lm(Crypto~lngdppc_cu_usd_1960_2017_avg_wdi+totalScore+savyr2005+
             giniCoefficient+Freedom+SCI+latitude,
           data=data)
summary(model6)


model7<-lm(Crypto~lngdppc_cu_usd_1960_2017_avg_wdi+totalScore+savyr2005+
             giniCoefficient+Freedom+SCI+latitude+pdiv_aa,
           data=data)
summary(model7)

model8<-lm(Crypto~lngdppc_cu_usd_1960_2017_avg_wdi+totalScore+savyr2005+
             giniCoefficient+Freedom+SCI+latitude+pdiv_aa+ I(pdiv_aa^2),
           data=data)
summary(model8)Τ


stargazer(model1, model2, model3,model4,model5,model6,model7,model8, title="Socioeconomic Factors and Genetic Diversity", align=TRUE, dep.var.labels=("Crypto Adoption"), 
          covariate.labels=c("GDP","Education", "Savings","GiniIndex","Economic Freedom","Social Cohesion","Latitude","Diversity(a.a.)","Diversity sqrt(a.a.)"),
          omit.stat=c("LL","ser","f"), no.space=TRUE, type = "html",out = "regression.html")



# 2nd Model-Behavioural Variables and Crypto

model1<-lm(Crypto~lngdppc_cu_usd_1960_2017_avg_wdi+totalScore+savyr2005+
             giniCoefficient+Freedom+SCI+latitude+pdiv_aa+ I(pdiv_aa^2),
           data=data)
summary(model1)

model2<-lm(Crypto~lngdppc_cu_usd_1960_2017_avg_wdi+totalScore+savyr2005+
             giniCoefficient+Freedom+SCI+latitude+pdiv_aa+ I(pdiv_aa^2)+Trust,
           data=data)
summary(model2)



model3<-lm(Crypto~lngdppc_cu_usd_1960_2017_avg_wdi+totalScore+savyr2005+
             giniCoefficient+Freedom+SCI+latitude+pdiv_aa+ I(pdiv_aa^2)+patience,
           data=data)
summary(model3)


model4<-lm(Crypto~lngdppc_cu_usd_1960_2017_avg_wdi+totalScore+savyr2005+
             giniCoefficient+Freedom+SCI+latitude+pdiv_aa+ I(pdiv_aa^2)+pdi,
           data=data)
summary(model4)


model5<-lm(Crypto~lngdppc_cu_usd_1960_2017_avg_wdi+totalScore+savyr2005+
             giniCoefficient+Freedom+SCI+latitude+pdiv_aa+ I(pdiv_aa^2)+idv,
           data=data)
summary(model5)

model6<-lm(Crypto~lngdppc_cu_usd_1960_2017_avg_wdi+totalScore+savyr2005+
             giniCoefficient+Freedom+SCI+latitude+pdiv_aa+ I(pdiv_aa^2)+mas,
           data=data)
summary(model6)

model7<-lm(Crypto~lngdppc_cu_usd_1960_2017_avg_wdi+totalScore+savyr2005+
             giniCoefficient+Freedom+SCI+latitude+pdiv_aa+ I(pdiv_aa^2)+uai,
           data=data)
summary(model7)

model8<-lm(Crypto~lngdppc_cu_usd_1960_2017_avg_wdi+totalScore+savyr2005+
             giniCoefficient+Freedom+SCI+latitude+pdiv_aa+ I(pdiv_aa^2)+ltowvs,
           data=data)
summary(model8)

model9<-lm(Crypto~lngdppc_cu_usd_1960_2017_avg_wdi+totalScore+savyr2005+
             giniCoefficient+Freedom+SCI+latitude+pdiv_aa+ I(pdiv_aa^2)+risktaking,
           data=data)
summary(model9)

model10<-lm(Crypto~lngdppc_cu_usd_1960_2017_avg_wdi+totalScore+savyr2005+
              giniCoefficient+Freedom+SCI+latitude+pdiv_aa+ I(pdiv_aa^2)+Altruism,
            data=data)
summary(model10)


stargazer(model1, model2, model3,model4,model5,model6,model7,model8,model9,model10, title="Behavioral Factors", 
          align=TRUE, dep.var.labels=("Crypto Adoption"), 
          covariate.labels=c("GDP","Education", "Savings","GiniIndex",
                             "Economic Freedom","Social Cohesion","Latitude","Diversity(a.a.)",
                             "Diversity sqrt(a.a.)","Trust","Patience","Power Distance","Individualism",
                             "Masculinity","Uncertainty Avoidance","Long-term Orientation",
                             "Risktaking","Altruism"),
          omit.stat=c("LL","ser","f"), no.space=TRUE, type = "html",out = "regression.html")


# 3rd Model-Folklore and Crypto


model1<-lm(Crypto~lngdppc_cu_usd_1960_2017_avg_wdi+totalScore+savyr2005+
             giniCoefficient+Freedom+SCI+latitude,
           data=data)
summary(model1)


model2<-lm(Crypto~lngdppc_cu_usd_1960_2017_avg_wdi+totalScore+savyr2005+
             giniCoefficient++Freedom+SCI+latitude+challenge_suc,
           data=data)
summary(model2)

model3<-lm(Crypto~lngdppc_cu_usd_1960_2017_avg_wdi+totalScore+savyr2005+
             giniCoefficient+Freedom+SCI+latitude+anti_success,
           data=data)
summary(model3)


model4<-lm(Crypto~lngdppc_cu_usd_1960_2017_avg_wdi+totalScore+savyr2005+
             giniCoefficient+Freedom+SCI+latitude+anti_notsuccess,
           data=data)
summary(model4)

model5<-lm(Crypto~lngdppc_cu_usd_1960_2017_avg_wdi+totalScore+savyr2005+
             giniCoefficient+Freedom+SCI+latitude+tricksters_punish,
           data=data)
summary(model5)

model6<-lm(Crypto~lngdppc_cu_usd_1960_2017_avg_wdi+totalScore+savyr2005+
             giniCoefficient+Freedom+SCI+latitude+challenge_competition,
           data=data)
summary(model6)


stargazer(model1, model2, model3,model4,model5,model6, title="Folklore",
          align=TRUE, dep.var.labels=("Crypto Adoption"), 
          covariate.labels=c("GDP","Education", "Savings","GiniIndex","Economic Freedom",
                             "Social Cohesion","Latitude","Challenge succesful","Antisocial not punished","Antisocial punished",
                             "Tricksters punished",
                             "Challenge Competition"),
          omit.stat=c("LL","ser","f"), no.space=TRUE, type = "html",out = "regression.html")

# Maps
library(rgdal)
world_spdf <- readOGR(dsn ="~/Desktop/DATA-5/TM_WORLD_BORDERS-0.3.shp")
library(leaflet)
library(RColorBrewer)

map_and_data <- merge(world_spdf, data, by='ISO3')
mypalette <- colorBin( palette="Blues", domain=map_and_data@data$Crypto, na.color="transparent")

# Prepare the text for tooltips:
mytext <- paste(
  "Country: ", map_and_data@data$NAME,"<br/>", 
  "Area: ", map_and_data@data$area, "<br/>", 
  "Crypto: ", round(map_and_data@data$Crypto, 2), 
  sep=""
) %>%
  lapply(htmltools::HTML)

# Final Map
leaflet(map_and_data) %>% 
  addTiles()  %>% 
  setView( lat=10, lng=0 , zoom=2) %>%
  addPolygons( 
    fillColor = ~mypalette(Crypto), 
    stroke=TRUE, 
    fillOpacity = 1.5, 
    color="white", 
    weight=0.3,
    label = mytext,
    labelOptions = labelOptions( 
      style = list("font-weight" = "normal", padding = "3px 8px"), 
      textsize = "13px", 
      direction = "auto"
    )
  ) %>%
  addLegend( pal=mypalette, values=~Crypto, opacity=0.9, title = "Cryptocurrency Adoption Index ", position = "bottomleft" )



