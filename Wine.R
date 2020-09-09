## LOAD DATA
library(purrr)
library(ggplot2)
Beer <- read.csv("~/Desktop/8260_1.csv")
# view column names
names(Beer)
summary(Beer)
View(summary(Beer))
#Paso Robles and Napa Valley are zip codes with most alcohol businesses 

## Looking at climate trends for wine regions for planning
Climate <- read.csv("~/Desktop/2274247.csv")
names(Climate)
#Grouping stations by locations
groups <- split(Climate, Climate$NAME)
Paso <- rbind(groups$`PASO ROBLES MUNICIPAL AIRPORT, CA US`,
              groups$`PASO ROBLES, CA US`,
              groups$`LAS TABLAS CALIFORNIA, CA US`)
Paso$AREA <- c('Paso')
Napa <- rbind(groups$`BERRYESSA LAKE, CA US`,
              groups$`LAKE BERRYESSA, CA US`,
              groups$`NAPA CO AIRPORT, CA US`,
              groups$`OAKVILLE 1 W, CA US`,
              groups$`OAKVILLE 4 SW N0 2, CA US`,
              groups$`MONTICELLO DAM, CA US`,
              groups$`MARKLEY COVE, CA US`,
              groups$`YOUNTVILLE, CA US`)
Napa$AREA <- c('Napa')

PN <- rbind(Paso, Napa)
groups <- split(PN, PN$AREA)
View(PNB)

## Exploratory data analysis by year and climate variables

Explore <-ggplot(PN)  +
  geom_boxplot(aes(x="TAVG", y=TAVG, fill=AREA)) + 
  geom_boxplot(aes(x="PRCP",y=PRCP, fill=AREA)) + 
  geom_boxplot(aes(x="SNOW", y=SNOW, fill=AREA))+
 labs(x="Indices", y="Value") + theme_bw() + theme(axis.line = element_line(colour = "black"))
Explore

## What is the best predictor of precipitation?
temp <- ggplot(PN,aes(y = PRCP, x = TAVG, colour = AREA,shape = AREA))+
  geom_point() + geom_smooth(method = "lm", se=TRUE, fullrange=TRUE)+
  xlab("Temperature")+
  ylab("Precipitation") + theme_bw()+
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"))+  theme(axis.title=element_text(family = "Times", face = "bold", size = 12)) +
  theme(axis.text = element_text(family = "Times", face="bold",size = 11))

Maxtemp <- ggplot(PN,aes(y = PRCP, x =TMAX , colour = AREA,shape = AREA))+
  geom_point() + geom_smooth(method = "lm", se=TRUE, fullrange=TRUE)+
  xlab("Max temp")+
  ylab("Precipitation") + theme_bw()+
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"))+  theme(axis.title=element_text(family = "Times", face = "bold", size = 12)) +
  theme(axis.text = element_text(family = "Times", face="bold",size = 11))

Maxtemp
Mintemp <- ggplot(PN,aes(y = PRCP, x =TMIN , colour = AREA,shape = AREA))+
  geom_point() + geom_smooth(method = "lm", se=TRUE, fullrange=TRUE)+
  xlab("Min temp")+
  ylab("Precipitation") + theme_bw()+
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"))+  theme(axis.title=element_text(family = "Times", face = "bold", size = 12)) +
  theme(axis.text = element_text(family = "Times", face="bold",size = 11))
Mintemp
## assessing the goodness of fit of models for each climatic variable
r2 = function(data) {
  fit = lm(TMAX ~ PRCP, data = PN)
  
  broom::glance(fit)
}
map_dfr(groups, r2, .id = "AREA")
# Minimum temperature is the best predictor of precipitation

