suppressMessages({
  library(shiny)
  library(shinyBS)
  library(shinyjs)
  library(leaflet)
  library(DT)
  library(rgdal)
  library(raster)
  library(data.table)
  library(plyr)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(purrr)
  library(lubridate)
})

# Yes this bring in teh shapefile twice but need it in global.R for bug model to recognize it, 
#  dont want to write separate reactive function in server.R to output gis comments
eco69_wgs84 <- readOGR('data/','Ecoregion69_level3_WGS84')
#eco69_wgs84 <- readOGR('C:/HardDriveBackup/R/BCG/Eco69_BCG/data','Ecoregion69_level3_WGS84')


#----------------------------------Taxa metric functions---------------------------------------------------------
# Number of Total Taxa
totTax <- function(sample){nrow(unique(sample))}
# Number of Total Individuals (for later calculations)
totInd <- function(sample){sum(sample$Count)}
# Number of Attribute I&II Taxa
T_12 <- function(sample){sum(sample$attLevel %in% c(1,2))}
# % Attribute 1,2,&3 Taxa
P_123 <- function(sample){(sum(sample$attLevel %in% c(1,2,3))/totTax(sample))*100}
# % Attribute 1,2,&3 Individuals
PI_123 <- function(sample){
  df <- filter(sample,attLevel %in% c(1,2,3))
  (sum(df$Count)/totInd(sample))*100}
# % Atribute 5 Individuals
PI_5 <- function(sample){
  df <- filter(sample,attLevel %in% c(5))
  (sum(df$Count)/totInd(sample))*100}
# % Atribute 5&6t Individuals
PI_56t <- function(sample){
  df <- filter(sample,attLevel %in% c('5','6t'))
  (sum(df$Count)/totInd(sample))*100}
# Number of Attribute 6 Taxa
T_6 <- function(sample){sum(sample$attLevel %in% c('6i','6m','6t'))}
# Number of Attribute 6t Taxa
T_6t <- function(sample){sum(sample$attLevel %in% c('6t'))}
# Number of Darter Taxa
T_darter <- function(sample){sum(filter(sample, grepl('darter',CommonName))$Count)}
# Number of native Brook Trout individuals
N_bTrout <- function(sample){sum(filter(sample, grepl('brook trout',CommonName))$Count)}
# % Most Dominant Attribute 5&6t individuals
P_Dom56t <- function(sample){(
  ifelse(nrow(filter(sample,attLevel %in% c('5','6t')))==0,0,max(filter(sample,attLevel %in% c('5','6t'))$Count))/totInd(sample))*100}
# Number of Attribute 1,2,&3 taxa
T_123 <- function(sample){sum(sample$attLevel %in% c(1,2,3))}
# % Attribute 5&6 taxa
P_56 <- function(sample){(sum(sample$attLevel %in% c('5','6i','6m','6t'))/totTax(sample))*100}

# Bug-specific functions
# Percent dominant (for later calculations)
P_dom <- function(sample){sample$Count/totInd(sample)}
# Number of Attribute 2 Taxa
T_2 <- function(sample){sum(sample$attLevel %in% c(2))}
# Number of Attribute 2 & 3 Taxa
T_23 <- function(sample){sum(sample$attLevel %in% c(2,3))}
# Number of Attribute 2 & 3 EPT Taxa
T_23EPT <- function(sample){
  df <- filter(sample,P_C_O %in% c("Ephemeroptera","Plecoptera","Trichoptera"))
  sum(df$attLevel %in% c(2,3))}
# Top 5% most dominant Attribute 4,5,6 individuals
P5_domAtt456 <- function(sample){
  df <- mutate(sample,dom=(Count/totInd(sample))*100)%>%filter(attLevel %in% c(4,5,6))%>%arrange(-dom)
  sum(df$dom[1:5])}
# Percent Attribute 2&3 individuals
PI_23 <- function(sample){
  df <- filter(sample,attLevel %in% c(2,3))
  (sum(df$Count)/totInd(sample))*100}
# Percent Attribute 2 & 3 EPT Individuals
PI_23EPT <- function(sample){
  df <- filter(sample,P_C_O %in% c("Ephemeroptera","Plecoptera","Trichoptera") & attLevel %in% c(2,3))
  (sum(df$Count)/totInd(sample))*100}
# Percent most dominant Attribute 4 OR 5 individuals
PI1_domAtt45 <- function(sample){
  df <- mutate(sample,dom=(Count/totInd(sample))*100)%>%filter(attLevel %in% c(4,5))%>%arrange(-dom)
  df$dom[1]}
# Number of Attribute 2,3, & 4 Taxa
T_234 <- function(sample){sum(sample$attLevel %in% c(2,3,4))}
# Number of EPT Taxa
T_EPT <- function(sample){
  df <- filter(sample,P_C_O %in% c("Ephemeroptera","Plecoptera","Trichoptera"))
  nrow(df)}
# % Attribute 5 Taxa
P_5 <- function(sample){(sum(sample$attLevel %in% c(5))/totTax(sample))*100}
# % Attribute 5 and midge individuals
PI_5midge <- function(sample){
  df <- filter(sample,attLevel %in% c(5)|FinalID=='Chironomidae')
  (sum(df$Count)/totInd(sample))*100}
# Percent EPT individuals
PI_EPT <- function(sample){
  df <- filter(sample,P_C_O %in% c("Ephemeroptera","Plecoptera","Trichoptera"))
  (sum(df$Count)/totInd(sample))*100}
# Percent Diptera individuals
PI_Diptera <- function(sample){
  df <- filter(sample,P_C_O == "Diptera")
  (sum(df$Count)/totInd(sample))*100}

## ---------------------------------------------------------------------------------------------------------
# Master metric that outputs dataframe of all results for fish
masterMetric <- function(sampleName,sample){
  data.frame(SampleName=sampleName,totTax=totTax(sample),totInd=totInd(sample),T_12=T_12(sample),P_123=P_123(sample)
             ,PI_123=PI_123(sample),PI_5=PI_5(sample),PI_56t=PI_56t(sample),T_6=T_6(sample)
             ,T_6t=T_6t(sample),T_darter=T_darter(sample),N_bTrout=N_bTrout(sample),P_Dom56t=P_Dom56t(sample)
             ,T_123=T_123(sample),P_56=P_56(sample))}

# Master metric that outputs dataframe of all results for bugs
masterMetric_bug <- function(sampleName,sample){
  data.frame(SampleName=sampleName,totTax=totTax(sample),totInd=totInd(sample),T_2=T_2(sample),T_23=T_23(sample),
             T_23EPT=T_23EPT(sample),P5_domAtt456=P5_domAtt456(sample),PI_23=PI_23(sample),PI_23EPT=PI_23EPT(sample),
             PI_5=PI_5(sample),PI1_domAtt45=PI1_domAtt45(sample),T_234=T_234(sample),T_EPT=T_EPT(sample),
             P_5=P_5(sample),PI_5midge=PI_5midge(sample),PI_EPT=PI_EPT(sample),PI_Diptera=PI_Diptera(sample))}

# BCG Fuzzy Membership logic Fish
fuzzyMembership <- function(metric,low,high){metric/(high-low)-low/(high-low)}

# BCG Fuzzy Membership logic Bug
fuzzyMembership_bug <- function(metric,low,high){(metric-low)/(high-low)}

# Final fuzzy membership calc Fish
fmFinal <- function(x){
  if(x>=1){return(1)}else(y=x)
  if(y<=0){return(0)}else(return(x))}

# Final fuzzy membership calc Bug
fmFinal_bug <- function(x){min(1,max(0,x))}

## ---------------------------- Level Rules FISH -----------------------------------------------------------------
# BCG level logic, Other Medium/Large
otherMedLarge_BCGlevel2 <- function(test){
  data.frame(fm_totTax=fmFinal(fuzzyMembership(test$totTax,10,20)),fm_T_12=fmFinal(fuzzyMembership(test$T_12,0,1))
             ,fm_P_123=fmFinal(fuzzyMembership(test$P_123,5,15)),fm_PI_123=fmFinal(fuzzyMembership(test$PI_123,15,25))
             ,fm_PI_56t=fmFinal(1-fuzzyMembership(test$PI_56t,55,65)),fm_T_6=fmFinal(1-fuzzyMembership(test$T_6,1,3)))}
otherMedLarge_BCGlevel3 <- function(test){
  data.frame(fm_totTax=fmFinal(fuzzyMembership(test$totTax,9,19)),fm_P_123=fmFinal(fuzzyMembership(test$P_123,4,10))
             ,fm_PI_123=fmFinal(fuzzyMembership(test$PI_123,3,7)),fm_PI_56t=fmFinal(1-fuzzyMembership(test$PI_56t,65,75))
             ,fm_P_Dom56t=fmFinal(1-fuzzyMembership(test$P_Dom56t,25,35)),fm_T_6=fmFinal(1-fuzzyMembership(test$T_6,1,5)))}
otherMedLarge_BCGlevel4alt1 <- function(test){
  data.frame(fm_totTax=fmFinal(fuzzyMembership(test$totTax,7,15)),fm_T_123=fmFinal(fuzzyMembership(test$T_123,0,1))
             ,fm_PI_56t=fmFinal(1-fuzzyMembership(test$PI_56t,75,85)))}
otherMedLarge_BCGlevel4alt2 <- function(test){
  data.frame(fm_totTax=fmFinal(fuzzyMembership(test$totTax,16,26)),fm_P_Dom56t=fmFinal(1-fuzzyMembership(test$P_Dom56t,25,35))
             ,fm_T_darter=fmFinal(fuzzyMembership(test$T_darter,2,5)))}
otherMedLarge_BCGlevel5 <- function(test){data.frame(fm_totTax=fmFinal(fuzzyMembership(test$totTax,3,6)))}

# BCG level logic, Other Medium/Large, NO BROOK TROUT
otherMedLarge_BCGlevel2_NOBKT <- function(test){
  data.frame(fm_totTax=fmFinal(fuzzyMembership(test$totTax,12,22)),fm_T_12=fmFinal(fuzzyMembership(test$T_12,0,1))
             ,fm_P_123=fmFinal(fuzzyMembership(test$P_123,5,15)),fm_PI_123=fmFinal(fuzzyMembership(test$PI_123,20,30))
             ,fm_PI_56t=fmFinal(1-fuzzyMembership(test$PI_56t,50,60)),fm_T_6=fmFinal(1-fuzzyMembership(test$T_6,0,2)))}
otherMedLarge_BCGlevel3_NOBKT <- function(test){
  data.frame(fm_totTax=fmFinal(fuzzyMembership(test$totTax,9,19)),fm_P_123=fmFinal(fuzzyMembership(test$P_123,4,10))
             ,fm_PI_123=fmFinal(fuzzyMembership(test$PI_123,3,7)),fm_PI_56t=fmFinal(1-fuzzyMembership(test$PI_56t,60,70))
             ,fm_P_Dom56t=fmFinal(1-fuzzyMembership(test$P_Dom56t,25,35)),fm_T_6=fmFinal(1-fuzzyMembership(test$T_6,1,5)))}
otherMedLarge_BCGlevel4alt1_NOBKT <- function(test){
  data.frame(fm_totTax=fmFinal(fuzzyMembership(test$totTax,6,14)),fm_T_123=fmFinal(fuzzyMembership(test$T_123,0,1))
             ,fm_PI_56t=fmFinal(1-fuzzyMembership(test$PI_56t,75,85)))}
otherMedLarge_BCGlevel4alt2_NOBKT <- function(test){
  data.frame(fm_totTax=fmFinal(fuzzyMembership(test$totTax,15,25)),fm_P_Dom56t=fmFinal(1-fuzzyMembership(test$P_Dom56t,25,35))
             ,fm_T_darter=fmFinal(fuzzyMembership(test$T_darter,2,5)))}
otherMedLarge_BCGlevel5alt1_NOBKT <- function(test){
  data.frame(fm_totTax=fmFinal(fuzzyMembership(test$totTax,4,9)),fm_totInd=fmFinal(fuzzyMembership(test$totInd,50,60)))}
otherMedLarge_BCGlevel5alt2_NOBKT <- function(test){
  data.frame(fm_totInd=fmFinal(fuzzyMembership(test$totInd,50,60)),fm_T_123=fmFinal(fuzzyMembership(test$T_123,0,1)))}

# BCG level logic, Other Small
otherSmall_BCGlevel2alt1 <- function(test){
  data.frame(fm_totTax=fmFinal(fuzzyMembership(test$totTax,6,16)),fm_T_12=fmFinal(fuzzyMembership(test$T_12,0,1))
             ,fm_P_123=fmFinal(fuzzyMembership(test$P_123,10,20)),fm_PI_123=fmFinal(fuzzyMembership(test$PI_123,15,25))
             ,fm_PI_56t=fmFinal(1-fuzzyMembership(test$PI_56t,55,65)),fm_T_6=fmFinal(1-fuzzyMembership(test$T_6,1,3))
             ,fm_T_darter=fmFinal(fuzzyMembership(test$T_darter,2,5)))}
otherSmall_BCGlevel2alt2 <- function(test){
  data.frame(fm_T_12=fmFinal(fuzzyMembership(test$T_12,0,1)),fm_P_123=fmFinal(fuzzyMembership(test$P_123,10,20))
             ,fm_PI_123=fmFinal(fuzzyMembership(test$PI_123,15,25)),fm_T_6=fmFinal(1-fuzzyMembership(test$T_6,1,3))
             ,fm_T_darter=fmFinal(fuzzyMembership(test$T_darter,2,5)),fm_N_bTrout=fmFinal(fuzzyMembership(test$N_bTrout,0,3)))}
otherSmall_BCGlevel3alt1 <- function(test){
  data.frame(fm_totTax=fmFinal(fuzzyMembership(test$totTax,5,13)),fm_P_123=fmFinal(fuzzyMembership(test$P_123,4,10))
             ,fm_PI_123=fmFinal(fuzzyMembership(test$PI_123,5,15)),fm_PI_56t=fmFinal(1-fuzzyMembership(test$PI_56t,65,75))
             ,fm_T_6=fmFinal(1-fuzzyMembership(test$T_6,1,5)),fm_T_darter=fmFinal(fuzzyMembership(test$T_darter,1,4)))}
otherSmall_BCGlevel3alt2 <- function(test){
  data.frame(fm_P_123=fmFinal(fuzzyMembership(test$P_123,4,10)),fm_PI_123=fmFinal(fuzzyMembership(test$PI_123,5,15))
             ,fm_T_6=fmFinal(1-fuzzyMembership(test$T_6,1,5)),fm_N_bTrout=fmFinal(fuzzyMembership(test$N_bTrout,0,3)))}
otherSmall_BCGlevel4alt1 <- function(test){
  data.frame(fm_T_123=fmFinal(fuzzyMembership(test$T_123,0,1)),fm_PI_56t=fmFinal(1-fuzzyMembership(test$PI_56t,80,90)))}
otherSmall_BCGlevel4alt2 <- function(test){
  data.frame(fm_totTax=fmFinal(fuzzyMembership(test$totTax,13,23)),fm_P_Dom56t=fmFinal(1-fuzzyMembership(test$P_Dom56t,25,35))
             ,fm_T_darter=fmFinal(fuzzyMembership(test$T_darter,1,4)))}
otherSmall_BCGlevel5 <- function(test){data.frame(fm_totTax=fmFinal(fuzzyMembership(test$totTax,3,6)))}

# BCG level logic, Other Small, NO BROOK TROUT
otherSmall_BCGlevel2_NOBKT <- function(test){
  data.frame(fm_totTax=fmFinal(fuzzyMembership(test$totTax,6,16)),fm_T_12=fmFinal(fuzzyMembership(test$T_12,0,1))
             ,fm_P_123=fmFinal(fuzzyMembership(test$P_123,10,20)),fm_PI_123=fmFinal(fuzzyMembership(test$PI_123,15,25))
             ,fm_PI_56t=fmFinal(1-fuzzyMembership(test$PI_56t,50,60)),fm_T_6=fmFinal(1-fuzzyMembership(test$T_6,0,2))
             ,fm_T_darter=fmFinal(fuzzyMembership(test$T_darter,2,5)))}
otherSmall_BCGlevel3_NOBKT <- function(test){
  data.frame(fm_totTax=fmFinal(fuzzyMembership(test$totTax,5,13)),fm_P_123=fmFinal(fuzzyMembership(test$P_123,4,10))
             ,fm_PI_123=fmFinal(fuzzyMembership(test$PI_123,5,15)),fm_PI_56t=fmFinal(1-fuzzyMembership(test$PI_56t,60,70))
             ,fm_T_6=fmFinal(1-fuzzyMembership(test$T_6,1,5)),fm_T_darter=fmFinal(fuzzyMembership(test$T_darter,0,3)))}
otherSmall_BCGlevel4alt1_NOBKT <- function(test){
  data.frame(fm_T_123=fmFinal(fuzzyMembership(test$T_123,0,1)),fm_PI_56t=fmFinal(1-fuzzyMembership(test$PI_56t,85,95)))}
otherSmall_BCGlevel4alt2_NOBKT <- function(test){
  data.frame(fm_totTax=fmFinal(fuzzyMembership(test$totTax,13,23)),fm_P_Dom56t=fmFinal(1-fuzzyMembership(test$P_Dom56t,25,35))
             ,fm_T_darter=fmFinal(fuzzyMembership(test$T_darter,1,4)))}
otherSmall_BCGlevel5_NOBKT <- function(test){
  data.frame(fm_totTax=fmFinal(fuzzyMembership(test$totTax,3,6)),fm_totInd=fmFinal(fuzzyMembership(test$totInd,50,60)))}

# BCG level logic, Above Falls Medium/Large
aboveFallsMedLarge_BCGlevel2 <- function(test){
  data.frame(fm_totTax=fmFinal(fuzzyMembership(test$totTax,5,15)),fm_T_12=fmFinal(fuzzyMembership(test$T_12,0,1))
             ,fm_P_123=fmFinal(fuzzyMembership(test$P_123,20,30)),fm_PI_123=fmFinal(fuzzyMembership(test$PI_123,25,35))
             ,fm_PI_5=fmFinal(1-fuzzyMembership(test$PI_5,30,40)),fm_T_6=fmFinal(1-fuzzyMembership(test$T_6,2,5))
             ,fm_T_6t=fmFinal(1-fuzzyMembership(test$T_6t,0,1)))}
aboveFallsMedLarge_BCGlevel3 <- function(test){
  data.frame(fm_totTax=fmFinal(fuzzyMembership(test$totTax,4,14)),fm_P_123=fmFinal(fuzzyMembership(test$P_123,15,25))
             ,fm_PI_123=fmFinal(fuzzyMembership(test$PI_123,15,25)),fm_PI_5=fmFinal(1-fuzzyMembership(test$PI_5,35,45))
             ,fm_T_6t=fmFinal(1-fuzzyMembership(test$T_6t,1,4)))}
aboveFallsMedLarge_BCGlevel4alt1 <- function(test){
  data.frame(fm_totTax=fmFinal(fuzzyMembership(test$totTax,6,10)),fm_T_123=fmFinal(fuzzyMembership(test$T_123,1,2))
             ,fm_P_123=fmFinal(fuzzyMembership(test$P_123,3,7)),fm_PI_56t=fmFinal(1-fuzzyMembership(test$PI_56t,65,75)))}
aboveFallsMedLarge_BCGlevel4alt2 <- function(test){
  data.frame(fm_totTax=fmFinal(fuzzyMembership(test$totTax,6,10)),fm_P_123=fmFinal(fuzzyMembership(test$P_123,3,7))
             ,fm_P_56=fmFinal(1-fuzzyMembership(test$P_56,60,70)),fm_PI_56t=fmFinal(1-fuzzyMembership(test$PI_56t,65,75)))}
aboveFallsMedLarge_BCGlevel5 <- function(test){data.frame(fm_totTax=fmFinal(fuzzyMembership(test$totTax,2,5)))}

# BCG level logic, Above Falls Medium/Large, NO BROOK TROUT
aboveFallsMedLarge_BCGlevel2_NOBKT <- function(test){
  data.frame(fm_totTax=fmFinal(fuzzyMembership(test$totTax,5,15)),fm_T_12=fmFinal(fuzzyMembership(test$T_12,0,1))
             ,fm_P_123=fmFinal(fuzzyMembership(test$P_123,20,30)),fm_PI_123=fmFinal(fuzzyMembership(test$PI_123,25,35))
             ,fm_PI_5=fmFinal(1-fuzzyMembership(test$PI_5,30,40)),fm_T_6=fmFinal(1-fuzzyMembership(test$T_6,1,4))
             ,fm_T_6t=fmFinal(1-fuzzyMembership(test$T_6t,0,1)))}
aboveFallsMedLarge_BCGlevel3_NOBKT <- function(test){
  data.frame(fm_totTax=fmFinal(fuzzyMembership(test$totTax,4,14)),fm_P_123=fmFinal(fuzzyMembership(test$P_123,15,25))
             ,fm_PI_123=fmFinal(fuzzyMembership(test$PI_123,15,25)),fm_PI_5=fmFinal(1-fuzzyMembership(test$PI_5,35,45))
             ,fm_T_6t=fmFinal(1-fuzzyMembership(test$T_6t,1,4)))}
aboveFallsMedLarge_BCGlevel4alt1_NOBKT <- function(test){
  data.frame(fm_totTax=fmFinal(fuzzyMembership(test$totTax,6,10)),fm_T_123=fmFinal(fuzzyMembership(test$T_123,1,2))
             ,fm_P_123=fmFinal(fuzzyMembership(test$P_123,3,7)),fm_PI_56t=fmFinal(1-fuzzyMembership(test$PI_56t,65,75)))}
aboveFallsMedLarge_BCGlevel4alt2_NOBKT <- function(test){
  data.frame(fm_totTax=fmFinal(fuzzyMembership(test$totTax,6,10)),fm_P_123=fmFinal(fuzzyMembership(test$P_123,3,7))
             ,fm_P_56=fmFinal(1-fuzzyMembership(test$P_56,60,70)),fm_PI_56t=fmFinal(1-fuzzyMembership(test$PI_56t,65,75)))}
aboveFallsMedLarge_BCGlevel5_NOBKT <- function(test){
  data.frame(fm_totTax=fmFinal(fuzzyMembership(test$totTax,2,5)),fm_totInd=fmFinal(fuzzyMembership(test$totInd,50,60)))}

# BCG level logic, Above Falls Small
aboveFallsSmall_BCGlevel2alt1 <- function(test){
  data.frame(fm_totTax=fmFinal(fuzzyMembership(test$totTax,4,12)),fm_T_12=fmFinal(fuzzyMembership(test$T_12,0,1))
             ,fm_P_123=fmFinal(fuzzyMembership(test$P_123,20,30)),fm_PI_123=fmFinal(fuzzyMembership(test$PI_123,25,35))
             ,fm_PI_5=fmFinal(1-fuzzyMembership(test$PI_5,30,40)),fm_T_6=fmFinal(1-fuzzyMembership(test$T_6,2,5))
             ,fm_T_6t=fmFinal(1-fuzzyMembership(test$T_6t,0,1)))}
aboveFallsSmall_BCGlevel2alt2 <- function(test){
  data.frame(fm_T_12=fmFinal(fuzzyMembership(test$T_12,0,1)),fm_P_123=fmFinal(fuzzyMembership(test$P_123,20,30))
             ,fm_PI_123=fmFinal(fuzzyMembership(test$PI_123,25,35)),fm_PI_5=fmFinal(1-fuzzyMembership(test$PI_5,30,40))
             ,fm_T_6=fmFinal(1-fuzzyMembership(test$T_6,2,5)),fm_T_6t=fmFinal(1-fuzzyMembership(test$T_6t,0,1))
             ,fm_N_bTrout=fmFinal(fuzzyMembership(test$N_bTrout,0,3)))}
aboveFallsSmall_BCGlevel3alt1 <- function(test){
  data.frame(fm_totTax=fmFinal(fuzzyMembership(test$totTax,4,11)),fm_P_123=fmFinal(fuzzyMembership(test$P_123,5,15))
             ,fm_PI_123=fmFinal(fuzzyMembership(test$PI_123,15,25)),fm_PI_5=fmFinal(1-fuzzyMembership(test$PI_5,50,60))
             ,fm_T_6t=fmFinal(1-fuzzyMembership(test$T_6t,1,4)))}
aboveFallsSmall_BCGlevel3alt2 <- function(test){
  data.frame(fm_P_123=fmFinal(fuzzyMembership(test$P_123,5,15)),fm_PI_123=fmFinal(fuzzyMembership(test$PI_123,15,25))
             ,fm_PI_5=fmFinal(1-fuzzyMembership(test$PI_5,50,60)),fm_T_6t=fmFinal(1-fuzzyMembership(test$T_6t,1,4))
             ,fm_N_bTrout=fmFinal(fuzzyMembership(test$N_bTrout,0,3)))}
aboveFallsSmall_BCGlevel4alt1 <- function(test){
  data.frame(fm_T_123=fmFinal(fuzzyMembership(test$T_123,1,2)),fm_P_123=fmFinal(fuzzyMembership(test$P_123,3,7))
             ,fm_PI_56t=fmFinal(1-fuzzyMembership(test$PI_56t,70,80)))}
aboveFallsSmall_BCGlevel4alt2 <- function(test){
  data.frame(fm_P_123=fmFinal(fuzzyMembership(test$P_123,3,7)),fm_P_56=fmFinal(1-fuzzyMembership(test$P_56,60,70))
             ,fm_PI_56t=fmFinal(1-fuzzyMembership(test$PI_56t,70,80)))}
aboveFallsSmall_BCGlevel5 <- function(test){data.frame(fm_totTax=fmFinal(fuzzyMembership(test$totTax,2,5)))}

# BCG level logic, Above Falls Small, NO BROOK TROUT
aboveFallsSmall_BCGlevel2_NOBKT <- function(test){
  data.frame(fm_totTax=fmFinal(fuzzyMembership(test$totTax,4,12)),fm_T_12=fmFinal(fuzzyMembership(test$T_12,0,1))
             ,fm_P_123=fmFinal(fuzzyMembership(test$P_123,20,30)),fm_PI_123=fmFinal(fuzzyMembership(test$PI_123,25,35))
             ,fm_PI_5=fmFinal(1-fuzzyMembership(test$PI_5,30,40)),fm_T_6=fmFinal(1-fuzzyMembership(test$T_6,1,4))
             ,fm_T_6t=fmFinal(1-fuzzyMembership(test$T_6t,0,1)))}
aboveFallsSmall_BCGlevel3_NOBKT <- function(test){
  data.frame(fm_totTax=fmFinal(fuzzyMembership(test$totTax,3,9)),fm_P_123=fmFinal(fuzzyMembership(test$P_123,5,15))
             ,fm_PI_123=fmFinal(fuzzyMembership(test$PI_123,15,25)),fm_PI_5=fmFinal(1-fuzzyMembership(test$PI_5,50,60))
             ,fm_T_6t=fmFinal(1-fuzzyMembership(test$T_6t,1,4)))}
aboveFallsSmall_BCGlevel4alt1_NOBKT <- function(test){
  data.frame(fm_T_123=fmFinal(fuzzyMembership(test$T_123,1,2)),fm_P_123=fmFinal(fuzzyMembership(test$P_123,3,7))
             ,fm_PI_56t=fmFinal(1-fuzzyMembership(test$PI_56t,70,80)),fm_P_Dom56t=fmFinal(1-fuzzyMembership(test$P_Dom56t,45,55)))}
aboveFallsSmall_BCGlevel4alt2_NOBKT <- function(test){
  data.frame(fm_P_123=fmFinal(fuzzyMembership(test$P_123,3,7)),fm_P_56=fmFinal(1-fuzzyMembership(test$P_56,60,70))
             ,fm_PI_56t=fmFinal(1-fuzzyMembership(test$PI_56t,70,80)),fm_P_Dom56t=fmFinal(1-fuzzyMembership(test$P_Dom56t,45,55)))}
aboveFallsSmall_BCGlevel5_NOBKT <- function(test){
  data.frame(fm_totTax=fmFinal(fuzzyMembership(test$totTax,2,5)),fm_totInd=fmFinal(fuzzyMembership(test$totInd,50,60)))}

##-------------------------------- BCG Level Rules BUG ---------------------------------------------------------------------
# BCG level logic, Bug BCG Level 2
Bug_BCGlevel2 <- function(test){
  data.frame(fm_totTax=fmFinal_bug(fuzzyMembership_bug(test$totTax,20,28)),
             fm_T_2=fmFinal_bug(fuzzyMembership_bug(test$T_2,1,3)),
             fm_T_23=fmFinal_bug(fuzzyMembership_bug(test$T_23,12,18)),
             fm_T_23EPT=fmFinal_bug(fuzzyMembership_bug(test$T_23EPT,10,15)),
             fm_P5_domAtt456=fmFinal_bug(fuzzyMembership_bug(test$P5_domAtt456,40,30)))}
# BCG level logic, Bug BCG Level 3
Bug_BCGlevel3 <- function(test){
  data.frame(fm_T_23=fmFinal_bug(fuzzyMembership_bug(test$T_23,6,10)),
             fm_PI_23=fmFinal_bug(fuzzyMembership_bug(test$PI_23,25,35)),
             fm_PI_23EPT=fmFinal_bug(fuzzyMembership_bug(test$PI_23EPT,20,30)),
             fm_PI_5=fmFinal_bug(fuzzyMembership_bug(test$PI_5,10,5)),
             fm_PI1_domAtt45=fmFinal_bug(fuzzyMembership_bug(test$PI1_domAtt45,50,40)))}
# BCG level logic, Bug BCG Level 4
Bug_BCGlevel4 <- function(test){
  data.frame(fm_T_23=fmFinal_bug(fuzzyMembership_bug(test$T_23,2,4)),
             fm_T_234=fmFinal_bug(fuzzyMembership_bug(test$T_234,10,14)),
             fm_T_EPT=fmFinal_bug(fuzzyMembership_bug(test$T_EPT,5,9)),
             fm_P_5=fmFinal_bug(fuzzyMembership_bug(test$P_5,35,25)),
             fm_PI_5midge=fmFinal_bug(fuzzyMembership_bug(test$PI_5midge,65,55)))}
# BCG level logic, Bug BCG Level 5
Bug_BCGlevel5 <- function(test){
  data.frame(fm_totTax=fmFinal_bug(fuzzyMembership_bug(test$totTax,4,12)),
             fm_T_EPT=fmFinal_bug(fuzzyMembership_bug(test$T_EPT,0,5)),
             fm_PI_EPT=fmFinal_bug(fuzzyMembership_bug(test$PI_EPT,5,10)),
             fm_PI_Diptera=fmFinal_bug(fuzzyMembership_bug(test$PI_Diptera,90,80)))}

##----------------------------- FISH Models ---------------------------------------------------------------------------
### Other Medium/Large BCG Model
OtherMedLargeModel <- function(sampleName,taxaListFromOneSite){
  metricResults <- masterMetric(sampleName,taxaListFromOneSite)
  levelresult <- data.frame(SampleName=metricResults[1]
                            ,BCGlevel5=min(otherMedLarge_BCGlevel5(metricResults))
                            ,BCGlevel4=max(min(otherMedLarge_BCGlevel4alt1(metricResults)),min(otherMedLarge_BCGlevel4alt2(metricResults)))
                            ,BCGlevel3=min(otherMedLarge_BCGlevel3(metricResults))
                            ,BCGlevel2=min(otherMedLarge_BCGlevel2(metricResults)))%>%
    mutate(Level1=0,Level2=min(1-Level1,BCGlevel2),Level3=min(1-sum(Level1,Level2),BCGlevel3)
           ,Level4=min(1-sum(Level1,Level2,Level3),BCGlevel4)
           ,Level5=min(1-sum(Level1,Level2,Level3,Level4),BCGlevel5)
           ,Level6=1-sum(Level1,Level2,Level3,Level4,Level5))
  placehold<-sort(levelresult[6:11],TRUE)[2] # pick second highest BCG level, can't be done in mutate statement
  final <- data.frame(SampleName=metricResults[1]
                      ,nominalTier=colnames(levelresult[,6:11])[apply(levelresult[6:11],1,which.max)]
                      ,nominalMembership=apply(levelresult[,6:11],1,max)
                      ,secondMembership=placehold[1,]
                      ,runnerupTier=ifelse(placehold[1,]==0,"",colnames(placehold)))%>%
    mutate(close=ifelse(nominalMembership-secondMembership<0.1,"tie"
                        ,ifelse(nominalMembership-secondMembership<0.2,"yes","")))
  return(final)
}


### Other Medium/Large BCG Model, NO BROOK TROUT
OtherMedLargeModel_NOBKT <- function(sampleName,taxaListFromOneSite){
  metricResults <- masterMetric(sampleName,taxaListFromOneSite)
  levelresult <- data.frame(SampleName=metricResults[1]
                            ,BCGlevel5=max(min(otherMedLarge_BCGlevel5alt1_NOBKT(metricResults)),min(otherMedLarge_BCGlevel5alt2_NOBKT(metricResults)))
                            ,BCGlevel4=max(min(otherMedLarge_BCGlevel4alt1_NOBKT(metricResults)),min(otherMedLarge_BCGlevel4alt2_NOBKT(metricResults)))
                            ,BCGlevel3=min(otherMedLarge_BCGlevel3_NOBKT(metricResults))
                            ,BCGlevel2=min(otherMedLarge_BCGlevel2_NOBKT(metricResults)))%>%
    mutate(Level1=0,Level2=min(1-Level1,BCGlevel2),Level3=min(1-sum(Level1,Level2),BCGlevel3)
           ,Level4=min(1-sum(Level1,Level2,Level3),BCGlevel4)
           ,Level5=min(1-sum(Level1,Level2,Level3,Level4),BCGlevel5)
           ,Level6=1-sum(Level1,Level2,Level3,Level4,Level5))
  placehold<-sort(levelresult[6:11],TRUE)[2] # pick second highest BCG level, can't be done in mutate statement
  final <- data.frame(SampleName=metricResults[1]
                      ,nominalTier=colnames(levelresult[,6:11])[apply(levelresult[6:11],1,which.max)]
                      ,nominalMembership=apply(levelresult[,6:11],1,max)
                      ,secondMembership=placehold[1,]
                      ,runnerupTier=ifelse(placehold[1,]==0,"",colnames(placehold)))%>%
    mutate(close=ifelse(nominalMembership-secondMembership<0.1,"tie"
                        ,ifelse(nominalMembership-secondMembership<0.2,"yes","")))
  return(final)
}


### Other Small BCG Model
OtherSmallModel <- function(sampleName,taxaListFromOneSite){
  metricResults <- masterMetric(sampleName,taxaListFromOneSite)
  levelresult <- data.frame(SampleName=metricResults[1]
                            ,BCGlevel5=min(otherSmall_BCGlevel5(metricResults))
                            ,BCGlevel4=max(min(otherSmall_BCGlevel4alt1(metricResults)),min(otherSmall_BCGlevel4alt2(metricResults)))
                            ,BCGlevel3=max(min(otherSmall_BCGlevel3alt1(metricResults)),min(otherSmall_BCGlevel3alt2(metricResults)))
                            ,BCGlevel2=max(min(otherSmall_BCGlevel2alt1(metricResults)),min(otherSmall_BCGlevel2alt2(metricResults))))%>%
    mutate(Level1=0,Level2=min(1-Level1,BCGlevel2),Level3=min(1-sum(Level1,Level2),BCGlevel3)
           ,Level4=min(1-sum(Level1,Level2,Level3),BCGlevel4)
           ,Level5=min(1-sum(Level1,Level2,Level3,Level4),BCGlevel5)
           ,Level6=1-sum(Level1,Level2,Level3,Level4,Level5))
  placehold<-sort(levelresult[6:11],TRUE)[2] # pick second highest BCG level, can't be done in mutate statement
  final <- data.frame(SampleName=metricResults[1]
                      ,nominalTier=colnames(levelresult[,6:11])[apply(levelresult[6:11],1,which.max)]
                      ,nominalMembership=apply(levelresult[,6:11],1,max)
                      ,secondMembership=placehold[1,]
                      ,runnerupTier=ifelse(placehold[1,]==0,"",colnames(placehold)))%>%
    mutate(close=ifelse(nominalMembership-secondMembership<0.1,"tie"
                        ,ifelse(nominalMembership-secondMembership<0.2,"yes","")))
  return(final)
}

### Other Small BCG Model, NO BROOK TROUT
OtherSmallModel_NOBKT <- function(sampleName,taxaListFromOneSite){
  metricResults <- masterMetric(sampleName,taxaListFromOneSite)
  levelresult <- data.frame(SampleName=metricResults[1]
                            ,BCGlevel5=min(otherSmall_BCGlevel5_NOBKT(metricResults))
                            ,BCGlevel4=max(min(otherSmall_BCGlevel4alt1_NOBKT(metricResults)),min(otherSmall_BCGlevel4alt2_NOBKT(metricResults)))
                            ,BCGlevel3=min(otherSmall_BCGlevel3_NOBKT(metricResults))
                            ,BCGlevel2=min(otherSmall_BCGlevel2_NOBKT(metricResults)))%>%
    mutate(Level1=0,Level2=min(1-Level1,BCGlevel2),Level3=min(1-sum(Level1,Level2),BCGlevel3)
           ,Level4=min(1-sum(Level1,Level2,Level3),BCGlevel4)
           ,Level5=min(1-sum(Level1,Level2,Level3,Level4),BCGlevel5)
           ,Level6=1-sum(Level1,Level2,Level3,Level4,Level5))
  placehold<-sort(levelresult[6:11],TRUE)[2] # pick second highest BCG level, can't be done in mutate statement
  final <- data.frame(SampleName=metricResults[1]
                      ,nominalTier=colnames(levelresult[,6:11])[apply(levelresult[6:11],1,which.max)]
                      ,nominalMembership=apply(levelresult[,6:11],1,max)
                      ,secondMembership=placehold[1,]
                      ,runnerupTier=ifelse(placehold[1,]==0,"",colnames(placehold)))%>%
    mutate(close=ifelse(nominalMembership-secondMembership<0.1,"tie"
                        ,ifelse(nominalMembership-secondMembership<0.2,"yes","")))
  return(final)
}


### AboveFalls Medium/Large BCG Model
AboveFallsMedLargeModel <- function(sampleName,taxaListFromOneSite){
  metricResults <- masterMetric(sampleName,taxaListFromOneSite)
  levelresult <- data.frame(SampleName=metricResults[1]
                            ,BCGlevel5=min(aboveFallsMedLarge_BCGlevel5(metricResults))
                            ,BCGlevel4=max(min(aboveFallsMedLarge_BCGlevel4alt1(metricResults)),min(aboveFallsMedLarge_BCGlevel4alt2(metricResults)))
                            ,BCGlevel3=min(aboveFallsMedLarge_BCGlevel3(metricResults))
                            ,BCGlevel2=min(aboveFallsMedLarge_BCGlevel2(metricResults)))%>%
    mutate(Level1=0,Level2=min(1-Level1,BCGlevel2),Level3=min(1-sum(Level1,Level2),BCGlevel3)
           ,Level4=min(1-sum(Level1,Level2,Level3),BCGlevel4)
           ,Level5=min(1-sum(Level1,Level2,Level3,Level4),BCGlevel5)
           ,Level6=1-sum(Level1,Level2,Level3,Level4,Level5))
  placehold<-sort(levelresult[6:11],TRUE)[2] # pick second highest BCG level, can't be done in mutate statement
  final <- data.frame(SampleName=metricResults[1]
                      ,nominalTier=colnames(levelresult[,6:11])[apply(levelresult[6:11],1,which.max)]
                      ,nominalMembership=apply(levelresult[,6:11],1,max)
                      ,secondMembership=placehold[1,]
                      ,runnerupTier=ifelse(placehold[1,]==0,"",colnames(placehold)))%>%
    mutate(close=ifelse(nominalMembership-secondMembership<0.1,"tie"
                        ,ifelse(nominalMembership-secondMembership<0.2,"yes","")))
  return(final)
}


### AboveFalls Medium/Large BCG Model, NO BROOK TROUT
AboveFallsMedLargeModel_NOBKT <- function(sampleName,taxaListFromOneSite){
  metricResults <- masterMetric(sampleName,taxaListFromOneSite)
  levelresult <- data.frame(SampleName=metricResults[1]
                            ,BCGlevel5=min(aboveFallsMedLarge_BCGlevel5_NOBKT(metricResults))
                            ,BCGlevel4=max(min(aboveFallsMedLarge_BCGlevel4alt1_NOBKT(metricResults)),min(aboveFallsMedLarge_BCGlevel4alt2_NOBKT(metricResults)))
                            ,BCGlevel3=min(aboveFallsMedLarge_BCGlevel3_NOBKT(metricResults))
                            ,BCGlevel2=min(aboveFallsMedLarge_BCGlevel2_NOBKT(metricResults)))%>%
    mutate(Level1=0,Level2=min(1-Level1,BCGlevel2),Level3=min(1-sum(Level1,Level2),BCGlevel3)
           ,Level4=min(1-sum(Level1,Level2,Level3),BCGlevel4)
           ,Level5=min(1-sum(Level1,Level2,Level3,Level4),BCGlevel5)
           ,Level6=1-sum(Level1,Level2,Level3,Level4,Level5))
  placehold<-sort(levelresult[6:11],TRUE)[2] # pick second highest BCG level, can't be done in mutate statement
  final <- data.frame(SampleName=metricResults[1]
                      ,nominalTier=colnames(levelresult[,6:11])[apply(levelresult[6:11],1,which.max)]
                      ,nominalMembership=apply(levelresult[,6:11],1,max)
                      ,secondMembership=placehold[1,]
                      ,runnerupTier=ifelse(placehold[1,]==0,"",colnames(placehold)))%>%
    mutate(close=ifelse(nominalMembership-secondMembership<0.1,"tie"
                        ,ifelse(nominalMembership-secondMembership<0.2,"yes","")))
  return(final)
}


### Above Falls Small BCG Model
AboveFallsSmallModel <- function(sampleName,taxaListFromOneSite){
  metricResults <- masterMetric(sampleName,taxaListFromOneSite)
  levelresult <- data.frame(SampleName=metricResults[1]
                            ,BCGlevel5=min(aboveFallsSmall_BCGlevel5(metricResults))
                            ,BCGlevel4=max(min(aboveFallsSmall_BCGlevel4alt1(metricResults)),min(aboveFallsSmall_BCGlevel4alt2(metricResults)))
                            ,BCGlevel3=max(min(aboveFallsSmall_BCGlevel3alt1(metricResults)),min(aboveFallsSmall_BCGlevel3alt2(metricResults)))
                            ,BCGlevel2=max(min(aboveFallsSmall_BCGlevel2alt1(metricResults)),min(aboveFallsSmall_BCGlevel2alt2(metricResults))))%>%
    mutate(Level1=0,Level2=min(1-Level1,BCGlevel2),Level3=min(1-sum(Level1,Level2),BCGlevel3)
           ,Level4=min(1-sum(Level1,Level2,Level3),BCGlevel4)
           ,Level5=min(1-sum(Level1,Level2,Level3,Level4),BCGlevel5)
           ,Level6=1-sum(Level1,Level2,Level3,Level4,Level5))
  placehold<-sort(levelresult[6:11],TRUE)[2] # pick second highest BCG level, can't be done in mutate statement
  final <- data.frame(SampleName=metricResults[1]
                      ,nominalTier=colnames(levelresult[,6:11])[apply(levelresult[6:11],1,which.max)]
                      ,nominalMembership=apply(levelresult[,6:11],1,max)
                      ,secondMembership=placehold[1,]
                      ,runnerupTier=ifelse(placehold[1,]==0,"",colnames(placehold)))%>%
    mutate(close=ifelse(nominalMembership-secondMembership<0.1,"tie"
                        ,ifelse(nominalMembership-secondMembership<0.2,"yes","")))
  return(final)
}


### Above Falls Small BCG Model, NO BROOK TROUT
AboveFallsSmallModel_NOBKT <- function(sampleName,taxaListFromOneSite){
  metricResults <- masterMetric(sampleName,taxaListFromOneSite)
  levelresult <- data.frame(SampleName=metricResults[1]
                            ,BCGlevel5=min(aboveFallsSmall_BCGlevel5_NOBKT(metricResults))
                            ,BCGlevel4=max(min(aboveFallsSmall_BCGlevel4alt1_NOBKT(metricResults)),min(aboveFallsSmall_BCGlevel4alt2_NOBKT(metricResults)))
                            ,BCGlevel3=min(aboveFallsSmall_BCGlevel3_NOBKT(metricResults))
                            ,BCGlevel2=min(aboveFallsSmall_BCGlevel2_NOBKT(metricResults)))%>%
    mutate(Level1=0,Level2=min(1-Level1,BCGlevel2),Level3=min(1-sum(Level1,Level2),BCGlevel3)
           ,Level4=min(1-sum(Level1,Level2,Level3),BCGlevel4)
           ,Level5=min(1-sum(Level1,Level2,Level3,Level4),BCGlevel5)
           ,Level6=1-sum(Level1,Level2,Level3,Level4,Level5))
  placehold<-sort(levelresult[6:11],TRUE)[2] # pick second highest BCG level, can't be done in mutate statement
  final <- data.frame(SampleName=metricResults[1]
                      ,nominalTier=colnames(levelresult[,6:11])[apply(levelresult[6:11],1,which.max)]
                      ,nominalMembership=apply(levelresult[,6:11],1,max)
                      ,secondMembership=placehold[1,]
                      ,runnerupTier=ifelse(placehold[1,]==0,"",colnames(placehold)))%>%
    mutate(close=ifelse(nominalMembership-secondMembership<0.1,"tie"
                        ,ifelse(nominalMembership-secondMembership<0.2,"yes","")))
  return(final)
}

##----------------------------- Bug Model ---------------------------------------------------------------------------
Bug_Model <- function(sampleName,taxaListFromOneSite){
  metricResults <- masterMetric_bug(sampleName,taxaListFromOneSite)
  levelresult <- data.frame(SampleName=metricResults[1],
                            BCGlevel2=min(Bug_BCGlevel2(metricResults)),
                            BCGlevel3=min(Bug_BCGlevel3(metricResults)),
                            BCGlevel4=min(Bug_BCGlevel4(metricResults)),
                            BCGlevel5=min(Bug_BCGlevel5(metricResults)))%>%
    mutate(Level2=BCGlevel2,Level3=min(1-BCGlevel2,BCGlevel3),Level4=min(1-sum(Level2,Level3),BCGlevel4),
           Level5=min(1-sum(Level2,Level3,Level4),BCGlevel5),Level6=1-sum(Level2,Level3,Level4,Level5))
  placehold<-sort(levelresult[6:10],TRUE)[2] # pick second highest BCG level, can't be done in mutate statement
  final <- data.frame(SampleName=metricResults[1]
                      ,nominalTier=colnames(levelresult[,6:10])[apply(levelresult[6:10],1,which.max)]
                      ,nominalMembership=apply(levelresult[,6:10],1,max)
                      ,secondMembership=placehold[1,]
                      ,runnerupTier=ifelse(placehold[1,]==0,"",colnames(placehold)))%>%
    mutate(close=ifelse(nominalMembership-secondMembership<0.1,"tie"
                        ,ifelse(nominalMembership-secondMembership<0.2,"yes","")))
  return(final)
}

##----------------------------------- FISH MODEL DETERMINATION ---------------------------------------------------
# Final BCG Model
BCG_Model_GIS <- function(dfInCorrectFormat){
  # split input dataframe by sampples, return list of dataframes
  splits <-split(dfInCorrectFormat,dfInCorrectFormat$SampleName,drop=T)%>% 
    lapply(function(x) x[!(names(x) %in% c('SampleName'))]) # remove SampleName column, housekeeping
  
  # establish blank dataframe to store data
  result <- data.frame(SampleName=NA,Date=NA,Catchment=NA,nominalTier=NA,nominalMembership=NA,secondMembership=NA,runnerupTier=NA
                       ,close=NA,Model=NA,FishAttributeComments=NA,TotalTaxaComments=NA,CatchmentSizeComments=NA,SampleWindowComments=NA)
  #### Get attribute data in
  att <- readRDS('data/attributes.RDS')
  
  for(i in 1:length(splits)){ #loop through each dataframe in the list of dataframes and do:
    splits[[i]] <- join(splits[[i]],att,by=c('ScientificName','CommonName','Subbasin_short'))
    # basic housekeeping
    print(i)
    samplename <- names(splits)[i]
    sampleathand <- data.frame(splits[[i]])
    
    # Taxa list QA, check to make sure no NA's for attributes after joined to correct Subbasin
    comment1 <- if(sum(is.na(sampleathand$attLevel))>0){
      NArows <- filter(sampleathand, is.na(attLevel))
      commonNames <- paste(as.character(NArows$CommonName),sep=', ',collapse=', ')
      paste(commonNames,'not attributed in the',NArows$Subbasin,'Subbasin',sep=' ')[1]
    }else(' ') #no taxa attribute list problems
    
    # Total Taxa QA, identify when results based on < 75 total fish
    comment2 <- ifelse(sum(sampleathand$Count)<75,
                       "Model results based on sample containing < 75 fish"," ")
    
    # Notify user that two models will be run because catchment size close to cut off
    comment3 <- paste(ifelse(splits[[i]]$Catchment[1] < 3,"Catchment < 3 sq mi"," "),
                      ifelse(splits[[i]]$Catchment[1] > 5 & splits[[i]]$Catchment[1] < 15,
                       "Small and Medium/Large model run"," "),sep=" ")
    
    # Sampling window comment
    step1 <- mutate(sampleathand, Date1=as.Date(as.character(sampleathand$Date),format="%m/%d/%Y"),
                       Month=as.numeric(format(Date1,"%m")),SampleWindowComments=ifelse(Month>7&Month<11,'','Not in Sampling Window'))
    comment4 <- step1[1,13]
    
    # Model decisions based on Subbasin and Catchment size (mi2)
    if(splits[[i]]$Subbasin_short[1] %in% c('UNew','MNew_WV','MNew_VA','LNew','Greenbrier','Gauley')){
      if(splits[[i]]$Catchment[1] < 5){
        if(any(splits[[i]]$CommonName %in% c('brook trout'))){
          result <- rbind(result,cbind(Date=splits[[i]]$Date[1],Catchment=splits[[i]]$Catchment[1],AboveFallsSmallModel(samplename,sampleathand),
                                       Model='AboveFalls,Small',FishAttributeComments=comment1,TotalTaxaComments=comment2,
                                       CatchmentSizeComments=comment3,SampleWindowComments=comment4))
        }else(result <- rbind(result,cbind(Date=splits[[i]]$Date[1],Catchment=splits[[i]]$Catchment[1],AboveFallsSmallModel_NOBKT(samplename,sampleathand),
                                           Model='AboveFalls,Small,NOBKT',FishAttributeComments=comment1,
                                           TotalTaxaComments=comment2,CatchmentSizeComments=comment3,SampleWindowComments=comment4)))
      }else if(splits[[i]]$Catchment[1] > 5 & splits[[i]]$Catchment[1] < 15){
        if(any(splits[[i]]$CommonName %in% c('brook trout'))){
          result <- rbind(result,cbind(Date=splits[[i]]$Date[1],Catchment=splits[[i]]$Catchment[1],AboveFallsSmallModel(samplename,sampleathand),
                                       Model='AboveFalls,Small',FishAttributeComments=comment1,
                                       TotalTaxaComments=comment2,CatchmentSizeComments=comment3,SampleWindowComments=comment4)
                          ,cbind(Date=splits[[i]]$Date[1],Catchment=splits[[i]]$Catchment[1],AboveFallsMedLargeModel(samplename,sampleathand),
                                 Model='AboveFalls,MediumLarge',FishAttributeComments=comment1,
                                 TotalTaxaComments=comment2,CatchmentSizeComments=comment3,SampleWindowComments=comment4))
        }else(result <- rbind(result,cbind(Date=splits[[i]]$Date[1],Catchment=splits[[i]]$Catchment[1],AboveFallsSmallModel_NOBKT(samplename,sampleathand),
                                           Model='AboveFalls,Small,NOBKT',FishAttributeComments=comment1,
                                           TotalTaxaComments=comment2,CatchmentSizeComments=comment3,SampleWindowComments=comment4)
                              ,cbind(Date=splits[[i]]$Date[1],Catchment=splits[[i]]$Catchment[1],AboveFallsMedLargeModel_NOBKT(samplename,sampleathand),
                                     Model='AboveFalls,MediumLarge,NOBKT',FishAttributeComments=comment1,
                                     TotalTaxaComments=comment2,CatchmentSizeComments=comment3,SampleWindowComments=comment4)))
      }else if(splits[[i]]$Catchment[1] > 15){
        if(any(splits[[i]]$CommonName %in% c('brook trout'))){
          result <- rbind(result,cbind(Date=splits[[i]]$Date[1],Catchment=splits[[i]]$Catchment[1],AboveFallsMedLargeModel(samplename,sampleathand),
                                       Model='AboveFalls,MediumLarge',FishAttributeComments=comment1,
                                       TotalTaxaComments=comment2,CatchmentSizeComments=comment3,SampleWindowComments=comment4))
        }else(result <- rbind(result,cbind(Date=splits[[i]]$Date[1],Catchment=splits[[i]]$Catchment[1],AboveFallsMedLargeModel_NOBKT(samplename,sampleathand),
                                           Model='AboveFalls,MediumLarge,NOBKT',FishAttributeComments=comment1,
                                           TotalTaxaComments=comment2,CatchmentSizeComments=comment3,SampleWindowComments=comment4)))
      }
    }else{
      if(splits[[i]]$Catchment[1] < 5){
        if(any(splits[[i]]$CommonName %in% c('brook trout'))){
          result <- rbind(result,cbind(Date=splits[[i]]$Date[1],Catchment=splits[[i]]$Catchment[1],OtherSmallModel(samplename,sampleathand),Model='Other,Small',
                                       FishAttributeComments=comment1,TotalTaxaComments=comment2,CatchmentSizeComments=comment3,SampleWindowComments=comment4))
        }else(result <- rbind(result,cbind(Date=splits[[i]]$Date[1],Catchment=splits[[i]]$Catchment[1],OtherSmallModel_NOBKT(samplename,sampleathand),
                                           Model='Other,Small,NOBKT',FishAttributeComments=comment1,TotalTaxaComments=comment2,
                                           CatchmentSizeComments=comment3,SampleWindowComments=comment4)))
      }else if(splits[[i]]$Catchment[1] > 5 & splits[[i]]$Catchment[1] < 15){
        if(any(splits[[i]]$CommonName %in% c('brook trout'))){
          result <- rbind(result,cbind(Date=splits[[i]]$Date[1],Catchment=splits[[i]]$Catchment[1],OtherSmallModel(samplename,sampleathand),Model='Other,Small',
                                       FishAttributeComments=comment1,TotalTaxaComments=comment2,CatchmentSizeComments=comment3,SampleWindowComments=comment4)
                          ,cbind(Date=splits[[i]]$Date[1],Catchment=splits[[i]]$Catchment[1],OtherMedLargeModel(samplename,sampleathand),Model='Other,MediumLarge',
                                 FishAttributeComments=comment1,TotalTaxaComments=comment2,CatchmentSizeComments=comment3,SampleWindowComments=comment4))
        }else(result <- rbind(result,cbind(Date=splits[[i]]$Date[1],Catchment=splits[[i]]$Catchment[1],OtherSmallModel_NOBKT(samplename,sampleathand),
                                           Model='Other,Small,NOBKT',FishAttributeComments=comment1,TotalTaxaComments=comment2,
                                           CatchmentSizeComments=comment3,SampleWindowComments=comment4)
                              ,cbind(Date=splits[[i]]$Date[1],Catchment=splits[[i]]$Catchment[1],OtherMedLargeModel_NOBKT(samplename,sampleathand),
                                     Model='Other,MediumLarge,NOBKT',FishAttributeComments=comment1,TotalTaxaComments=comment2,
                                     CatchmentSizeComments=comment3,SampleWindowComments=comment4)))
      }else if(splits[[i]]$Catchment[1] > 15){
        if(any(splits[[i]]$CommonName %in% c('brook trout'))){
          result <- rbind(result,cbind(Date=splits[[i]]$Date[1],Catchment=splits[[i]]$Catchment[1],OtherMedLargeModel(samplename,sampleathand),Model='Other,MediumLarge',
                                       FishAttributeComments=comment1,TotalTaxaComments=comment2,CatchmentSizeComments=comment3,SampleWindowComments=comment4))
        }else(result <- rbind(result,cbind(Date=splits[[i]]$Date[1],Catchment=splits[[i]]$Catchment[1],OtherMedLargeModel_NOBKT(samplename,sampleathand),
                                           Model='Other,MediumLarge,NOBKT',FishAttributeComments=comment1,TotalTaxaComments=comment2,
                                           CatchmentSizeComments=comment3,SampleWindowComments=comment4)))
      }
    }
    result<-filter(result,!(SampleName=='NA'))
  }
  return(result)
}



##----------------------------------- FINAL BUG MODEL ------------------------------------------------------------------
Bug_BCG_Model_GIS <- function(dfInCorrectFormat){
  # Make sure sites are in Eco69
  polys <- eco69_wgs84  
  # Make shapefile from dfInCorrectFormat
  sites_shp <- dfInCorrectFormat
  coordinates(sites_shp) <- ~Longitude+Latitude
  sites_shp@proj4string <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 ") #first need to give it it's own projection 
  # Make proper date format to test against sampling window
  datedf <- mutate(dfInCorrectFormat,lubridate=mdy(Date),Year=year(lubridate),JulianDay=yday(lubridate))
  
  Comments <- data.frame(SiteLocationComment=NA,SamplingWindowComment=NA)
  
  for(i in 1:length(sites_shp)){
    # Intersect site lat/long with eco69 poly
    sites_int <- over(sites_shp[i,],polys)
    Comments[i,1] <- ifelse(sites_int$NAME=='Central Appalachians',' ','Site does not fall in Ecoregion 69 (EPA Level 3)')
    # Sampling window test
    Comments[i,2] <- if(datedf$Year[i] %in% c('1992','1996','2000','2004','2008','2012','2016','2020','2024','2028')){
      # Leap Years
      ifelse(datedf$JulianDay[i]>=61&datedf$JulianDay[i]<=167|datedf$JulianDay[i]>=214&datedf$JulianDay[i]<=335," ","Sample not within sampling window")
    }else{
      # Normal Years
      ifelse(datedf$JulianDay[i]>=60&datedf$JulianDay[i]<=166|datedf$JulianDay[i]>=213&datedf$JulianDay[i]<=334," ","Sample not within sampling window")}
    }
  dfInCorrectFormat <- cbind(dfInCorrectFormat,Comments)
    
  # split input dataframe by samples, return list of dataframes
  splits <-split(dfInCorrectFormat,dfInCorrectFormat$SampleName,drop=T)%>% 
    lapply(function(x) x[!(names(x) %in% c('SampleName'))]) # remove SampleName column, housekeeping
  
  # establish blank dataframe to store data
  result <- data.frame(SampleName=NA,Date=NA,Catchment=NA,nominalTier=NA,nominalMembership=NA,secondMembership=NA,runnerupTier=NA
                       ,close=NA,GISComments=NA,TaxaComments=NA,SamplingWindowComments=NA)
  #### Get attribute data in
  att <- readRDS('data/bug_att.RDS')
  
    
  
  for(i in 1:length(splits)){ #loop through each dataframe in the list of dataframes and do:
    splits[[i]] <- join(splits[[i]],att,by=c('FinalID'))
    # basic housekeeping
    print(i)
    samplename <- names(splits)[i]
    sampleathand <- data.frame(splits[[i]])
    
    # Taxa list QA, check to make sure no NA's for attributes after joined to correct Subbasin
    comment1 <- if(sum(is.na(sampleathand$attLevel))>0){
      NArows <- filter(sampleathand, is.na(attLevel))
      commonNames <- paste(as.character(NArows$CommonName),sep=', ',collapse=', ')
      paste(commonNames,'not attributed in the',NArows$Subbasin,'Subbasin',sep=' ')[1]
    }else(' ') #no taxa attribute list problems
    
    # Transfer site location and Sampling window comments
    siteComment <- unique(splits[[i]]$SiteLocationComment)
    windowComment <- unique(splits[[i]]$SamplingWindowComment)
    
    # Run Model
    result <- rbind(result,cbind(Date=splits[[i]]$Date[1],
                                 Catchment=splits[[i]]$Catchment[1],
                                 Bug_Model(samplename,sampleathand),
                                 GISComments=siteComment,TaxaComments=comment1,SamplingWindowComments=windowComment))
    result<-filter(result,!(SampleName=='NA'))}
  return(result)  
}

