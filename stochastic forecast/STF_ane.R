# Script information ------------------------------------------------------

# Authors: 

# Date: 2024

# Load libraries --------------------------------------------------------------
rm(list=ls())

library(TAF)
library(r4ss)
library(parallel)
library(doParallel)
library(foreach)
library(tidyverse)


icesTAF::mkdir("model/STF")
icesTAF::mkdir("model/bpr")
esc<-"S2"
mkdir(paste0("model/STF/",esc))
mkdir(paste0("model/STF/",esc,"/Forecast_Files"))

ESC_model<-"S1.0_sigmaR_0.33_h0.8_selP10_qPela1_forecast"
#number of years (-1) over which to average weights, selectivity
Naver = 2  # most recent 3-years

model<-file.path("model","run",ESC_model)
fore<-file.path( "model","STF",esc, "forecast.ss")
#Create scenarios
file.copy(file.path("model","run",ESC_model, "forecast.ss"),
          file.path( "model","STF",esc, "forecast.ss"))

#read in the forecast file
fore <- r4ss::SS_readforecast(file = file.path("model", "STF",esc, "forecast.ss"),verbose = FALSE)

#run assessment
# wd <- model
# system(wd)
# system(paste0("chmod 755 ",wd,"/ss3_linux"))
# r4ss::run(dir=model, exe="ss3_linux", skipfinished=FALSE, show_in_console =T)


#read in assessment ouput
replist <- SS_output(dir = model, verbose=TRUE, printstats=TRUE) ## read
stdreptlist<-data.frame(replist$derived_quants[,1:3])

# Define the range of years to include
start_year <- 1989
end_year <- 2023

# Define a function to process data
process_data <- function(data, pattern, value_col, stddev_col = NULL) {
  # Ensure the required column exists
  if (!"Label" %in% colnames(data)) {
    stop("The 'Label' column is missing in the data frame.")
  }
  
  filtered_data <- data %>%
    filter(grepl(pattern, Label)) %>%
    mutate(year = as.numeric(sub(paste0(pattern, "_"), "", Label))) %>%
    filter(!is.na(year) & year >= start_year & year <= end_year) %>%
    select(year, Value, StdDev)
  
  # Remove StdDev column if not needed
  if (is.null(stddev_col)) {
    filtered_data <- filtered_data %>% select(-StdDev)
  }
  
  return(filtered_data)
}

# Process 'summary' data
process_summary_data <- function(data, pattern, value_col) {
  if (!"V1" %in% colnames(data)) {
    stop("The 'V1' column is missing in the summary data frame.")
  }
  
  filtered_data <- data %>%
    filter(grepl(pattern, V1)) %>%
    mutate(year = as.numeric(sub(paste0(pattern, "_"), "", V1))) %>%
    filter(!is.na(year) & year >= start_year & year <= end_year) %>%
    select(year, value_col)
  
  return(filtered_data)
}

# Apply the function to each dataset
ssb <- process_data(stdreptlist, "SSB", "Value", "StdDev")
ssb$type<-"SSB"
recr <- process_data(stdreptlist, "Recr", "Value", "StdDev")
recr$type<-"Rt"
ft <- process_data(stdreptlist, "F", "Value", "StdDev")
ft$type<-"Ft"

# Encontrar el valor mínimo y el año correspondiente en el rango filtrado
min_value <- min(ssb$Value)
min_sd <-ssb$StdDev[which.min(ssb$Value)]
min_year <- ssb$year[which.min(ssb$Value)]
last_year<-end_year
last_value<-ssb$Value[ssb$year==last_year]
last_sd<-ssb$StdDev[ssb$year==last_year]
sigma<-sqrt(log(1+(last_sd/last_value)^2)) # sigma  is the estimated standard deviation of ln(SSB) in the last year of the assessment, accounting for the uncertainty in SSB for the terminal year. 

#'*=============================================================================*
# Reference Points ----
#'*=============================================================================*
Blim <- min_value
Bpa <-exp(1.645*sigma)*Blim
Flim <- NA
Fp05 <- NA
Fpa <- NA
Fmsy <- NA
MSYBtrigger <- NA

save(min_value,min_sd,min_year,
     last_year,last_value,last_sd,
     sigma,Blim,Bpa, 
     ssb,ft,
     file=paste0("model/bpr/bpr_",ESC_model,".Rdata"))

# Plot biological reference points ----
#data<-rbind(ssb,ft)
# data<-rbind(ssb)
# data <- data %>%
#   mutate(
#     Value = as.numeric(Value),
#     StdDev = as.numeric(StdDev)) %>%
#   mutate(
#     lower = case_when(
#       is.na(StdDev) ~ 0,
#       TRUE ~ Value - 1.96 * StdDev),
#     upper = case_when(
#       is.na(StdDev) ~ 0,
#       TRUE ~ Value + 1.96 * StdDev),
#     CV=case_when(
#       is.na(StdDev) ~ 0,
#       TRUE ~ StdDev/ Value))
# 
# hline_data <- data.frame(
#   yintercept = c(Blim, Bpa),
#   type = c("SSB", "SSB"),
#   Line = c("Blim", "Bpa")
# )

# figbpr<-ggplot(data, aes(x = year, y = Value)) +
#   geom_line() +
#   geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
#   facet_wrap(.~type, scales = "free", ncol = 1, strip.position = "top",
#              labeller = labeller(type = c("SSB" = "SSB", 
#                                           "Ft" = "F apical"))) +
#   labs(x = "", y = "", title = "") +
#   theme_bw() +
#   theme(plot.title = element_text(hjust = 0.5), legend.position = "top") +
#   geom_hline(data = hline_data, aes(yintercept = yintercept, color = Line, linetype = Line), size = 1) +
#   scale_color_manual(name = "Biological Reference points", values = c("Blim" = "red", "Bpa" = "blue")) +
#   scale_linetype_manual(name = "Biological Reference points", values = c("Blim" = "dashed", "Bpa" = "dotted")) +
#   
#   theme(legend.position = "top", legend.title = element_text(hjust = 0.5))+
#   theme(plot.margin = unit(c(0, 0.1, 0, 0), "cm"))
# ggsave(file.path(paste0("model/bpr/","/fig_bpr_",ESC_model,".png")), figbpr,  width=6, height=4)

#'*=============================================================================*
# Short-term forecast ----
#'*=============================================================================*
# #exploitation
dat <- replist$exploitation
dat <- dat[-c(3,4,5,6)]
 tail(dat)
# 
# #### mean years
Nfor <- fore$Nforecastyrs
 startyear <- max(dat$Yr) - Nfor - Naver
 endyear <- max(dat$Yr) - Nfor
 year_inter <- endyear+1
# 
# # SET Fsq i) average of the last Nfor+1 years across seasons and fleets ii)last year
data <- subset(dat,dat$Yr>=startyear & dat$Yr<=endyear)

 Fsqmean <- aggregate(.~Seas,data=data,mean) # i) use mean 4 year in F for STF 
# 
 Fsq1 <- data$SEINE_Q1[data$Yr==endyear]   #  ii) use last year
 Fsq2 <- data$SEINE_Q2[data$Yr==endyear] # ii) use last year
 Fsq3 <- data$SEINE_Q3[data$Yr==endyear] #  ii) use last year
 Fsq4 <- data$SEINE_Q4[data$Yr==endyear] #  ii) use last year

 
## prepare intermediate year data 
dat <- replist$exploitation
# keep year, seas and fleets
dat <- dat %>% select(-Seas_dur, -F_std, -annual_F, -annual_M)
# head(dat) 



## average of the last 3 years across seasons and fleets
startyear <- max(dat$Yr)-Nfor-Naver+1
endyear   <- max(dat$Yr)-Nfor
data_int <- dat %>% filter(Yr>=startyear & Yr<=endyear) %>%
  select(-Yr) %>% group_by(Seas) %>%
  summarise_all(mean)

## input intermediate year data
nfleet=4
dimen <- dim(data_int)
Year  <- rep(endyear+1,dimen[1]*(dimen[2]-1))
fore_dat_int       <- data.frame(Year)
fore_dat_int$Seas  <- data_int$Seas
fore_dat_int$Fleet <- 1:nfleet#rep(1:nfleet, each = length(data_int$Seas))
#fore_dat_int$F     <- c(Fsq1,Fsq2,Fsq3,Fsq4) #as.vector(as.matrix(Fsqmean[,-which(names(Fsqmean)=="Seas")]))
fore_dat_int$F     <- as.vector(as.matrix(Fsqmean[,-which(names(Fsqmean)==c("Seas","Yr"))]))
fore_dat_int<-fore_dat_int[fore_dat_int$F != 0, ]
fore_dat_int
#####------------
###  Using apical F multipliers are exact for Fsq multipliers 
vector0 <- c(0,1, 1*1.2, 1*1.6, 1*2)


## search for multipliers for BRP

# Search fishing mortality multipliers
# fsq=Fsq
# a=Fmsy/fsq
# b=Fpa/fsq
# c=Flim/fsq
# # s1=20
# # s2=20
# # s3=20
# # FMult <- c(seq(0,a,length.out=s1),seq(a+0.01,b,length.out=s1),seq(b+0.01,c+0.05,length.out=s3))

### Fmultipliers with Beverton-Holt
start_value1 <- 2.293984423
end_value1 <- 2.300912438
vector1 <- seq(from = start_value1, to = end_value1, length.out = 5)
# Define the start and end values
start_value2 <- 2.911111111
end_value2 <- 2.925
vector2 <- seq(from = start_value2, to = end_value2, length.out = 5)
# Define the start and end values
start_value3 <- 3.147222222
end_value3 <- 3.161111111
vector3 <- seq(from = start_value3, to = end_value3, length.out = 5)

#FMult <- c(vector0, vector1, vector2,vector3)
FMult <- c(vector0)

FMult_names <- paste0("FMult",FMult)
l_FMult <- length(FMult)

##### ----- SET GEOMEAN Recruitment (use or not depends on uncertainty)
# Get assessment outputs for geomean recruitment
year_inter <- endyear+1
ass.sum <- SSsummarize(SSgetoutput(dirvec=model))
hist.rec <- as.data.frame(ass.sum$recruits) %>% filter(Yr %in% 2021:(year_inter-1)) %>% .[,1]  # all series 2021-2023
virg.rec <- as.data.frame(ass.sum$recruits) %>% filter(Label == "Recr_Virgin") %>% .[,1]

gmrec <- exp(mean(log(hist.rec)))
##### -----

aux=fore_dat_int # F last year

### CREATE forecast.ss with interim year F
 for (i in 1:l_FMult){
 #fore_dat=fore_dat_int;
  aux_fore=fore_dat_int
   #for(j in 1:(Nfor-1)){
   j<-1
     aux_fore$Year=endyear+j
     aux_fore$F=FMult[i]*aux$F
     fore_dat=aux_fore
     for(j in 2:(Nfor-1)){ 
       aux_fore$Year=endyear+j
       aux_fore$F=FMult[i]*aux$F
      # fore_dat=aux_fore
     fore_dat=rbind(fore_dat,aux_fore)
   }
  j=Nfor
  aux_fore$Year=endyear+j
  aux_fore$F=FMult[i]*aux$F
  fore_dat=rbind(fore_dat,aux_fore)
 
  # input ------------------------------------------------------------------------
  fore$InputBasis<-99 # 99 for F, 2 for Catch
  fore$ForeCatch<-fore_dat # input ForeCatch(orF) data
  fore$fcast_rec_option <- 0 #= value*(virgin recruitment) # comment lines to use model BH # how to replace last year assessment!?
  fore$fcast_rec_val <- 1#gmrec/virg.rec # geomean / virgin rec # comment lines to use model BH
  
  ## write all forecast files/scenarios
  r4ss::SS_writeforecast(fore, dir = file.path("model","STF",esc,"Forecast_Files"), 
                         file = paste0("Forecast",FMult_names[i], ".ss"), 
                         overwrite = TRUE, verbose = FALSE)
}

###### ------- change starter.ss! line 31 -2 # max yr for sdreport outputs (-1 for endyr+1; -2 for endyr+Nforecastyrs) 
# create forecast folder and subfolders
for (i in 1:l_FMult){
  
  dir.FMult <- file.path("model","STF",esc,paste0("FMult",FMult[i]))
  dir.create(path = dir.FMult, showWarnings = F, recursive = T)
  
  file.copy(file.path(model, "starter.ss"), file.path(dir.FMult, "starter.ss"))
  file.copy(paste(model, "control.SS", sep="/"), paste(dir.FMult, "control.SS", sep="/"))
  file.copy(paste(model, "data.SS", sep="/"), paste(dir.FMult, "data.SS", sep="/"))	
  file.copy(paste(model, "ss3_linux", sep="/"), paste(dir.FMult, "ss3_linux", sep="/"))
  file.copy(paste(model, "wtatage.ss", sep="/"), paste(dir.FMult, "wtatage.ss", sep="/"))
  
  #copy the forecast file
  file.copy(paste("model","STF",esc,"Forecast_Files",paste0("ForecastFMult",FMult[i],".ss") , sep="/"),
            paste(dir.FMult, "forecast.ss", sep="/"))
}


### -- run the forecast models
all.models <- c(paste0("FMult", FMult))


for (m in all.models){
wd <- model
system(wd)
system(paste0("chmod 755 ",wd,"/ss3_linux")) 
r4ss::run(dir=file.path("model","STF",esc,m), exe="ss3_linux", skipfinished=FALSE, show_in_console =T)
}
#r4ss::run(dir = file.path("model","STF",m), exe = "ss3_linux", show_in_console = TRUE, skipfinished = FALSE)

#retrieve and summarise outputs
forecastModels <- r4ss::SSgetoutput(dirvec = file.path("model","STF",esc,c(FMult_names)), getcovar = FALSE)

namesrepl<-names(forecastModels)
all.scen <- c(FMult)
num.scen <- length(all.scen)
reference_ssb<-matrix(ncol=num.scen,nrow = 5)
reference_rec<-matrix(ncol=num.scen,nrow = 3)

for(i in 1:num.scen){
SSB_Virgin<- forecastModels[[namesrepl[i]]]$derived_quants[forecastModels[[namesrepl[i]]]$derived_quants$Label == "SSB_Virgin", "Value"]
Recr_Virgin<-forecastModels[[namesrepl[i]]]$derived_quants[forecastModels[[namesrepl[i]]]$derived_quants$Label =="Recr_Virgin","Value"]

SSB_Initial<-forecastModels[[namesrepl[i]]]$derived_quants[forecastModels[[namesrepl[i]]]$derived_quants$Label =="SSB_Initial","Value"]
Recr_Initial<-forecastModels[[namesrepl[i]]]$derived_quants[forecastModels[[namesrepl[i]]]$derived_quants$Label =="Recr_Initial","Value"]

SSB_unfished <-forecastModels[[namesrepl[i]]]$derived_quants[forecastModels[[namesrepl[i]]]$derived_quants$Label =="SSB_unfished","Value"]
Recr_unfished<-forecastModels[[namesrepl[i]]]$derived_quants[forecastModels[[namesrepl[i]]]$derived_quants$Label =="Recr_unfished","Value"]


SSB_Btgt<-forecastModels[[namesrepl[i]]]$derived_quants[forecastModels[[namesrepl[i]]]$derived_quants$Label =="SSB_Btgt","Value"]
SPR_Btgt<-forecastModels[[namesrepl[i]]]$derived_quants[forecastModels[[namesrepl[i]]]$derived_quants$Label =="SPR_Btgt","Value"]

reference_ssb[,i]<-c(SSB_Virgin,SSB_Initial,SSB_unfished,SSB_Btgt,SPR_Btgt)
reference_rec[,i]<-c(Recr_Virgin,Recr_Initial,Recr_unfished)
}

forecastModels[[namesrepl[1]]]$derived_quants

reference_ssb
reference_rec

# forecastModels$replist1$SPAWN_RECR_CURVE
# forecastModels$replist1$managementratiolabels




SS_decision_table_stuff(forecastModels$replist1, yrs = 2023:2026, digits = c(0, 0, 3))

save(forecastModels, file=file.path("model","STF",esc,"STF.Rdata"))

forecastSummary <- r4ss::SSsummarize(forecastModels)
SStableComparisons(
  forecastSummary,
  models = "all",
  likenames = c("TOTAL", "Survey", "Length_comp", "Age_comp", "priors", "Size_at_age"),
  names = c("Recr_Virgin", "R0", "steep", "NatM",   "SSB_Virg"),
  digits = NULL,
  modelnames = "default",
  csv = FALSE,
  csvdir = "workingdirectory",
  csvfile = "parameter_comparison_table.csv",
  verbose = TRUE,
  mcmc = FALSE
)
# check dataframes in SSB, F, Recr
SSB <- as.data.frame(forecastSummary[17])
SSB.SD <- as.data.frame(forecastSummary[18])   #SD for pnormal Blim
SSB.Lower <- as.data.frame(forecastSummary[19])   #SSB + 1.96SD
SSB.Upper <- as.data.frame(forecastSummary[20])   #SSB - 1.96SD
Fvalue <- as.data.frame(forecastSummary[30])
Recr <- as.data.frame(forecastSummary[38])

# Filtrar los datos entre 1989 y 2026
data <- subset(SSB, SpawnBio.Yr >= 1989)

# Crear la gráfica con ggplot2
ggplot(data, aes(x = SpawnBio.Yr, y = SpawnBio.replist1)) +
  geom_line(aes(color = ifelse(SpawnBio.Yr <= 2023, "1989-2023", "2024-2026")), size = 1) +
  scale_color_manual(values = c("1989-2023" = "blue", "2024-2026" = "red")) +
  labs(title = "SSB from 1989 to 2026", x = "Year", y = "SSB") +
  theme_minimal() +
  theme(legend.title = element_blank())

 #revisar recruitment deviation!!!!


## --  WRITE ALL IN A DATAFRAME
all.scen <- c(FMult)
num.scen <- length(all.scen)
dfSTFSummary <- 
  data.frame(Scenario.Type = c(rep("FMult",length(FMult))),
             Val = all.scen,
             SSB_2023 = as.numeric(SSB[SSB$SpawnBio.Yr==2023,paste0('SpawnBio.replist',seq(1,num.scen))]),
             SSB_2023_SD = as.numeric(SSB.SD[SSB.SD$SpawnBioSD.Yr==2023,paste0('SpawnBioSD.replist',seq(1,num.scen))]),
             SSB_2024 = as.numeric(SSB[SSB$SpawnBio.Yr==2024,paste0('SpawnBio.replist',seq(1,num.scen))]),
             SSB_2024_SD = as.numeric(SSB.SD[SSB.SD$SpawnBioSD.Yr==2024,paste0('SpawnBioSD.replist',seq(1,num.scen))]),
             SSB_2025 = as.numeric(SSB[SSB$SpawnBio.Yr==2025,paste0('SpawnBio.replist',seq(1,num.scen))]),
             SSB_2025_SD = as.numeric(SSB.SD[SSB.SD$SpawnBioSD.Yr==2025,paste0('SpawnBioSD.replist',seq(1,num.scen))]),
             SSB_2026 = as.numeric(SSB[SSB$SpawnBio.Yr==2026,paste0('SpawnBio.replist',seq(1,num.scen))]),
             SSB_2026_SD = as.numeric(SSB.SD[SSB.SD$SpawnBioSD.Yr==2026,paste0('SpawnBioSD.replist',seq(1,num.scen))]),
             F_2023 = as.numeric(Fvalue[Fvalue$Fvalue.Yr==2023,paste0('Fvalue.replist',seq(1,num.scen))]),
             F_2024 = as.numeric(Fvalue[Fvalue$Fvalue.Yr==2024,paste0('Fvalue.replist',seq(1,num.scen))]),
             F_2025 = as.numeric(Fvalue[Fvalue$Fvalue.Yr==2025,paste0('Fvalue.replist',seq(1,num.scen))]),
             F_2026 = as.numeric(Fvalue[Fvalue$Fvalue.Yr==2026,paste0('Fvalue.replist',seq(1,num.scen))]),
             Rec_2023 = as.numeric(Recr[Recr$recruits.Yr==2023,paste0('recruits.replist',seq(1,num.scen))]),
             Rec_2024 = as.numeric(Recr[Recr$recruits.Yr==2024,paste0('recruits.replist',seq(1,num.scen))]),
             Rec_2025 = as.numeric(Recr[Recr$recruits.Yr==2025,paste0('recruits.replist',seq(1,num.scen))]),
             Rec_2026 = as.numeric(Recr[Recr$recruits.Yr==2026,paste0('recruits.replist',seq(1,num.scen))]),
             Catch_2023 = NA,
             Catch_2024 = NA, 
             Catch_2025 = NA, 
             Catch_2026 = NA)

#probablility of being below Blim assuming pNormal
dfSTFSummary$pBlim_2023 <- round(pnorm(Blim, dfSTFSummary$SSB_2023, dfSTFSummary$SSB_2023_SD),3)
dfSTFSummary$pBlim_2024 <- round(pnorm(Blim, dfSTFSummary$SSB_2024, dfSTFSummary$SSB_2024_SD),3)
dfSTFSummary$pBlim_2025 <- round(pnorm(Blim, dfSTFSummary$SSB_2025, dfSTFSummary$SSB_2025_SD),3)
dfSTFSummary$pBlim_2026 <- round(pnorm(Blim, dfSTFSummary$SSB_2026, dfSTFSummary$SSB_2026_SD),3)
dfSTFSummary

#catches
for (i in 1:num.scen){
  output = forecastModels[[i]]
  catch <- output$timeseries %>%
    filter(Era == "FORE") %>%
    select("Yr", starts_with("dead(B)")) %>%
    mutate(Total_deadB = `dead(B):_1` + `dead(B):_2` + `dead(B):_3` + `dead(B):_4`) %>%
    mutate(FMult = FMult[i]) %>%
    select(Yr, Total_deadB, FMult) %>%
    group_by(Yr) %>%
    summarise(Sum_Total_deadB = sum(Total_deadB))
  names(catch) <- c("Year","Catch","FMult")
  catch <- catch %>% pivot_wider(names_from = Year, values_from = Catch, names_prefix = "Catch_")
  dfSTFSummary$Catch_2024[i] <- catch$Catch_2024
  dfSTFSummary$Catch_2025[i] <- catch$Catch_2025
  dfSTFSummary$Catch_2026[i] <- catch$Catch_2026
}

dfSTFSummary


save(dfSTFSummary, file=paste0("model/STF/STFSummary",esc,".Rdata"))
# 
write.csv(dfSTFSummary, paste0("model/STF/STFSummary",esc,".csv"))

