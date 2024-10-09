# Script information ------------------------------------------------------

# run TAF analysis for Anchovy in ICES Subdivision 9a South.

#Authors: María José Zúñiga (maria.zuniga@ieo.csic.es) 

# Date: 2024


# Load packages -----------------------------------------------------------

library(icesTAF)
library(rmarkdown)

# clean the TAF directories (all except bootstrap/initial):
#clean()

# Run the TAF analysis ----------------------------------------------------

# run step by step

# sourceTAF("bootstrap")
# sourceTAF("data")
# sourceTAF("model")
# sourceTAF("output") 
# sourceTAF("report")
library(icesTAF)
library(rmarkdown)
boot<-"boot/initial/data/run/" 
list.files(boot)
esc<-"S1.0_sigmaR_0.33_h0.8_selP10_qPela1_forecast" 
write(esc, file = paste0(boot,"Esc.txt"))
sourceTAF("bootstrap")
sourceTAF("data")
sourceTAF("model_01_run")

 esc<-"S1.0_sigmaR_0.33_h0.8_selP10_qPela1_forecast" 
 # run.dir  <- paste0("model/run/",esc)
 # output <- r4ss::SS_output(dir = run.dir,forecast=T)


sourceTAF("output_01_run")
sourceTAF("report_01_run")


sourceTAF("model_02_retro")
sourceTAF("output_02_retro")
sourceTAF("report_02_retro")


# run reporte.Rmd 
mkdir("Report_rmd")
  render("Report_SS3_quarter_with_age_data_MR_prueba_mod_2.Rmd", 
         output_file = paste0("Report_rmd/Report_",esc,".pdf"))


#sourceAll()


# End of script -----------------------------------------------------------



