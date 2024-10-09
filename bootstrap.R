# Script information ------------------------------------------------------
# The script automates the generation of `DATA.bib` and `SOFTWARE.bib` files,
# which contain metadata about the data and software used for an SS3 model, 
# based on files in the `boot/initial` directory. It employs the `draft.data()` 
# and `draft.software()` functions to create these files, and organizes the data
# and software into the `boot/data` and `boot/software` directories using `taf.bootstrap()`. 
# Finally, it commits and pushes these changes to the Git repository, 
# ensuring proper version control of the generated files. 

library("icesTAF")
library("r4ss")
#taf.skeleton()
# -boot
#   - initial
#      - data
# -data.R
# -model.R
# -output.R
# -report.R

# crea DATA.bib
# SS3 scenarios
 draft.data(
   originator = "WKBANSP",
   year = 2024,
   title = "SS3 data format",
  period = "1989-2023",file=TRUE)
taf.bootstrap() # se obtiene los datos de la carpeta boot/data

# crea SOFTWARE.bib
dir<-"boot/initial/software"
#r4ss::get_ss3_exe("boot/initial/software", version = "v3.30.22.1")
draft.software(c('boot/initial/software/ss3','boot/initial/software/ss3_linux'),file=TRUE)
taf.bootstrap() # se obtiene los datos de la carpeta boot/data, 


