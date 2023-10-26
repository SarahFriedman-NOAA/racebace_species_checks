

# ~~~~~~~~~~~~~~~~~~ KNOWNS/TOGGLES ~~~~~~~~~~~~~~~~~~~~~ #

# Toggle to skip downloading Oracle tables already present on your local computer. This will save time but will only use cached versions of the tables, which may not be up to date
use_cached <- TRUE


# Toggle to auto save plot to output folder
save_plot <- FALSE


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #



## Load packages -----------------------------------------------------------
pkg <- c("tidyverse", "RODBC", "here", "janitor", "cowplot", "getPass")
for (p in pkg) {
  if (!require(p, character.only = TRUE)) {
    install.packages(p)
    require(p, character.only = TRUE)
  }
}
rm(p, pkg)



source("code/functions.R")




## Connect to Oracle -------------------------------------------------------
# only need to run this once per session
#source("C:/Users/sarah.friedman/Work/Rfunctions/ConnectToOracle_STF.R")
if(!use_cached){
  source("code/ConnectToOracle.R")
  source("code/00_download_data.R")
}




## Download data  -------------------------------------------------------

# download all relevant tables from Oracle to local computer
source("code/01_clean_data.R") # final output are a lengths table and a cruise_haul table




## Plot data for species  -------------------------------------------------------
sp <- "aleutian skate"


# function ensures species is actually found in database
sp <- check_species(sp)


# entering values here will display them on the associated output plots for easy comparisons to database info. for that species
# lat <- 52
# long <- -170
# depth <- 30
# length <- 740
# weight <- 3040

# plot species data
source("code/02_plot_data.R")

