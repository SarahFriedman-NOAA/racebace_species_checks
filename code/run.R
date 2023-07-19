

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
source("code/ConnectToOracle.R")




## Download data  -------------------------------------------------------

if(!exists("cruise_haul_all")){
  # download all relevant tables from Oracle to local computer
  source("code/00_download_data.R")
  source("code/01_clean_data.R") # final output are a lengths table and a cruise_haul table
}




## Plot data for species  -------------------------------------------------------
sp <- "redstripe rockfish"


# function ensures species is actually found in database
check_species(sp)


cruise_haul <- cruise_haul_all %>%
  dplyr::filter(tolower(common_name) == tolower(sp)) 


# can add filters to sp_data to look at specific years, stations, stratum, etc.
sp_data <- specimen %>%
  dplyr::filter(common_name == tolower(sp)) %>%
  full_join(cruise_haul) 




## plots data

# entering values here will display them on the associated output plots for easy comparisons to database info. for that species
# lat <- 58.109
# long <- -167.123
# depth <- 30
# weight <- 1.2

source("code/02_plot_data.R")

