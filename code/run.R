

# ~~~~~~~~~~~~~~~~~~ KNOWNS/TOGGLES ~~~~~~~~~~~~~~~~~~~~~ #

# Toggle to skip downloading Oracle tables already present on your local computer. This will save time but will only use cached versions of the tables, which may not be up to date
use_cached <- TRUE


# Toggle to auto save plot to output folder
save_plot <- FALSE


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #



## Load packages -----------------------------------------------------------
pkg <- c("tidyverse", "RODBC", "here", "janitor", "cowplot", "getPass", "mgcv")
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
if(!use_cached | !file.exists("data/oracle")){
  if (file.exists("Z:/Projects/ConnectToOracle.R")) {
    source("Z:/Projects/ConnectToOracle.R")
  } else {
    gapindex::get_connected()
  }
  source("code/00_download_data.R")
}




## Download data  -------------------------------------------------------

# download all relevant tables from Oracle to local computer
source("code/01_clean_data.R") # final output are a lengths table and a cruise_haul table




## Plot data for species  -------------------------------------------------------
# function to plot species (either common or scientific name), can add arguments lat, long, depth, length, or weight; entering any of these values will display them on the associated output plots for easy comparisons to database info. for that species
plot_species("sebastes aleutianus")


# function to check length-weight relationships for a species, can enter arguments length and/or weight. If either length or weight is not designated function will predict the other value and plot.
plot_length_weight("pacific cod", length = 1130)
