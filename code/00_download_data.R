## Download data sets to local machine -------------------------------------------------------

# RACEBASE tables to query
locations <- c(
  ## General Tables of data (racebase)
  "RACEBASE.HAUL",
  # "RACEBASE.LENGTH", #DO NOT USE THIS
  "RACEBASE.SPECIMEN",
  "RACEBASE.SPECIES",
  "RACEBASE.CATCH",
  "RACEBASE.SPECIES_CLASSIFICATION",
  ## Race Data tables
  "RACE_DATA.V_EXTRACT_FINAL_LENGTHS",
  "RACE_DATA.RACE_SPECIES_CODES",
  "RACE_DATA.V_CRUISES"
)





if (!file.exists("data/oracle")) {
  message("\nData not found in folder. Re-downloading tables from Oracle.")
  use_cached <- FALSE 
  dir.create("data/oracle", recursive = TRUE)
}

# what files are already on local machine?
if (use_cached) {
  # checking for local files so no redundant time consuming downloads
  local_files <- str_extract(list.files("data/oracle/", recursive = TRUE), "[^/]*(?=\\.)")
} else {
  local_files <- NULL
}


if(!all(tolower(gsub("\\.", "-", locations)) %in% local_files) | is.null(local_files)){
  if(!exists("channel")){
    cat("Tables required from racebase. Connect to Oracle and re-run this script.\n")
    source("code/ConnectToOracle.R")
  }
  for (i in 1:length(locations)) {
    print(locations[i])
    filename <- tolower(gsub("\\.", "-", locations[i]))
    if (filename %in% local_files) {
      cat(paste0(locations[i], " already present locally. Skipping download.\n\n"))
    } else {
      a <- RODBC::sqlQuery(channel, paste0("SELECT * FROM ", locations[i]))
      write_csv(
        x = a,
        here("data", "oracle", paste0(filename, ".csv"))
      )
      remove(a)
    }
  }
}
