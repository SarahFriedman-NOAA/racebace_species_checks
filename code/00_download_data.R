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


# if (specific_query) {
#   # get species code for query
#   code <- RODBC::sqlQuery(channel, paste0("SELECT *
#                                           FROM RACEBASE.SPECIES
#                                           WHERE(lower(RACEBASE.SPECIES.COMMON_NAME) = '", tolower(sp), "')"))$SPECIES_CODE
#   
#   if(length(code) == 0){
#     stop("Unable to locate species. Check spelling.")
#   }
# 
#   sp_locs <- locations[!str_detect(locations, "HAUL|CRUISES")]
# 
#   for (i in 1:length(sp_locs)) {
#     tmp <- RODBC::sqlQuery(channel, paste0(
#       "SELECT * FROM ", sp_locs[i],
#       " WHERE (", sp_locs[i], ".SPECIES_CODE = ", code, ")"
#     )) %>%
#       janitor::clean_names() %>%
#       as_tibble()
# 
#     if (names(tmp)[1] %in% "x1") {
#       tmp$x1 <- NULL
#     }
# 
#     
#     assign(
#       x = paste0(
#         tolower(str_remove(sp_locs[i], "[^_]+\\.")), "0"
#       ),
#       value = tmp
#     )
#     
#     rm(tmp)
#   }
# 
#   locations <- locations[str_detect(locations, "HAUL|CRUISES")]
# }



if (!file.exists("data/oracle")) {
  message("Data not found in folder. Re-downloading tables from Oracle.")
  source("code/ConnectToOracle.R")
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
