# To see how Emily does this: https://github.com/EmilyMarkowitz-NOAA/gap_bs_data_report/blob/main/code/data.R


## Get RACEBASE data -----------------------------------------------
#if(specific_query == FALSE){
  # This local folder contains csv files of all the  tables.
  a <- list.files(
    path = here::here("data", "oracle"),
    pattern = "\\.csv"
  )
  
  for (i in 1:length(a)) {
    b <- read_csv(file = here::here("data", "oracle", a[i]))
    b <- janitor::clean_names(b)
    if (names(b)[1] %in% "x1") {
      b$x1 <- NULL
    }
    b$database <- str_extract(a[i], "[^*]+(?=-)")
    assign(x = paste0(str_extract(a[i], "[^-]*(?=\\.)"), "0"), value = b)
    rm(b)
  }
#}



## Data wrangling -----------------------------------------------

# collecting all species information, broadest possible dataset, needs to be cleaned more for use
species <- species0 %>%
  dplyr::left_join(species_classification0, by = "species_code") %>%
  mutate(species_name = case_when(   
    species_name == "Mallotus catervarius (=villosus)" ~ "Mallotus villosus",
    species_name == "Poromitra curilensis (=crassiceps)" ~ "Poromitra crassiceps",
    species_name == "Antherinopsis californiensis" ~ "Atherinopsis californiensis",
    species_name == "Rhamphocottus richardsoni" ~ "Rhamphocottus richardsonii",
    species_name == "Lycodema barbatum" ~ "Lyconema barbatum",
    species_name == "Percis japonicus" ~ "Percis japonica",
    grepl("(adult)", species_name) ~ gsub(" \\(adult\\)", "", species_name),
    TRUE ~ species_name
  )) %>%
  dplyr::mutate(taxon = dplyr::case_when(
    species_code <= 31550 ~ "fish",
    species_code >= 40001 ~ "invert",
    class_taxon == "Mammalia" ~ "mammal"
  )) %>%
  select(species_code, species_name, common_name, taxon,
    family = family_taxon, order = order_taxon,
    class = class_taxon, phylum = phylum_taxon
  )


# making a clean lengths data frame with taxonomic information and up-to-date length info
# length <- v_extract_final_lengths0 %>%
#   mutate(length_type = case_when(
#     length_type == 1 ~ "fork length",
#     length_type == 2 ~ "mideye to fork length",
#     length_type == 3 ~ "standard length",
#     length_type == 4 ~ "mideye to hypural plate",
#     length_type == 5 ~ "total length",
#     length_type == 6 ~ "snout to second dorsal",
#     length_type == 7 ~ "carapace from back of right eye socket to end of carapace",
#     length_type == 8 ~ "carapace width",
#     length_type == 9 ~ "head length",
#     length_type == 11 ~ "pre-anal length",
#     length_type == 12 ~ "mantle length",
#     length_type == 13 ~ "posterior of orbital to end of telson",
#     length_type == 14 ~ "wingtip to wingtip",
#     length_type == 15 ~ "outer tip of rostrum to end of telson",
#     length_type == 16 ~ "modal length",
#     length_type == 17 ~ "Length frequency estimated using size composition proportions from adjacent hauls with similar catch composition"
#   )) %>%
#   left_join(species, by = "species_code") %>% 
#   dplyr::select(
#     cruise, haul, region, species_name, taxon,
#     length, species_code, common_name, family:phylum
#   ) %>%
#   mutate(common_name = tolower(common_name))


specimen <- specimen0 %>%
  left_join(species, by = "species_code") %>%
  mutate(common_name = tolower(common_name)) %>%
  mutate(sex_new = case_when(
    sex == 1 ~ "male",
    sex == 2 ~ "female",
    sex == 3 ~ "unid"
  )) %>%
  dplyr::select(cruisejoin:cruise, specimenid, species_code, 
                length, sex = sex_new, weight, age, species_name:phylum) 



## Cleaning cruise info from RACE.DATA
# AFAIK (Megsie) you can only get the name of the survey from the cruises.csv file, which is from RACEDATA
cruise <- v_cruises0 %>%
  dplyr::select(
    cruise_id, year, vessel_id, cruise, start_date,
    end_date, cruisejoin
  ) %>%
  dplyr::filter(year >= 2000 & year != 2020) # no cruises happened in 2020 (covid)



# ok, now I need to incorporate the catch data
catch <- catch0 %>%
  select(-c(subsample_code:database), -vessel) %>%
  rename(total_weight = weight) 



# standard filters for haul data set among RACE folks, dropping rows with missing coordinates
haul <- haul0 %>%
  dplyr::filter(performance >= 0 & # only "good" hauls
    haul_type == 3 & # only standard bottom trawl
    abundance_haul == "Y" & !is.null(stationid)) %>% # hauls used in abundance estimates
  dplyr::select(
    cruisejoin, hauljoin, region, gear_depth,
    bottom_depth, start_latitude, start_longitude, start_time, stationid, stratum
  )


# combining haul & cruise information, has all abiotic info + locations
cruise_haul_all <- left_join(haul, cruise, by = c("cruisejoin")) %>%
  full_join(catch) %>% 
  left_join(species, by = "species_code") %>%
  select(species_name:class,cruisejoin:start_longitude, year, species_code, cruise, stationid, stratum)


lat <- long <- depth <- size <- weight <- NA
counter <- 0