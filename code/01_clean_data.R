# To see how Emily does this: https://github.com/EmilyMarkowitz-NOAA/gap_bs_data_report/blob/main/code/data.R


## Get RACEBASE data -----------------------------------------------
#if(specific_query == FALSE){
  # This local folder contains csv files of all the  tables.
  a <- list.files(
    path = here::here("data", "oracle"),
    pattern = "\\.csv"
  )
  
  for (i in 1:length(a)) {
    suppressWarnings(b <- read_csv(file = here::here("data", "oracle", a[i]), show_col_types = FALSE))
    b <- janitor::clean_names(b)
    if (names(b)[1] %in% "x1") {
      b$x1 <- NULL
    }
    b$database <- stringr::str_extract(a[i], "[^*]+(?=-)")
    assign(x = paste0(stringr::str_extract(a[i], "[^-]*(?=\\.)"), "0"), value = b)
    rm(b)
  }
#}



## Data wrangling -----------------------------------------------

# collecting all species information, broadest possible dataset, needs to be cleaned more for use
species <- species0 %>%
  dplyr::left_join(species_classification0, by = "species_code") %>%
  dplyr::mutate(species_name = case_when(   
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
  dplyr::select(species_code, species_name, common_name, taxon,
    family = family_taxon, order = order_taxon,
    class = class_taxon, phylum = phylum_taxon
  )


## Cleaning cruise info from RACE.DATA
# AFAIK (Megsie) you can only get the name of the survey from the cruises.csv file, which is from RACEDATA
survey_def_ids <- c("AI" = 52, "GOA" = 47, "EBS" = 98, 
                    "BSS" = 78, "NBS" = 143)



cruise <- readr::read_csv("data/oracle/race_data-v_cruises.csv", show_col_types = FALSE) %>%
  janitor::clean_names() %>%
  dplyr::filter(year >= 2000 & survey_definition_id %in% survey_def_ids) %>%
  dplyr::select(year, survey_definition_id, cruisejoin, region, cruise, cruise_id, vessel_id)



specimen <- specimen0 %>%
  dplyr::left_join(species, by = "species_code") %>%
  dplyr::mutate(common_name = tolower(common_name)) %>%
  dplyr::mutate(sex_new = case_when(
    sex == 1 ~ "male",
    sex == 2 ~ "female",
    sex == 3 ~ "unid"
  )) %>%
  dplyr::right_join(cruise, by = join_by(cruisejoin, region, cruise)) %>% 
  dplyr::select(cruisejoin:cruise, year, specimenid, species_code, length, 
                sex = sex_new, weight, age, species_name:phylum) 



haul <- haul0 %>%
  janitor::clean_names() %>%
  dplyr::filter(abundance_haul == "Y" & performance >= 0) %>%
  dplyr::select(cruisejoin:haul, start_latitude, start_longitude, bottom_depth)


catch <- catch0 %>%
  janitor::clean_names() %>%
  dplyr::mutate(avg_weight = (weight/number_fish) * 1000) %>%
  dplyr::select(cruisejoin:species_code, avg_weight) %>%
  dplyr::right_join(cruise, by = join_by(cruisejoin, region, cruise)) %>%
  dplyr::right_join(haul, by = join_by(cruisejoin, hauljoin, region, vessel, cruise, haul))


# combining haul & cruise information, has all abiotic info + locations
cruise_haul_all <- catch %>% 
  dplyr::left_join(v_extract_final_lengths0, by = join_by(region, vessel, cruise, haul, species_code)) %>%
  dplyr::left_join(species, by = "species_code") %>%
  dplyr::select(species_name:class, cruisejoin:start_longitude, year, 
         species_code, length, weight = avg_weight, cruise, bottom_depth ) 

