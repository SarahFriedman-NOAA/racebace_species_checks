# code to check if species present in database
check_species <- function(sp){
  
  ## Ensuring species found in database
  g <- species$common_name[agrep(sp, species$common_name, ignore.case = TRUE)]
  s <- species$species_name[agrep(sp, species$species_name, ignore.case = TRUE)]
  
  if(length(g) == 0 & length(s) != 0) g <- s

  if( length(g) == 0 || !any(tolower(g) == tolower(sp)) ){
    if(length(g) == 0 & grepl(" ", sp)){
      g2 <- str_extract(sp, "[A-Za-z]+$")
      g <- species$common_name[agrep(g2, species$common_name, ignore.case = TRUE)]
    } 
      
      message("\n\n")
      g <- data.frame(species = g) %>%
        arrange(species)
      rownames(g) <- NULL
      if(nrow(g > 0)){
      print(g)
      message("\nSpecies not found in racebase. Here are the closest species found.\n
            Enter rownumber of taxon:\n")
      take <- scan(n = 1, quiet = TRUE, what = "raw")
      
      if (length(take) == 0) {
        stop("\nNo species selected.\n")
      }
      if (take %in% seq_len(NROW(g))) {
        take <- as.numeric(take)
        message("Input accepted, took taxon '",
                as.character(g$species[take]), "'.\n")
        sp <-  g$species[take]
      }
      } else { cat("No similar species found in database. Check spelling.")}
      } else {
    message(sp, " found in database!\n")
      }
  return(sp)
}



plot_species <- function(sp, lat = NA, long = NA, depth = NA, weight = NA){
  
  # function ensures species is actually found in database
  sp <- check_species(sp)
  
  
  # creating species-specific data
  if(any(grepl(sp, species$common_name, ignore.case = TRUE))){
    
    cruise_haul <- cruise_haul_all %>%
      dplyr::filter(grepl(sp, common_name, ignore.case = TRUE)) 
    
    # can add filters to sp_data to look at specific years, stations, stratum, etc.
    sp_data <- specimen %>%
      select(-length) %>%
      dplyr::filter(grepl(sp, common_name, ignore.case = TRUE)) %>%
      right_join(cruise_haul, relationship = "many-to-many", 
                 by = join_by(cruisejoin, hauljoin, region, vessel, cruise,
                              species_code, species_name, common_name, taxon, 
                              family, order, class)) 
    
  } else {
    if(any(grepl(sp, species$species_name, ignore.case = TRUE))){
      cruise_haul <- cruise_haul_all %>%
        dplyr::filter(grepl(sp, species_name, ignore.case = TRUE)) 
      
      # can add filters to sp_data to look at specific years, stations, stratum, etc.
      sp_data <- specimen %>%
        select(-length) %>%
        dplyr::filter(grepl(sp, species_name, ignore.case = TRUE)) %>%
        right_join(cruise_haul, relationship = "many-to-many", 
                   by = join_by(cruisejoin, hauljoin, region, vessel, cruise, 
                                species_code, species_name, common_name, taxon, 
                                family, order, class)) 
    } else {
      stop("No data selected. Spelling of species name may be incorrect.")
    }
  }
  
  
  
  # data to check against, should be input in "knowns" section of run file
  check_data <- tibble(
    common_name = tolower(sp),
    length = length,
    weight = weight,
    bottom_depth = depth
  ) %>%
    pivot_longer(cols = length:bottom_depth, names_to = "var", values_to = "val") %>%
    mutate(val = as.numeric(val))
  
  
  
  # randomly subsampling for plotting large data sets
  if(nrow(sp_data) > 5e6){
    cat("Plotting only a subsample of data due to size. \n")
    tmp <- sp_data %>%
      sample_n(2e6, replace = FALSE)
  } else {
    tmp <- sp_data 
  }
  
  
  plot_data <- tmp %>%
    select(region, year, length_mm = length, depth_m = bottom_depth, weight_g = weight) %>%
    pivot_longer(cols = c(length_mm:weight_g), names_to = "var", values_to = "val") %>%
    filter(complete.cases(.))
  
  
  p <- ggplot(plot_data, aes(x = val)) +
    geom_density(aes(fill = region, col = region), alpha = 0.6) +
    facet_wrap(~var, scales = "free", strip.position = "bottom") +
    theme_classic() +
    theme(
      axis.title = element_blank(),
      strip.background = element_blank(),
      strip.placement = "outside"
    ) +
    ggtitle(str_to_sentence(sp)) +
    geom_vline(data = filter(check_data, var %in% plot_data$var),
               aes(xintercept = val), col = "black", linewidth = 0.6)
  
  
  
  ## species occurrence and plotting map data
  sp_occ <- sp_data %>%
    dplyr::select(species_name, lat = start_latitude, 
                  lon = start_longitude, region, year) %>%
    dplyr::mutate(lon = ifelse(lon < 0, 360 + lon, lon)) %>%
    # mutate(lon = ifelse(lon < 0, lon, lon*-1),
    #        lon = 360+lon) %>% #fixing mistake long
    unique()
  
  
  if(!is.na(long)){
    #long <- ifelse(long < 0, abs(long), long)
    long <- ifelse(long < 0, 360 + long, long)
  }
  
  
  world <- map_data('world2', wrap=c(40,400)) %>%
    filter(region %in% c("Russia", "USA", "Canada"))
  
  
  m <- ggplot() +
    geom_polygon(data = world, aes(x = long, y = lat, group = group),
                 col = "grey60", fill = "grey90", lwd = 0) + 
    coord_map(ylim = c(47, 70), xlim = c(165, 235)) +
    theme_bw() +
    labs( x = "Longitude", y = "Latitude" ) +
    geom_point(data = sp_occ, aes(x = lon, y = lat, col = year), cex = 1) +
    geom_point(data = tibble(lat = as.numeric(lat), long = as.numeric(long)),
               aes(x = long, y = lat), col = "red", cex = 1.5) 
  
  
  if (save_plot) {
    ti <- paste0("plots/", sp_data$family[1], "_", tolower(sp), "_", Sys.Date(), ".pdf")
    ggsave(pp, file = gsub(" ", "_", ti))
  }
  
  # plotting everything together
  pp <- suppressWarnings(plot_grid(p, m, nrow = 2))
  return(pp)
  
}




# # length vs. weight plot
# tmp <- specimen %>%
#   dplyr::filter(grepl(sp, common_name, ignore.case = TRUE)) %>%
#   dplyr::filter(cruise %in% catch$cruise) %>%
#   dplyr::select(species_name, length, weight, sex) %>%
#   dplyr::filter(complete.cases(.)) %>%
#   unique()
# 
# ggplot(tmp, aes(x = length, y = weight)) +
#   geom_point(alpha = 0.4, col = "grey80") +
#   xlab("length (mm)") + ylab("weight (g)") +
#   geom_smooth(method = "gam", col = "black", se = FALSE, lwd = 1) +
#   theme_classic() +
#   geom_point(data = tibble(length, weight),
#              aes(x = length, y = weight), col = "red", cex = 2.5) +
#   ggtitle(str_to_sentence(sp))
# 
# # get predicted values based on GAM
# library(mgcv)
# mod <- mgcv::gam(weight ~ s(length, bs = "cs", fx = TRUE, k = 10), data = tmp)
# 
# # expected length and weight based on values
# pred_length <- approx(x = mod$fitted.values, y = tmp$length, xout = weight)$y
# pred_weight <- predict(mod, data.frame(length = length))[[1]] 
