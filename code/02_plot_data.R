# creating species-specific data
if(any(grepl(sp, species$common_name, ignore.case = TRUE))){

cruise_haul <- cruise_haul_all %>%
  dplyr::filter(grepl(sp, common_name, ignore.case = TRUE)) 

# can add filters to sp_data to look at specific years, stations, stratum, etc.
sp_data <- specimen %>%
  dplyr::filter(grepl(sp, common_name, ignore.case = TRUE)) %>%
  right_join(cruise_haul) 

} else {
  if(any(grepl(sp, species$species_name, ignore.case = TRUE))){
    cruise_haul <- cruise_haul_all %>%
      dplyr::filter(grepl(sp, species_name, ignore.case = TRUE)) 
    
    # can add filters to sp_data to look at specific years, stations, stratum, etc.
    sp_data <- specimen %>%
      dplyr::filter(grepl(sp, species_name, ignore.case = TRUE)) %>%
      right_join(cruise_haul) 
  } else {
    stop("No data selected. Spelling of species name may be incorrect.")
  }
}



# data to check against, should be input in "knowns" section of run file
check_data <- tibble(
  common_name = tolower(sp),
  length = size,
  bottom_depth = depth
) %>%
  pivot_longer(cols = length:bottom_depth, names_to = "var", values_to = "val") %>%
  mutate(val = as.numeric(val))


# if(nrow(sp_data) == 0){
#   nm <- species$common_name[agrep(sp, species$common_name)]
#   nm <- paste0(nm, collpase = "\n")
#   stop("No data selected. Spelling of species name may be incorrect. Similar names that are present:\n", nm)
# }





# randomly subsampling for plotting large data sets
if(nrow(sp_data) > 5e6){
  cat("Plotting only a subsample of data due to size. \n")
  tmp <- sp_data %>%
    sample_n(2e6, replace = FALSE)
} else {
  tmp <- sp_data 
}


plot_data <- tmp %>%
  select(region, length, bottom_depth, year) %>%
  pivot_longer(cols = c(length:bottom_depth), names_to = "var", values_to = "val")


p <- ggplot(plot_data, aes(x = val)) +
  geom_density(aes(fill = region, col = region), alpha = 0.6) +
  facet_wrap(~var, scales = "free", strip.position = "bottom") +
  theme_classic() +
  theme(
    axis.title = element_blank(),
    strip.background = element_blank(),
    strip.placement = "outside"
  ) +
  ggtitle(str_to_title(sp)) +
  geom_vline(data = check_data, aes(xintercept = val), col = "black", size = 0.6)



## species occurrence and plotting map data
sp_occ <- sp_data %>%
  dplyr::select(species_name, lat = start_latitude, 
                lon = start_longitude, region, year) %>%
  mutate(lon = ifelse(lon < 0, lon, lon*-1),
         lon = 360+lon) %>% #fixing mistake long
  unique()


if(!is.na(long) & counter == 0){
  long <- ifelse(long > 0, 360-long, 360+long)
  counter <- counter + 1
}


world <- map_data('world2', wrap=c(40,400)) %>%
     filter(region %in% c("Russia", "USA", "Canada"))


m <- ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group),
               col = "grey60", fill = "grey90", lwd = 0) + 
  coord_map(ylim = c(45, 70), xlim = c(150, 250)) +
  theme_bw() +
  labs( x = "Longitude", y = "Latitude" ) +
  geom_point(data = sp_occ, aes(x = lon, y = lat, col = year), cex = 1) +
  geom_point(data = tibble(lat = as.numeric(lat), long = as.numeric(long)),
             aes(x = long, y = lat), col = "red", cex = 1.5) 



# plotting everything together
pp <- plot_grid(p, m, nrow = 2)
print(pp)



if (save_plot) {
  ti <- paste0("plots/", sp_data$family[1], "_", tolower(sp), "_", Sys.Date(), ".pdf")
  ggsave(pp, file = gsub(" ", "_", ti))
}




# # #length vs. weight table
# tmp <- specimen %>%
#   dplyr::filter(common_name == tolower(sp)) %>%
#   select(species_name, length, weight, sex) %>%
#   dplyr::filter(!is.na(length) & !is.na(weight))
# 
# tmp %>%
#   ggplot(aes(x = length/10, y = weight/1000, group = sex, col = sex)) +
#   geom_point(alpha = 0.4) +
#   xlab("length (cm)") + ylab("weight (kg)") +
#   geom_smooth(method = "gam", col = "black", se = FALSE, lwd = 1.5) +
#   theme_classic()