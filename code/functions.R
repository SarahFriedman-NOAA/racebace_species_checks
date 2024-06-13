# code to check if species present in database
check_species <- function(sp) {
  ## Ensuring species found in database
  g <- species$common_name[agrep(sp, species$common_name, ignore.case = TRUE)]
  s <- species$species_name[agrep(sp, species$species_name, ignore.case = TRUE)]

  if (length(g) == 0 & length(s) != 0) g <- s

  if (length(g) == 0 || !any(tolower(g) == tolower(sp))) {
    if (length(g) == 0 & grepl(" ", sp)) {
      g2 <- str_extract(sp, "[A-Za-z]+$")
      g <- species$common_name[agrep(g2, species$common_name, ignore.case = TRUE)]
    }

    message("\n\n")
    g <- data.frame(species = g) %>%
      dplyr::arrange(species)
    rownames(g) <- NULL
    if (nrow(g > 0)) {
      print(g)
      message("\nSpecies not found in racebase. Here are the closest species found.\n
            Enter rownumber of taxon:\n")
      take <- scan(n = 1, quiet = TRUE, what = "raw")

      if (length(take) == 0) {
        stop("\nNo species selected.\n")
      }
      if (take %in% seq_len(NROW(g))) {
        take <- as.numeric(take)
        message(
          "Input accepted, took taxon '",
          as.character(g$species[take]), "'.\n"
        )
        sp <- g$species[take]
      }
    } else {
      cat("No similar species found in database. Check spelling.\n")
    }
  }
  return(sp)
}


to_vouch <- function(x) {
  this_year <- as.numeric(format(Sys.Date(), "%Y"))
  x <- x %>%
    dplyr::count(year) %>%
    dplyr::filter(year > (this_year - 10)) %>%
    tidyr::complete(year = seq(this_year - 10, this_year, by = 1), fill = list(n = 0)) %>%
    dplyr::mutate(freq = mean(n) / 10)
  if (x$freq[[1]] > 0.05) "" else "voucher recommended"
}


plot_species <- function(sp, lat = NA, long = NA, depth = NA, length = NA, weight = NA) {
  # function ensures species is actually found in database
  sp <- check_species(sp)


  # creating species-specific data
  if (any(grepl(sp, species$common_name, ignore.case = TRUE))) {
    cruise_haul <- cruise_haul_all %>%
      dplyr::filter(grepl(sp, common_name, ignore.case = TRUE))

    # can add filters to sp_data to look at specific years, stations, stratum, etc.
    sp_data <- specimen %>%
      dplyr::select(-length) %>%
      dplyr::filter(grepl(sp, common_name, ignore.case = TRUE)) %>%
      dplyr::right_join(cruise_haul,
        relationship = "many-to-many",
        by = join_by(
          cruisejoin, hauljoin, region, vessel, cruise,
          species_code, species_name, common_name, taxon,
          family, order, class
        )
      )
  } else {
    if (any(grepl(sp, species$species_name, ignore.case = TRUE))) {
      cruise_haul <- cruise_haul_all %>%
        dplyr::filter(grepl(sp, species_name, ignore.case = TRUE))

      # can add filters to sp_data to look at specific years, stations, stratum, etc.
      sp_data <- specimen %>%
        dplyr::select(-length) %>%
        dplyr::filter(grepl(sp, species_name, ignore.case = TRUE)) %>%
        dplyr::right_join(cruise_haul,
          relationship = "many-to-many",
          by = join_by(
            cruisejoin, hauljoin, region, vessel, cruise,
            species_code, species_name, common_name, taxon,
            family, order, class
          )
        )
    } else {
      stop("No data selected. Spelling of species name may be incorrect.")
    }
  }

  # data to check against, should be input in "knowns" section of run file
  check_data <- tibble::tibble(
    common_name = tolower(sp),
    length_mm = length,
    weight_kg = weight,
    bottom_depth = depth
  ) %>%
    tidyr::pivot_longer(cols = length_mm:bottom_depth, names_to = "var", values_to = "val") %>%
    dplyr::mutate(val = as.numeric(val))



  # randomly subsampling for plotting large data sets
  if (nrow(sp_data) > 5e6) {
    cat("Plotting only a subsample of data due to size. \n")
    tmp <- sp_data %>%
      sample_n(2e6, replace = FALSE)
  } else {
    tmp <- sp_data
  }

  plot_data <- tmp %>%
    dplyr::select(region, year, length_mm = length, depth_m = bottom_depth, weight_kg = weight) %>%
    dplyr::mutate(weight_kg = weight_kg / 1000,
                  length_mm = length_mm) %>%
    tidyr::pivot_longer(cols = c(length_mm:weight_kg), names_to = "var", values_to = "val") %>%
    dplyr::filter(complete.cases(.))



  ## species occurrence and plotting map data
  sp_occ <- sp_data %>%
    dplyr::select(species_name,
      lat = start_latitude,
      lon = start_longitude, region, year
    ) %>%
    dplyr::mutate(lon = ifelse(lon < 0, 360 + lon, lon)) %>%
    # mutate(lon = ifelse(lon < 0, lon, lon*-1),
    #        lon = 360+lon) %>% #fixing mistake long
    unique()



  p <- ggplot2::ggplot(plot_data, aes(x = val)) +
    ggplot2::geom_density(aes(fill = region, col = region), alpha = 0.6) +
    ggplot2::facet_wrap(~var, scales = "free", strip.position = "bottom") +
    ggplot2::theme_classic() +
    ggplot2::theme(
      axis.title = element_blank(),
      strip.background = element_blank(),
      strip.placement = "outside"
    ) +
    ggplot2::ggtitle(label = str_to_sentence(sp), subtitle = to_vouch(sp_occ)) +
    ggplot2::geom_vline(
      data = dplyr::filter(check_data, var %in% plot_data$var),
      aes(xintercept = val), col = "red", linewidth = 0.6
    )


  if (!is.na(long)) {
    # long <- ifelse(long < 0, abs(long), long)
    long <- ifelse(long < 0, 360 + long, long)
  }


  world <- ggplot2::map_data("world2", wrap = c(40, 400)) %>%
    dplyr::filter(region %in% c("Russia", "USA", "Canada"))


  m <- ggplot2::ggplot() +
    ggplot2::geom_polygon(
      data = world, aes(x = long, y = lat, group = group),
      col = "grey60", fill = "grey90", lwd = 0
    ) +
    ggplot2::coord_map(ylim = c(47, 70), xlim = c(165, 235)) +
    ggplot2::theme_bw() +
    ggplot2::labs(x = "Longitude", y = "Latitude") +
    ggplot2::geom_point(data = sp_occ, aes(x = lon, y = lat, col = year), cex = 1) +
    ggplot2::geom_point(
      data = tibble(lat = as.numeric(lat), long = as.numeric(long)),
      aes(x = long, y = lat), col = "red", cex = 1.5
    )


  if (save_plot) {
    ti <- paste0("plots/", sp_data$family[1], "_", tolower(sp), "_", Sys.Date(), ".pdf")
    ggplot2::ggsave(pp, file = gsub(" ", "_", ti))
  }

  # plotting everything together
  pp <- suppressWarnings(cowplot::plot_grid(p, m, nrow = 2))
  return(pp)
}




# length vs. weight plot and predicting values
plot_length_weight <- function(sp, length = NA, weight = NA) {
  # function ensures species is actually found in database
  sp <- check_species(sp)

  if (any(grepl(sp, species$common_name, ignore.case = TRUE))) {
    tmp <- specimen %>%
      dplyr::filter(grepl(sp, common_name, ignore.case = TRUE)) %>%
      dplyr::filter(cruise %in% catch$cruise) %>%
      dplyr::select(species_name, length, weight, sex) %>%
      dplyr::mutate(weight = weight/1000,
                    length = length) %>%
      dplyr::filter(complete.cases(.)) %>%
      unique()
  }

  if (any(grepl(sp, species$species_name, ignore.case = TRUE))) {
    tmp <- specimen %>%
      dplyr::filter(grepl(sp, species_name, ignore.case = TRUE)) %>%
      dplyr::filter(cruise %in% catch$cruise) %>%
      dplyr::select(species_name, length, weight, sex) %>%
      dplyr::mutate(weight = weight / 1000,
                    length = length) %>%
      dplyr::filter(complete.cases(.)) %>%
      unique()
  }

  if (nrow(tmp) < 5) stop("Not enough length/weight data to complete request. Try a different species.")

  p <- ggplot2::ggplot(tmp, aes(x = length, y = weight)) +
    ggplot2::geom_point(alpha = 0.4, col = "grey80") +
    ggplot2::xlab("length (cm)") +
    ggplot2::ylab("weight (kg)") +
    ggplot2::geom_smooth(method = "gam", col = "black", se = FALSE, lwd = 1) +
    ggplot2::theme_classic() +
    ggplot2::ggtitle(str_to_sentence(sp))


  # get predicted values based on GAM
  mod <- mgcv::gam(weight ~ s(length, bs = "cs", fx = TRUE, k = 10), data = tmp)

  # expected length and weight based on values
  if (!is.na(weight) & is.na(length)) {
    pred_length <- stats::approx(x = mod$fitted.values, y = tmp$length, xout = weight)$y
    cat(paste("Predicted length based on weight is", round(pred_length, 0), "mm\n"))
    p <- p +
      ggplot2::geom_point(
        data = tibble(pred_length, weight),
        aes(x = pred_length, y = weight), col = "red", cex = 2.5
      )
  }
  if (is.na(weight) & !is.na(length)) {
    pred_weight <- stats::predict(mod, data.frame(length = length))[[1]]
    cat(paste("Predicted weight based on length is", round(pred_weight, 2), "kg\n"))
    p <- p +
      ggplot2::geom_point(
        data = tibble(length, pred_weight),
        aes(x = length, y = pred_weight), col = "red", cex = 2.5
      )
  }

  if (!is.na(length) & !is.na(weight)) {
    p <- p +
      ggplot2::geom_point(
        data = tibble(length, weight),
        aes(x = length, y = weight), col = "blue", cex = 2.5
      )
  }
  suppressMessages(print(p))
}
