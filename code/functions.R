# code to check if species present in database
check_species <- function(sp){
  
  ## Ensuring species found in database
  g <- species$common_name[agrep(sp, species$common_name, ignore.case = TRUE)]
  s <- species$species_name[agrep(sp, species$species_name, ignore.case = TRUE)]
  
  if(length(g) == 0 & length(s) != 0) g <- s

  if(!grepl(g, sp, ignore.case = TRUE)){
    if(length(g) == 0 & grepl(" ", sp)){
      g2 <- str_extract(sp, "[A-Za-z]+$")
      if(!is.null(s)){
        g <- species$common_name[agrep(g2, species$species_name, ignore.case = TRUE)]
        
      } else {
      g <- species$common_name[agrep(g2, species$common_name, ignore.case = TRUE)]
      }
    } 
      
      message("\n\n")
      g <- data.frame(species = g)
      rownames(g) <- NULL
      if(nrow(g > 0)){
      print(g)
      message("\nSpecies not found in racebase. Here are the closest species found.
            Enter rownumber of taxon (other inputs will return 'NA'):\n")
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

