# code to check if species present in database
check_species <- function(sp){
  g <- species$common_name[agrep(sp, species$common_name)]
  if(!sp %in% g){
    if(length(g) == 0 & grepl(" ", sp)){
      g2 <- str_extract(sp, "[A-Za-z]+$")
      g3 <- species$common_name[agrep(g2, species$common_name)]
      cat("Species not found in racebase. Closest species are:\n", paste(g3, "\n"))
    } else {
    cat("Species not found in racebase. Closest species are:\n", paste(g, "\n"))
    }
  } else {
    cat("Species found in database!\n")
  }
}

