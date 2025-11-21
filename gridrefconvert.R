library(sf)
library(tidyverse)
#' @param prefix A string vector of 2-letter OS grid prefixes (e.g., "NY", "SD").
#' @return A data frame with 'easting_offset' and 'northing_offset' in meters.
get_100km_offset <- function(prefix) {
  offsets <- data.frame(
    prefix = c("NY", "SD", "SE", "NZ", "NA", "NB", "NC", "ND", "NF", "SH", "SK"),
    easting_offset = c(300000, 300000, 400000, 400000, 100000, 200000, 300000, 400000, 0, 200000, 400000),
    northing_offset = c(500000, 400000, 400000, 500000, 500000, 500000, 500000, 500000, 900000, 300000, 300000)
  )
  
  # Match the input prefix with the lookup table
  match_index <- match(toupper(prefix), offsets$prefix)
  
  if (any(is.na(match_index))) {
    warning("One or more map prefixes were unrecognized. Check the input.")
  }
  
  return(data.frame(
    easting_offset = offsets$easting_offset[match_index],
    northing_offset = offsets$northing_offset[match_index]
  ))
}


#' Converts a 6-figure Ordnance Survey (OS) Grid Reference to Latitude and Longitude (WGS84).
#' @param os_grid_ref A character vector of OS grid reference strings (e.g., "NY215072").
#' @return A data frame with two columns: 'Latitude' and 'Longitude' (Decimal Degrees).
os_to_latlon <- function(os_grid_ref) {
  
  
  if (!is.character(os_grid_ref)) {
    stop("Input must be a character vector representing the OS grid reference (e.g., 'NY215072').")
  }
  
  
  ref_clean <- toupper(gsub("[[:space:]]", "", os_grid_ref))
  
  num_chars <- nchar(ref_clean)
  
 
  if (any(!num_chars %in% c(8, 10, 12))) {
    stop("Expected a standard 6, 8, or 10-figure OS grid reference (8, 10, or 12 characters total).")
  }
  
  
  map_prefix <- substr(ref_clean, 1, 2)
  numeric_part <- substr(ref_clean, 3, num_chars)
  
 
  num_numeric_chars <- nchar(numeric_part)
  num_digits <- num_numeric_chars / 2 # Should be 3, 4, or 5
  
 
  scale_factor <- 10^(5 - num_digits)
  
  easting_part <- as.numeric(substr(numeric_part, 1, num_digits))
  northing_part <- as.numeric(substr(numeric_part, num_digits + 1, num_numeric_chars))
    offsets <- get_100km_offset(map_prefix)

  easting_offset_m <- easting_part * scale_factor
  northing_offset_m <- northing_part * scale_factor
  
  full_eastings <- offsets$easting_offset + easting_offset_m
  full_northings <- offsets$northing_offset + northing_offset_m
  
  coords_bng <- st_point(cbind(full_eastings, full_northings)) %>%
    st_sfc(crs = 27700)

  coords_wgs84 <- st_transform(coords_bng, crs = 4326)

  lat_lon_matrix <- st_coordinates(coords_wgs84)

  results <- data.frame(
    latitude = lat_lon_matrix[, "Y"],
    longitude = lat_lon_matrix[, "X"]
  )
  
  return(results)
}
wainwrights=googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1asnafeRHRpv9Ddf3HVg6q9Lr0cn3hevj4c2aq2hqcZc/edit?gid=0#gid=0")

out=tribble(~"latitude", ~"longtitude")
for(i in 1:nrow(wainwrights)){
  out=out %>% rbind.data.frame(os_to_latlon(wainwrights$os[i]))
}
wainwrights$latitude=out$latitude
wainwrights$longitude=out$longitude
googlesheets4::write_sheet(wainwrights, ss="https://docs.google.com/spreadsheets/d/1asnafeRHRpv9Ddf3HVg6q9Lr0cn3hevj4c2aq2hqcZc/edit?gid=0#gid=0", sheet = "wainwrights")
