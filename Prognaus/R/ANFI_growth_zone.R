#' Growth Zones for Austria (ANFI)
#'
#' @description Returns the Austrian growth zone which the coordinate is placed
#' in. If there is no match, growth zone will be 'Other', and a warning will be shown.
#'
#' @param latitude Latitude, decimal
#' @param longitude Longitude, decimal
#' @param epsg EPSG for latitude and longitude (Default=4326).
#'
#' @return Text, Austrian growth zone.
#' @export

Austria_growth_zone <- function(
  latitude,
  longitude,
  epsg=4326
){
  if(missing(latitude) | missing(longitude)) stop("Cannot calculate county, requires Latitude & Longitude.")

  #Create a simple features point and provide it with a CRS.
  point <- sf::st_sfc(sf::st_point(x=c(longitude,latitude)))
  sf::st_crs(point) <- epsg

  #Reproject to WGS84 if not already to match CRS for wuchsbezirk stored in internal data.
  if(epsg!=4326){
    assign(x="point",value=sf::st_transform(point,crs=4326))
  }

  area <- Prognaus:::wuchsbezirk$Wuchsnam[sf::st_within(point, Prognaus:::wuchsbezirk)[[1]]]

  if(identical(area,character(0))){
    warning("Outside known Growth Zones.")
    return(
      "Other"
    )
  } else {
    return(
      area
    )
  }

}

Vectorize(Austria_growth_zone)
