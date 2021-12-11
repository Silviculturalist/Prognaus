#'@title Open-grown tree crown area
#'
#'@description Calculate the represented open-grown tree crown area
#'in m²
#'
#'@export
#'
#'@details
#'Calculates the represented open-grown crown area from an open-grown tree
#'diameter, e.g. [Prognaus::Hasenauer_1997_crown_diameter_open_grown_Austria()]
#'assuming circular crown areas and multiplies
#'them by the represented stem number (e.g. in fixed area plots or
#'angle count samples)
#'@param crown_diameter Diameter of an open-grown tree in meter
#'@param nrep represented stem number, default=1, data is not from
#'a sample, i.e. each tree represents 1 tree per hectare
#'
influence_zone<-function(crown_diameter, nrep=1){
  #Berechnet eine kreisförmige Einflusszone um den Durchmesser
  #Calculates a circular zone of influence around the diameter.
  SR_m = crown_diameter / 2

  return(
    (SR_m^2) * pi * nrep
  )
}

