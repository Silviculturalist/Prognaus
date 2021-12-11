#' Krajicek's Crown competition factor (1961) as implemented in PROGNAUS
#'
#'@description Calculates the crown competition factor
#'
#'@source Krajicek, J. E., Brinkmann, K. E., and Gingrich, S. F. (1961): Crown Competition - a Measure of Density. For. Sci. 7, 35-42.
#'
#'@details
#'Calculates the crown competition factor according to Krajicek et al. (1961)
#'using the open-grown tree diameter functions developed
#'by Hasenauer (1997) [Prognaus::Hasenauer_1997_crown_diameter_open_grown_Austria()].
#'
#'Circular crown areas are assumed
#'and multiplied by the represented stem number (e.g. in fixed area plots or
#'angle count samples), e.g. [Prognaus::influence_zone()]
#'@inheritParams influence_zone
#'@inheritParams Hasenauer_1997_crown_diameter_open_grown_Austria
#'@param area Area of plot/stand in m^2. Default = 100 m^2.

Krajicek_1961_crown_competition_factor_PROGNAUS<-function(species, diameter_cm, nrep=1, log_correction=0){

  #Calculate Potential Crown diameters for all trees.
  openly_grown_tree_crown_diameters <- Hasenauer_1997_crown_diameter_open_grown_Austria(species = species, diameter_cm = diameter_cm,log_correction = log_correction)

  #Calculate corresponding influence zone for each tree.
  influence_zone <- influence_zone(crown_diameter = openly_grown_tree_crown_diameters,nrep = nrep)

  #Total crown area
  return(
  sum(influence_zone)/area
  )

}
