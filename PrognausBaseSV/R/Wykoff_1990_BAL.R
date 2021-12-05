#'@title Basal area of larger trees
#'
#'@description
#'Calculates the basal area of larger trees according to Wykoff 1990.
#'
#'@param diameter_cm Diameter of tree at breast height in cm.
#'@param basal_area_m2_ha Basal area of tree, m2/ha. (Or the BA which it represents).
#'
#'@return A vector of the Basal area of the larger trees for each tree.

Wykoff_1990_BAL <- function(
  diameter_cm,
  basal_area_m2_ha
){
  #Assign a temporary ID and sort by diameter_cm.
  dat <- data.frame("ID"=1:length(diameter_cm),"Diameter"=diameter_cm,"BA"=basal_area_m2_ha)
  temp <- dat[order(dat$Diameter,decreasing=TRUE),]

  #prior-to-cumulative sum of the larger trees' BA.
  temp$BAL <- cumsum(temp$basal_area_m2_ha)-temp$basal_area_m2_ha

  #merge for the right order and return vector.
  return(
    merge(dat,temp)["BAL"]
    )

}
