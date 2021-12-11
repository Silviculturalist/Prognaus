#' Calculate the Crown Diameter of Open grown trees in Austria following Hasenauer 1997.
#'
#' @source Hasenauer, H. 1997. Dimensional relationships of open-grown trees in Austria. Forest Ecology & Management. Vol 96:3. pp. 197-206. DOI: \url{https://doi.org/10.1016/S0378-1127(97)00057-1}
#'
#' @param species One of: "Picea abies", "Abies alba", "Larix decidua","Pinus sylvestris","Pinus nigra","Pinus cembra","Fagus sylvatica","Quercus","Fraxinus","Acer", or another deciduous tree species.
#' @param diameter_cm Diameter of the tree at breast height, cm.
#' @param log_correction TRUE or FALSE (Default) to apply logarithmic correction.
#'
#' @return Crown diameter.
#' @export

Hasenauer_1997_crown_diameter_open_grown_Austria <- function(
  species,
  diameter_cm,
  log_correction=FALSE
){

  if (species=="Picea abies") {#Fichte Picea abies
    b0 = -0.3232
    b1 = 0.6441
    MSE = 0.026
  }

  if (species=="Abies alba") {#Tanne Abies alba
    b0 = 0.092
    b1 = 0.538
    MSE = 0.03
  }

  if (species=="Larix decidua") {#Lärche Larix decidua
    b0 = -0.3396
    b1 = 0.6823
    MSE = 0.035
  }

  if (species=="Pinus sylvestris") {#Kiefer Pinus sylvestris
    b0 = -0.1797
    b1 = 0.6267
    MSE = 0.037
  }

  if (species=="Pinus nigra") {#Schwarzkiefer Pinus nigra
    b0 = -0.157
    b1 = 0.631
    MSE = 0.019
  }

  if (species=="Pinus cembra") {#Zirbe Pinus cembra
    b0 = -1.3154
    b1 = 0.8288
    MSE = 0.2
  }

  if (species=="Fagus sylvatica") {#Buche Fagus sylvatica
    b0 = 0.2662
    b1 = 0.6072
    MSE = 0.058
  }

  if (species=="Quercus") {#Eiche Quercus
    b0 = -0.3973
    b1 = 0.7328
    MSE = 0.038
  }

  if (species=="Fraxinus") {#Esche Fraxinus
    b0 = 0.1366
    b1 = 0.6183
    MSE = 0.051
  }

  if (species=="Acer") {#Ahorn Acer
    b0 = 0.418
    b1 = 0.5285
    MSE = 0.025
  }

  #Betula pendula  and Deciduous trees which are not included above.
  if ((!(species%in%c("Acer","Fraxinus","Quercus","Fagus sylvatica","Pinus cembra","Pinus nigra","Pinus sylvestris","Larix decidua","Abies alba","Picea abies"))) & tree_type(species)=="Deciduous"){
    b0 = 0.1783
    b1 = 0.5665
    MSE = 0.025
  }

  return(
    exp(b0 + b1 * log(diameter_cm) + (0.5 * MSE * log_correction))
  )
}

Hasenauer_1997_crown_diameter_open_grown_Austria <-Vectorize(Hasenauer_1997_crown_diameter_open_grown_Austria)
