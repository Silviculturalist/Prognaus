#' Diameter increment model Prognaus
#'
#'@description Helper function to [Prognaus::Monserud_1996_BA_increment_Austria()].
#' Transforms output to the diameter increment in cm/period.
#'
#'@inheritParams Monserud_1996_BA_increment_Austria
#'
#'@details
#'Calculates the diameter increment of Prognaus. The model calls the
#'basal area increment function of Prognaus, e.g. [Prognaus::Monserud_1996_BA_increment_Austria()] and converts it to diameter
#'increment.
#'
#'@export
#'
#'@examples
#'Vospernik_diameter_increment_PROGNAUS(species="Picea abies",diameter_cm=30,crown_ratio = 0.5,BAL=0,crown_competition_factor=200)
#'
#'dbh2012<-30
#'dbh2015<-dbh2012+Vospernik_diameter_increment_PROGNAUS(species="Picea abies",diameter_cm=30,crown_ratio = 0.5,BAL=0,crown_competition_factor=200,period=3)

Vospernik_diameter_increment_PROGNAUS <-function(species="Picea abies", diameter_cm, crown_ratio, BAL, crown_competition_factor,
               altitude=600, incline_percent=50, aspect_degree=180,
               Humus_F_cm=3, Humus_H_cm=2, soil_depth_cm=50,
               soil_moisture=3, slope_position=2, soil_type=9, vegetation=1,
               ANFI_region=13, log_correction=0, period=5){

  BAI <- Prognaus::Monserud_1996_BA_increment_Austria(
    species = species,
    diameter_cm = diameter_cm,
    crown_ratio = crown_ratio,
    BAL = BAL,
    crown_competition_factor = crown_competition_factor,
    altitude = altitude,
    incline_percent = incline_percent,
    aspect_degree = aspect_degree,
    Humus_F_cm = Humus_F_cm,
    Humus_H_cm = Humus_H_cm,
    soil_depth_cm = soil_depth_cm,
    soil_moisture = soil_moisture,
    slope_position = slope_position,
    soil_type = soil_type,
    vegetation = vegetation,
    ANFI_region = ANFI_region,
    log_correction = log_correction,
    period = period
  )

  BHD2_cm<-sqrt(diameter_cm^2+BAI*4/pi) #New diameter from diameter cm2 BA increment

  #Increment from start.
  return(
    (BHD2_cm-diameter_cm)
  )

}

Vospernik_diameter_increment_PROGNAUS <-Vectorize(Vospernik_diameter_increment_PROGNAUS)
