#' Crown ratio model Prognaus
#'
#'@description Calculates the crown ratio from tree and site variables. Dynamic crown ratio models predict the
#'change in crown ratio. The model was developed by
#'Hasenauer & Monserud (1996). Coefficients are taken from Hasenauer (2000).
#'
#'@source Hasenauer, H. and Monserud, R. A. (1996): A Crown Model for Austrian Forests. For. Ecol. and Manage. 84, 49-60.
#'@source Hasenauer, H. (2000): Die simultanen Eigenschaften von Waldwachstumsmodellen. Paul Parey Verlag. Berlin, 131 pp.
#'
#'@details
#'@param species Tree species
#'@param diameter_cm Diameter at breast height in cm
#'@param height_m Tree height in m
#'@param BAL Basal area of larger trees (Wykoff, 1990)
#'@param crown_competition_factor Crown competition factor (Krajicek, 1961), e.g. [Prognaus::crown_competition_factor()]
#'@param altitude Elevation in m (Range: 200-2200 m)
#'@param incline_percent Slope in percent
#'@param aspect_degree Aspect in degree
#'
#'@examples
#'Hausenauer_1996_crown_ratio_Austria(species="Picea abies",diameter_cm=30,height_m=30,BAL=0,crown_competition_factor=200)

Hausenauer_1996_crown_ratio_Austria <-function(species=1, diameter_cm, height_m, BAL, crown_competition_factor,
               altitude=600, incline_percent=50, aspect_degree=180){

  #Checks.
  stopifnot(altitude >= 200 & altitude <= 2200)
  stopifnot(aspect_degree>=0 & aspect_degree<=360)
  stopifnot(incline_percent>=0)



  #Throw a warning for other deciduous species but Quercus and F. sylvatica to make clear.
  if(tree_type(species)=="Deciduous" & species!="Fagus sylvatica" & grep("^[A-Z]", unlist(strsplit(species, " ")), value = TRUE)!="Quercus"){
    warning("Using general values for a broadleaf which is not Oak or European Beech")
  }

  #stop if an unsupported Conifer
  if(tree_type!="Deciduous" & !(species%in%c("Picea abies","Abies alba","Larix decidua","Pinus sylvestris","Pinus nigra","Pinus cembra"))){
    stop("Unsupported Conifer! Supported conifers are: Picea abies, Abies alba, Larix decidua, Pinus sylvestris, Pinus nigra, Pinus cembra")
  }

  #Berechnet das Kronenmodel mit den Koeffizienten von Hasenauer 2000
  if (species=="Picea abies") {#Fichte
    a0 = -3.893748
    b1 = 0.011232
    b2 = 0.02687
    b3 = -0.6358 / 10000
    c1 = 0.010574
    c2 = 0.404436
    d1 = -0.043123
    d2 = 0
    d3 = 0
    d4 = -0.222507
    d5 = -0.040207
    d6 = 0.147378
  }

  if (species=="Abies alba") {#Tanne
    a0 = -2.364763
    b1 = 0.007637
    b2 = 0.022854
    b3 = -0.74049 / 10000
    c1 = 0.010184
    c2 = 0.270049
    d1 = -0.030867
    d2 = 0
    d3 = 0
    d4 = -0.555051
    d5 = -0.030725
    d6 = 0.158594
  }

  if (species=="Larix decidua") {#Lärche
    a0 = -2.859456
    b1 = 0.009106
    b2 = 0.027547
    b3 = 0 / 10000
    c1 = 0.009133
    c2 = 0.373768
    d1 = -0.037037
    d2 = 0
    d3 = -0.251112
    d4 = 0
    d5 = 0.060256
    d6 = 0.106756
  }

  if (species=="Pinus sylvestris") {#Kiefer
    a0 = -1.676984
    b1 = 0.002942
    b2 = 0.026387
    b3 = 0 / 10000
    c1 = 0.013054
    c2 = 0.245749
    d1 = 0
    d2 = -0.001494
    d3 = 0
    d4 = -0.413472
    d5 = 0.079776
    d6 = 0.167692
  }

  if (species=="Pinus nigra") {#Schwarzkiefer
    a0 = -2.262052
    b1 = 0
    b2 = 0.019348
    b3 = 0 / 10000
    c1 = 0.013737
    c2 = 0.449099
    d1 = 0
    d2 = -0.012183
    d3 = 0
    d4 = 0.412069
    d5 = 0.257688
    d6 = -0.135794
  }

  if (species=="Pinus cembra") {#Zirbe
    a0 = -5.601587
    b1 = 0.018602
    b2 = 0.031416
    b3 = 0 / 10000
    c1 = 0
    c2 = 0.674139
    d1 = 0
    d2 = 0
    d3 = 0
    d4 = 0
    d5 = 0
    d6 = 0
  }

  if (species=="Fagus sylvatica") {#Buche
    a0 = -3.853739
    b1 = 0.007435
    b2 = 0.024684
    b3 = -1.35686 / 10000
    c1 = 0
    c2 = 0.384344
    d1 = 0.015155
    d2 = 0
    d3 = 0.144669
    d4 = 0
    d5 = 0
    d6 = 0
  }

  if(grep("^[A-Z]", unlist(strsplit(species, " ")), value = TRUE)=="Quercus"){#Eiche
    a0 = -1.985395
    b1 = 0.005525
    b2 = 0.013867
    b3 = -1.10735 / 10000
    c1 = 0.006856
    c2 = 0.201014
    d1 = 0
    d2 = 0
    d3 = 0
    d4 = 0
    d5 = 0
    d6 = 0
  }

  #All broadleaves but F. sylvatica and Quercus...
  if (tree_type(species)="Deciduous" & !(species%in%c("Fagus")) & !(grep("^[A-Z]", unlist(strsplit(species, " ")), value = TRUE)=="Quercus")) {
    a0 = -1.995385
    b1 = 0.00649
    b2 = 0.019637
    b3 = -1.2235 / 10000
    c1 = 0.003633
    c2 = 0.158546
    d1 = 0
    d2 = 0
    d3 = 0
    d4 = 0.17509
    d5 = 0.015639
    d6 = 0.074927
  }


  Expo_rad  <-  (aspect_degree / 180) * pi #Umrechnung in Radiant
  if(crown_competition_factor==0){
    crown_competition_factor<-1
  }

  return(
    Crown_ratio = 1 / (1 + exp(a0 + b1 * (100 * height_m / diameter_cm) + b2 * height_m + b3 * diameter_cm ^ 2+
                                 c1 * BAL + c2 * log(crown_competition_factor) + d1 * altitude / 100 + d2 * (altitude / 100) ^ 2 +
                                 d3 * incline_percent / 100 + d4 * (incline_percent / 100) ^ 2 +
                                 d5 * (incline_percent / 100) * sin(Expo_rad) + d6 * (incline_percent / 100) * cos(Expo_rad)))

  )


}

Hausenauer_1996_crown_ratio_Austria<-Vectorize(Hausenauer_1996_crown_ratio_Austria)
