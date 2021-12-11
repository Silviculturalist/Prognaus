#' Calculate the height increment for trees as in Prognaus.
#'
#'@source Nachtmann, G. (2005): Der Höhenzuwachs österreichischer Baumarten in Abhängigkeit von Standort und Konkurrenz. Diploma thesis. Boku, University of Natural Resources and Life Sciences. 65 pp.
#'
#'@description Calculates the height increment in m/period as in Prognaus.
#'The model is an Evolon-model, c.f. Mende and Albrecht (2001).
#'Coding of tree species, soil moisture, relief, soil type, vegetation type
#'and growth district uses the coding of the
#'Austrian National Forest Inventory
#'(\href{https://homepage.boku.ac.at/sonja/ANFI_E.pdf}{ANFI}).
#'
#'@param species One of: "Picea abies", "Abies alba", "Larix decidua","Pinus sylvestris","Pinus nigra","Pinus cembra","Fagus sylvatica","Quercus","Fraxinus","Acer".
#'@param height_m Tree height in m
#'@param dominant_height_m Dominant height in m
#'@param BAL Basal area of larger trees (Wykoff, 1990), e.g. [Prognaus::Wykoff_1990_BAL()]
#'@param crown_competition_factor Crown competition factor (Krajicek, 1961), e.g. [Prognaus::crown_competition_factor()]
#'@param altitude Elevation in m (Range: 200-2200 m)
#'@param incline_percent Slope in percent
#'@param aspect_degree Aspect in degree
#'@param soil_depth_cm Soil depth in cm, model uses dummy variable which is 1 for soildepth <=30cm
#'@param soil_moisture ,Type 1="Dry/torr",2="Mesic/frisk",3="Mesic-moist/frisk-fuktig",4="Moist/fuktig",5="Wet/Blöt"
#'@param slope_position ANFI Slope position. Type 1="Upper slope,crest",2="Middle slope",3="Lower slope",4="Toe slope",5="Valley plain",6="Plain",7="Depression".
#'@param soil_type Soil type e.g. [Prognaus::ANFI_soil_codes()]
#'@param ANFI_region Growth district, e.g. [Prognaus::ANFI_growth_zone()]
#'@param period Period length in years, default=5 years, 5 years is maximum
#'recommended time step for the update of BAL and CCF
#'
#'@examples
#'Nachtmann_2005_height_increment_Prognaus_Austria(species="Picea abies",height_m=30,dominant_height_m=30,BAL=0,crown_competition_factor=200)
#'
#'height2012<-30
#'height2015<-height2012+Nachtmann_2005_height_increment_Prognaus_Austria(species="Picea abies",height_m=30,dominant_height_m=30,BAL=0,crown_competition_factor=200,period=3)


Nachtmann_2005_height_increment_Prognaus_Austria <- function(species = "Picea abies",
                                                    height_m,
                                                    dominant_height_m,
                                                    BAL,
                                                    crown_competition_factor,
                                                    altitude = 600,
                                                    incline_percent = 50,
                                                    aspect_degree = 180,
                                                    soil_depth_cm = 50,
                                                    soil_moisture = 3,
                                                    slope_position = 2,
                                                    soil_type = 9,
                                                    ANFI_region = 13,
                                                    period = 5) {

  #Checks.
  stopifnot(altitude >= 200 & altitude <= 2200)
  stopifnot(ANFI_region%in%c(1:21))
  stopifnot(soil_moisture%in%(1:5))
  stopifnot(slope_position%in%c(1:7))
  stopifnot(soil_type%in%c(1:26))
  stopifnot(aspect_degree>=0 & aspect_degree<=360)
  stopifnot(incline_percent>=0)

  #Throw warnings
  if(period>5){
    warning("Period not recommended to be longer than 5 years...")
  }

  if (species == "Picea abies") {
    #Fichte
    b0 = 830.3758
    b1 = -1.48641
    b1a = 0
    b2 = -0.002482722
    b3 = 0.08999699
    b5 = -21.52708
    b7 = 27.66197
    b8 = 55.77791
    b9 = 65.91359
    C = 0.000000000122
    k0 = 0
    k1 = 0.1768697
    k2 = -0.0000494
    k3 = 0
    k4 = 0
    k5 = 0.03018899
    k6 = 0.03984067
    k9 = 0.01550065
    l0 = 3.5
    l1 = 0.06769241
    l2 = 0.07338717
    l3 = 0.03828342
    l5 = 0.007590793
    l6 = -0.006656075
    shopt = 6.535413
    dBoden1 <-
      ifelse(soil_type %in% c(2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 15, 16, 20, 24, 25),
             1,
             0)
    dBoden2 <-  ifelse(soil_type %in% c(13, 14, 22, 23), 1, 0)
    dBoden3 <-  ifelse(soil_type %in% c(18, 19, 21, 26), 1, 0)
    dWuchs1 <-  ifelse(ANFI_region %in% c(4, 5, 8, 16, 17), 1, 0)
    dWuchs2 <-  ifelse(ANFI_region %in% c(1, 3, 10, 18, 19), 1, 0)
  }

  if (species == "Abies alba") {
    #Tanne
    b0 = 576.8744
    b1 = 0
    b1a = -5.865515
    b2 = -0.005925026
    b3 = 0
    b5 = 0
    b7 = -46.06445
    b8 = 0
    b9 = 0
    C = 0.0000377
    k0 = 0.4864953
    k1 = 0
    k2 = 0
    k3 = -0.002183014
    k4 = 0
    k5 = 0
    k6 = 0.05096631
    k9 = 0
    l0 = 1.5
    l1 = 0
    l2 = 0.04532396
    l3 = 0
    l5 = 0
    l6 = 0
    shopt = 0
    dBoden1  <-  ifelse(soil_type %in% c(0), 1, 0)
    dBoden2  <-
      ifelse(soil_type %in% c(2, 3, 4, 5, 8, 10, 13, 14, 15, 16, 19, 20, 22, 23, 24, 25, 26),
             1,
             0)
    dBoden3  <-  ifelse(soil_type %in% c(0), 1, 0)
    dWuchs1  <-  ifelse(ANFI_region %in% c(0), 1, 0)
    dWuchs2  <-  ifelse(ANFI_region %in% c(0), 1, 0)
  }

  if (species == "Larix decidua") {
    #Laerche
    b0 = 827.5023
    b1 = 0
    b1a = -10.055784
    b2 = -0.00494291
    b3 = 0
    b5 = 0
    b7 = -135.91511
    b8 = -113.635
    b9 = -113.24792
    C = 0.000000105
    k0 = 0.4
    k1 = 0
    k2 = 0
    k3 = -0.002518417
    k4 = -0.07928417
    k5 = 0
    k6 = 0
    k9 = 0
    l0 = 2.5
    l1 = 0.0477241
    l2 = 0.03162142
    l3 = 0
    l5 = 0
    l6 = 0
    shopt = 0
    dBoden1  <-  ifelse(soil_type %in% c(6, 10, 12, 13), 1, 0)
    dBoden2  <-  ifelse(soil_type %in% c(2, 3, 4, 9), 1, 0)
    dBoden3  <-  ifelse(soil_type %in% c(0), 1, 0)
    dWuchs1  <-  ifelse(ANFI_region %in% c(0), 1, 0)
    dWuchs2  <-  ifelse(ANFI_region %in% c(0), 1, 0)
  }

  if (species == "Pinus sylvestris") {
    #Weiszkiefer #Pinus sylvestris?
    b0 = 482.4536
    b1 = 0
    b1a = 0
    b2 = -0.013846154
    b3 = 0
    b5 = 0
    b7 = 0
    b8 = 0
    b9 = 0
    C = 0.000000231
    k0 = 0.4
    k1 = 0
    k2 = 0
    k3 = -0.001940466
    k4 = 0
    k5 = 0
    k6 = 0
    k9 = 0
    l0 = 2.5
    l1 = -0.15991975
    l2 = -0.06091851
    l3 = -0.05142054
    l5 = 0.024703279
    l6 = 0
    shopt = 0
    dBoden1  <-  ifelse(soil_type %in% c(11, 17), 1, 0)
    dBoden2  <-  ifelse(soil_type %in% c(18), 1, 0)
    dBoden3  <-  ifelse(soil_type %in% c(6, 19), 1, 0)
    dWuchs1  <-
      ifelse(ANFI_region %in% c(6, 7, 8, 13, 15, 16, 17, 20, 21), 1, 0)
    dWuchs2  <-  ifelse(ANFI_region %in% c(0), 1, 0)
  }

  if (species %in% c("Pinus nigra",
                     "Pinus cembra",
                     "Pinus strobus",
                     "Pseudotsuga menziesii") |
      Prognaus::tree_type(species) == "Coniferous" &
      !(species %in% c(
        "Picea abies",
        "Abies alba",
        "Larix decidua",
        "Pinus sylvestris"
      ))) {
    #Sonstiges Nadelholz: Baumarten 5-9
    b0 = 569.1469
    b1 = 0
    b1a = 0
    b2 = -0.012921057
    b3 = 0
    b5 = 0
    b7 = 0
    b8 = 47.12252
    b9 = 0
    C = 0.0000002272
    k0 = 0.4
    k1 = 0
    k2 = 0
    k3 = -0.002397636
    k4 = 0
    k5 = 0
    k6 = 0
    k9 = 0
    l0 = 2.5
    l1 = 0
    l2 = 0
    l3 = 0
    l5 = 0
    l6 = 0
    shopt = 0
    dBoden1  <-  ifelse(soil_type %in% c(0), 1, 0)
    dBoden2  <-  ifelse(soil_type %in% c(0), 1, 0)
    dBoden3  <-  ifelse(soil_type %in% c(0), 1, 0)
    dWuchs1  <-  ifelse(ANFI_region %in% c(0), 1, 0)
    dWuchs2  <-  ifelse(ANFI_region %in% c(0), 1, 0)
  }

  #Some species specific values, otherwise as other conifers.
  if (species == 5) {
    #Pinus nigra
    b0 <-  b0 - 194.7221
  }
  if (species == 6) {
    #Pinus cembra
    C <-  C  - 0.000000186
  }

  #No additional change for Pinus strobus.

  if (species == 8) {
    #Pseudotsuga menziesii
    b0 <-  b0   - 152.1846
    C <- C   - 0.000000168
  }

  #Assuming below is typo that should not apply to all 'other coniferous'.
  # if(species==9) {#Douglasie
  #   C <- C   - 0.000000168
  # }

  if (species == "Fagus sylvatica") {
    #Fagus sylvatica
    b0 = 645.2093
    b1 = 0
    b1a = 0
    b2 = -0.016237986
    b3 = 0
    b5 = 0
    b7 = -57.34682
    b8 = 0
    b9 = 0
    C = 0.000000126
    k0 = 0.4
    k1 = 0
    k2 = 0
    k3 = -0.003353847
    k4 = 0
    k5 = 0
    k6 = 0
    k9 = 0
    l0 = 2.5
    l1 = -0.02554873
    l2 = 0
    l3 = 0
    l5 = 0.036714137
    l6 = 0
    shopt = 0
    dBoden1  <-
      ifelse(
        soil_type %in% c(
          2,
          3,
          4,
          5,
          6,
          7,
          8,
          10,
          11,
          14,
          15,
          16,
          17,
          18,
          19,
          20,
          21,
          22,
          23,
          24,
          25
        ),
        1,
        0
      )
    dBoden2  <-  ifelse(soil_type %in% c(0), 1, 0)
    dBoden3  <-  ifelse(soil_type %in% c(0), 1, 0)
    dWuchs1  <-  ifelse(ANFI_region %in% c(3, 4, 5, 21), 1, 0)
    dWuchs2  <-  ifelse(ANFI_region %in% c(0), 1, 0)
  }

  #Hardwoods
  if (species %in% c("Carpinus betulus", "Castanea sativa", "Robinia pseudoacacia") |
      (grep("^[A-Z]", unlist(strsplit(species, " ")), value = TRUE)) %in% c("Quercus", "Fraxinus", "Acer", "Ulmus", "Sorbus", "Prunus")) {
    b0 = 639.2017
    b1 = 0
    b1a = 0
    b2 = -0.01108105
    b3 = 0
    b5 = 0
    b7 = -32.03469
    b8 = 0
    b9 = 0
    C = 0.0000000000709
    k0 = 0.4
    k1 = 0
    k2 = 0
    k3 = -0.002119498
    k4 = 0
    k5 = 0
    k6 = 0
    k9 = 0
    l0 = 3.637734
    l1 = 0
    l2 = 0
    l3 = 0
    l5 = 0
    l6 = 0
    shopt = 0
    dBoden1  <-  ifelse(soil_type %in% c(0), 1, 0)
    dBoden2  <-  ifelse(soil_type %in% c(10, 15, 17), 1, 0)
    dBoden3  <-  ifelse(soil_type %in% c(0), 1, 0)
    dWuchs1  <-  ifelse(ANFI_region %in% c(0), 1, 0)
    dWuchs2  <-  ifelse(ANFI_region %in% c(0), 1, 0)
  }

  #Species specific changes Hardwoods..
  if (grep("^[A-Z]", unlist(strsplit(species, " ")), value = TRUE) == "Quercus") {
    #Eiche
    l2 <-  0.0454656
  }
  #Carpinus betulus no change.

  if (grep("^[A-Z]", unlist(strsplit(species, " ")), value = TRUE) == "Fraxinus") {
    #Esche
    b0 <- b0 + 62.57866
    b9 = 36.31986
  }
  if (grep("^[A-Z]", unlist(strsplit(species, " ")), value = TRUE) == "Acer") {
    #Ahorn
    b0 <- b0 + 38.4246
  }
  #Ulmus spp.. no change

  if (species == "Castanea sativa") {
    #Edelkastanie
    b0 <- b0 + 59.97233
  }


  other_broadleaved <-
    ifelse(tree_type(species) == "Deciduous" &
             !(
               species %in% c(
                 "Fagus sylvatica",
                 "Carpinus betulus",
                 "Castanea sativa",
                 "Robinia pseudoacacia"
               )
             ) &
             !(
               grep("^[A-Z]", unlist(strsplit(
                 paste0(species), " "
               )), value = TRUE) %in% c(
                 "Sorbus",
                 "Acer",
                 "Tilia",
                 "Quercus",
                 "Fraxinus",
                 "Ulmus",
                 "Betula",
                 "Alnus",
                 "Populus",
                 "Salix"
               )
             ),
           1,
           0)


  if (species %in% c(
    "Betula pendula",
    "Alnus glutinosa",
    "Alnus incana",
    "Tilia cordata",
    "Tilia platyphyllos"
  ) |
  grep("^[A-Z]", unlist(strsplit(species, " ")), value = TRUE) %in% c("Salix", "Populus") |
  tree_type(species) == "Deciduous" &
  other_broadleaved) {
    #Softwoods, species 20-28.
    b0 = 658.5414
    b1 = 0
    b1a = -5.765316
    b2 = 0
    b3 = 0
    b5 = 0
    b7 = 0
    b8 = 0
    b9 = 0
    C = 0.00000000203
    k0 = 0.4
    k1 = 0
    k2 = 0
    k3 = -0.001424609
    k4 = 0
    k5 = -0.06449457
    k6 = 0
    k9 = 0
    l0 = 3.12
    l1 = 0
    l2 = 0
    l3 = 0
    l5 = 0
    l6 = 0
    shopt = 0
    dBoden1  <-  ifelse(soil_type %in% c(0), 1, 0)
    dBoden2  <-  ifelse(soil_type %in% c(0), 1, 0)
    dBoden3  <-  ifelse(soil_type %in% c(0), 1, 0)
    dWuchs1  <-  ifelse(ANFI_region %in% c(0), 1, 0)
    dWuchs2  <-  ifelse(ANFI_region %in% c(0), 1, 0)
  }

  #Species specific values softwoods
  if (species == "Betula pendula") {
    #Birke
    l0 <-  l0 + 0.0512
  }
  #No change for Alnus or Tilia spp.

  if (species == "Populus tremula") {
    #Aspe
    k0 <-  k0 + 0.09482742
  }
  if (species == "Populus nigra") {
    #Schwarzpappel
    b0 <-  b0 + 151.4617
  }

  #Hybridpappel (?) If not one of the usual, but Populus, then hybrid poplar?
  if (grep("^[A-Z]", unlist(strsplit(species, " ")), value = TRUE) == "Populus" &
      !(species %in% c("Populus tremula", "Populus nigra", "Populus alba"))) {
    b0 <-  b0 + 109.9534
  }


  #No change for Salix spp. or other broadleaves..

  H_dm  <-  height_m * 10 #Umrechnung in dm
  Ho_dm  <-  dominant_height_m * 10  #Oberhöhe in dm
  H_Ho  <-  H_dm / Ho_dm #Verhaeltnis von Hoehe zur Oberhoehe
  altitude  <-  altitude_m / 100 #Umrechnung in hm
  Expo_rad  <-  (aspect_degree / 180) * pi #Umrechnung in Radiant

  #Gründigkeit und Wasserhaushaltsklassen
  SGR <- ifelse(soil_depth_cm <= 30, 1, 0) #seichtgruendig

  TR <- ifelse(soil_moisture == 1, 1, 0) #trocken
  MFR <- ifelse(soil_moisture == 2, 1, 0) #mäßig frisch
  FR <- ifelse(soil_moisture == 3, 1, 0) #frisch
  SFR <- ifelse(soil_moisture == 4, 1, 0) #sehr frisch, hangsickerfeucht
  #Alle Dummyv=0 feucht

  #slope_position
  OH <- ifelse(slope_position == 1, 1, 0)  #Oberhang
  MH <- ifelse(slope_position == 2, 1, 0) #Mittelhang
  UH <- ifelse(slope_position == 3, 1, 0) #Unterhang
  EB <- ifelse(slope_position == 6, 1, 0) #Ebene
  #Alle Dummyv=0 Mulde

  #Evolon-Model
  Kappa = k0 + k1 * H_Ho + k2 * crown_competition_factor + k3 * BAL + k4 * OH + k5 * MH + k6 * UH + k9 * EB
  Lambda = l0 + l1 * dBoden1 + l2 * dBoden2 + l3 * dBoden3 + l5 * dWuchs1 + l6 * dWuchs2
  b = b0 + b1 * (altitude - shopt) ^ 2 + b1a * altitude + b2 * incline_percent ^ 2 + b3 * incline_percent * sin(Expo_rad) +
    b5 * SGR + b7 * MFR + b8 * FR + b9 * SFR

  if (b < H_dm) {
    b <- H_dm
  }

  #Yearly height increment
  ihpa = C * H_dm ^ (Kappa) * (b - H_dm) ^ (Lambda)
  ih = ihpa / 10 #from decimeter to meters.

  return(ih * period)

}

Nachtmann_2005_height_increment_Prognaus_Austria <- Vectorize(Nachtmann_2005_height_increment_Prognaus_Austria)
