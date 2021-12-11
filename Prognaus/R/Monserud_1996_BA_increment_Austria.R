#' Basal area increment model Prognaus
#'
#'@description Calculates the basal area increment in cm²/period
#'
#'@details
#'Calculates the basal area increment of Prognaus. The model is based
#'on Monserud and Sterba (1996); Coefficients are taken from Hasenauer (2000).
#'
#'@source Hasenauer, H. (2000): Die simultanen Eigenschaften von Waldwachstumsmodellen. Paul Parey Verlag. Berlin, 131 pp.
#'@source Monserud, R. A. and Sterba, H. (1996): A Basal Area Increment Model for Individual Trees Growing in Even- and Unevenaged Forest Stands in Austria. For. Ecol. and Manage. 80, 57-80.
#'
#'@param species One of.. : "Picea abies","Abies alba","Larix decidua","Pinus sylvestris","Pinus nigra","Pinus cembra", "Fagus sylvatica","Quercus", or other deciduous tree.
#'@param diameter_cm Diameter at breast height of tree.
#'@param crown_ratio Crown ratio of tree, e.g. [Prognaus::Hasenauer_1996_crown_ratio_Austria()]
#'@param BAL Basal area of larger trees (Wykoff, 1990), e.g. [Prognaus::Wykoff_1990_BAL()]
#'@param crown_competition_factor Crown competition factor (Krajicek, 1961), e.g.[Prognaus::Krajicek_1961_crown_competition_factor_PROGNAUS()]
#'@param altitude Elevation in m (Range: 200-2200 m)
#'@param incline_percent Slope in percent
#'@param aspect_degree Aspect in degree
#'@param Humus_F_cm Thickness of F-humus layer in cm
#'@param Humus_H_cm Thickness of H-humus layer in cm
#'@param soil_depth_cm Soil depth in cm, model uses dummy variable which is 1 for soil depth <=30cm
#'@param soil_moisture Soil moisture,Type 1="Dry",2="Mesic",3="Mesic-moist",4="Moist",5="Wet"
#'@param slope_position ANFI Slope position. Type 1="Upper slope,crest",2="Middle slope",3="Lower slope",4="Toe slope",5="Valley plain",6="Plain",7="Depression".
#'@param soil_type Soil type, code according to [Prognaus::ANFI_soil_codes()]
#'@param vegetation Vegetation type, code according to [Prognaus::ANFI_vegetation_codes()]
#'@param ANFI_region Growth district, code according to [Prognaus::Austria_growth_zone()]
#'@param log_correction Dummy variable, default=0, if 1 half of the model MSE is
#'added to correct for logarithmic transformation bias
#'@param period Period length in years, default=5 years, 5 years is maximum
#'recommended time step for the update of BAL and crown_competition_factor
#'
#'@export
#'
#'@examples
#'Monserud_1996_BA_increment_Austria(species="Picea abies",diameter_cm=30,crown_ratio = 0.5,BAL=0,crown_competition_factor=200)



Monserud_1996_BA_increment_Austria <-
  function(species = "Picea abies",
           diameter_cm,
           crown_ratio,
           BAL,
           crown_competition_factor,
           altitude = 600,
           incline_percent = 50,
           aspect_degree = 180,
           Humus_F_cm = 3,
           Humus_H_cm = 2,
           soil_depth_cm = 50,
           soil_moisture = 3,
           slope_position = 2,
           soil_type = 9,
           vegetation = 1,
           ANFI_region = 13,
           log_correction = 0,
           period = 5) {
    #Stop if not a supported Conifer
    if (Prognaus::tree_type(species) == "Coniferous" &
        !(
          species %in% c(
            "Picea abies",
            "Abies alba",
            "Larix decidua",
            "Pinus sylvestris",
            "Pinus nigra",
            "Pinus cembra"
          )
        )) {
      stop(
        "Unsupported Conifer. Supported conifers are: Picea abies, Abies alba, Larix decidua, Pinus sylvestris, Pinus nigra, Pinus cembra."
      )
    }

    #Basic checks
    stopifnot(altitude >= 200 & altitude <= 2200)
    stopifnot(ANFI_region %in% c(1:21))
    stopifnot(soil_moisture %in% (1:5))
    stopifnot(slope_position %in% c(1:7))
    stopifnot(soil_type %in% c(1:26))
    stopifnot(aspect_degree >= 0 & aspect_degree <= 360)
    stopifnot(incline_percent >= 0)
    stopifnot(log_correction %in% c(0, 1, TRUE, FALSE))

    #Throw warnings
    if (period > 5) {
      warning("Period not recommended to be longer than 5 years...")
    }



    if (species == "Picea abies") {
      #Fichte
      a0 = 0.07
      b1 = 1.396876
      b2 = -0.000165
      b3 = 0.634179
      c1 = -0.017369
      c2 = -0.000497
      d1 = -0.003824
      d1a = 0
      d2 = -0.186184
      d2a = 0
      d3 = 0.034217
      d4 = -0.076437
      d5 = -0.018425
      d6 = -0.009333
      SD = -0.117116
      sm = 0.122647
      SR = 0
      s1 = 0.410682
      s2 = 0.350527
      s3 = 0.174713
      s4 = 0
      v1 = 0.187571
      v2 = 0.323107
      v3 = 0
      g1 = 0.106262
      g2 = 0.043582
      MSE = 0.559
      X = 6
      dANFI_region1 <-
        ifelse(ANFI_region %in% c(4, 5, 8, 16, 17), 1, 0)
      dANFI_region2 <-
        ifelse(ANFI_region %in% c(1, 3, 10, 18, 19), 1, 0)
      dsoil_type1 <-
        ifelse(soil_type %in% c(2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 15, 16, 20, 24, 25),
               1,
               0)
      dsoil_type2 <-
        ifelse(soil_type %in% c(1, 13, 14, 22, 23), 1, 0)
      dsoil_type3 <- ifelse(soil_type %in% c(18, 19, 21, 26), 1, 0)
      dsoil_type4 <- ifelse(soil_type %in% c(0), 1, 0)
      dvegetation1 <- ifelse(vegetation %in% c(1, 4, 12), 1, 0)
      dvegetation2 <- ifelse(vegetation %in% c(18, 19, 20), 1, 0)
      dvegetation3 <- ifelse(vegetation %in% c(0), 1, 0)
      dslope_position1 <- ifelse(slope_position %in% c(0), 1, 0)
      dsoil_moisture1 <- ifelse(soil_moisture %in% c(2, 3, 4), 1, 0)
    }

    if (species == "Abies alba") {
      #Tanne
      a0 = -0.712624
      b1 = 1.469029
      b2 = -0.000117
      b3 = 0.502449
      c1 = -0.015949
      c2 = -0.000564
      d1 = 0
      d1a = 0
      d2 = 0
      d2a = 0
      d3 = 0
      d4 = 0
      d5 = 0
      d6 = 0
      SD = 0
      sm = 0
      SR = 0.293053
      s1 = 0.550273
      s2 = 0.383876
      s3 = 0
      s4 = 0
      v1 = 0.185379
      v2 = 0
      v3 = 0
      g1 = 0.37513
      g2 = 0.211501
      MSE = 0.753
      X = 0
      dANFI_region1 <- ifelse(ANFI_region %in% c(17, 19), 1, 0)
      dANFI_region2 <-
        ifelse(ANFI_region %in% c(1, 2, 4, 5, 6, 7, 8, 9, 10, 11, 12, 14, 16, 18, 20, 21),
               1,
               0)
      dsoil_type1 <- ifelse(soil_type %in% c(6, 7, 9, 11, 12), 1, 0)
      dsoil_type2 <-
        ifelse(soil_type %in% c(2, 3, 4, 5, 8, 10, 13, 14, 15, 16, 19, 20, 22, 23, 24, 25, 26),
               1,
               0)
      dsoil_type3 <- ifelse(soil_type %in% c(0), 1, 0)
      dsoil_type4 <- ifelse(soil_type %in% c(0), 1, 0)
      dvegetation1 <- ifelse(vegetation %in% c(4, 6, 8, 18), 1, 0)
      dvegetation2 <- ifelse(vegetation %in% c(0), 1, 0)
      dvegetation3 <- ifelse(vegetation %in% c(0), 1, 0)
      dslope_position1 <- ifelse(slope_position %in% c(3), 1, 0)
      dsoil_moisture1 <- ifelse(soil_moisture %in% c(0), 1, 0)
    }

    if (species == "Larix decidua") {
      #Lärche
      a0 = 0.696551
      b1 = 1.246003
      b2 = -0.000137
      b3 = 0.433521
      c1 = -0.015379
      c2 = -0.000401
      d1 = 0
      d1a = -0.042101
      d2 = 0
      d2a = 0
      d3 = 0
      d4 = 0
      d5 = 0
      d6 = 0
      SD = 0
      sm = 0
      SR = 0
      s1 = 0.497502
      s2 = 0.181174
      s3 = 0
      s4 = 0
      v1 = 0.537855
      v2 = 0.342462
      v3 = 0.214816
      g1 = 0.199215
      g2 = 0
      MSE = 0.573
      X = 0
      dANFI_region1 <-
        ifelse(ANFI_region %in% c(12, 13, 14, 15, 17, 18, 19, 20), 1, 0)
      dANFI_region2 <- ifelse(ANFI_region %in% c(0), 1, 0)
      dsoil_type1 <-
        ifelse(soil_type %in% c(6, 8, 10, 12, 13), 1, 0)
      dsoil_type2 <- ifelse(soil_type %in% c(2, 3, 4, 9), 1, 0)
      dsoil_type3 <- ifelse(soil_type %in% c(0), 1, 0)
      dsoil_type4 <- ifelse(soil_type %in% c(0), 1, 0)
      dvegetation1 <- ifelse(vegetation %in% c(18, 19), 1, 0)
      dvegetation2 <- ifelse(vegetation %in% c(1, 4, 7, 13), 1, 0)
      dvegetation3 <-
        ifelse(vegetation %in% c(2, 6, 8, 10, 11), 1, 0)
      dslope_position1 <- ifelse(slope_position %in% c(0), 1, 0)
      dsoil_moisture1 <- ifelse(soil_moisture %in% c(0), 1, 0)
    }

    if (species == "Pinus sylvestris") {
      #Kiefer
      a0 = 0.658017
      b1 = 1.287496
      b2 = -0.00024
      b3 = 0.722032
      c1 = -0.011146
      c2 = 0
      d1 = -0.00405
      d1a = 0
      d2 = 0
      d2a = -0.225295
      d3 = 0
      d4 = 0
      d5 = 0
      d6 = -0.041745
      SD = 0
      sm = 0.269704
      SR = 0
      s1 = -0.402267
      s2 = -0.347126
      s3 = -0.170259
      s4 = 0.394765
      v1 = 0.130182
      v2 = 0
      v3 = 0
      g1 = 0
      g2 = 0.10559
      MSE = 0.595
      X = 6
      dANFI_region1 <- ifelse(ANFI_region %in% c(0), 1, 0)
      dANFI_region2 <-
        ifelse(ANFI_region %in% c(6, 7, 8, 13, 15, 16, 17, 20, 21), 1, 0)
      dsoil_type1 <- ifelse(soil_type %in% c(11, 17), 1, 0)
      dsoil_type2 <- ifelse(soil_type %in% c(18), 1, 0)
      dsoil_type3 <- ifelse(soil_type %in% c(6, 19), 1, 0)
      dsoil_type4 <-
        ifelse(soil_type %in% c(1, 5, 7, 14, 16, 20, 22), 1, 0)
      dvegetation1 <-
        ifelse(vegetation %in% c(3, 4, 8, 12, 19), 1, 0)
      dvegetation2 <- ifelse(vegetation %in% c(0), 1, 0)
      dvegetation3 <- ifelse(vegetation %in% c(0), 1, 0)
      dslope_position1 <- ifelse(slope_position %in% c(0), 1, 0)
      dsoil_moisture1 <- ifelse(soil_moisture %in% c(1, 2, 3), 1, 0)
    }

    if (species == "Pinus nigra") {
      #Schwarzkiefer
      a0 = -1.54957
      b1 = 1.635013
      b2 = -0.000173
      b3 = 0
      c1 = -0.015162
      c2 = 0
      d1 = -0.007108
      d1a = 0
      d2 = -0.52881
      d2a = 0
      d3 = 0
      d4 = 0
      d5 = 0
      d6 = 0
      SD = 0
      sm = 0
      SR = 0.433283
      s1 = 0
      s2 = 0
      s3 = 0
      s4 = 0
      v1 = 0
      v2 = 0
      v3 = 0
      g1 = 0
      g2 = 0
      MSE = 0.595
      X = 0
      dANFI_region1 <- ifelse(ANFI_region %in% c(0), 1, 0)
      dANFI_region2 <- ifelse(ANFI_region %in% c(0), 1, 0)
      dsoil_type1 <- ifelse(soil_type %in% c(0), 1, 0)
      dsoil_type2 <- ifelse(soil_type %in% c(0), 1, 0)
      dsoil_type3 <- ifelse(soil_type %in% c(0), 1, 0)
      dsoil_type4 <- ifelse(soil_type %in% c(0), 1, 0)
      dvegetation1 <- ifelse(vegetation %in% c(0), 1, 0)
      dvegetation2 <- ifelse(vegetation %in% c(0), 1, 0)
      dvegetation3 <- ifelse(vegetation %in% c(0), 1, 0)
      dslope_position1 <- ifelse(slope_position %in% c(0), 1, 0)
      dsoil_moisture1 <- ifelse(soil_moisture %in% c(0), 1, 0)
    }

    if (species == "Pinus cembra") {
      #Zirbe
      a0 = 2.56557
      b1 = 0.466866
      b2 = 0
      b3 = 0
      c1 = -0.010259
      c2 = 0
      d1 = -0.029576
      d1a = 0
      d2 = 0
      d2a = 0
      d3 = 0
      d4 = 0
      d5 = 0
      d6 = 0
      SD = 0
      sm = 0
      SR = 0
      s1 = 0
      s2 = 0
      s3 = 0
      s4 = 0
      v1 = 0
      v2 = 0
      v3 = 0
      g1 = 0.310889
      g2 = 0
      MSE = 0.595
      X = 18
      dANFI_region1 <-
        ifelse(ANFI_region %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 15, 16, 17, 18, 19, 20, 21),
               1,
               0)
      dANFI_region2 <- ifelse(ANFI_region %in% c(0), 1, 0)
      dsoil_type1 <- ifelse(soil_type %in% c(0), 1, 0)
      dsoil_type2 <- ifelse(soil_type %in% c(0), 1, 0)
      dsoil_type3 <- ifelse(soil_type %in% c(0), 1, 0)
      dsoil_type4 <- ifelse(soil_type %in% c(0), 1, 0)
      dvegetation1 <- ifelse(vegetation %in% c(0), 1, 0)
      dvegetation2 <- ifelse(vegetation %in% c(0), 1, 0)
      dvegetation3 <- ifelse(vegetation %in% c(0), 1, 0)
      dslope_position1 <- ifelse(slope_position %in% c(0), 1, 0)
      dsoil_moisture1 <- ifelse(soil_moisture %in% c(0), 1, 0)
    }

    if (species == "Fagus sylvatica") {
      #Buche
      a0 = -0.644436
      b1 = 1.589274
      b2 = -0.000189
      b3 = 0.630734
      c1 = -0.016353
      c2 = -0.000175
      d1 = -0.00308
      d1a = 0
      d2 = -0.269852
      d2a = 0
      d3 = 0
      d4 = 0
      d5 = -0.031316
      d6 = 0.021224
      SD = -0.215741
      sm = 0
      SR = 0
      s1 = 0.178703
      s2 = 0
      s3 = 0
      s4 = 0
      v1 = 0.710601
      v2 = 0.458712
      v3 = 0
      g1 = 0.329867
      g2 = 0.179966
      MSE = 0.596
      X = 0
      dANFI_region1 <- ifelse(ANFI_region %in% c(3, 4, 5, 21), 1, 0)
      dANFI_region2 <-
        ifelse(ANFI_region %in% c(1, 13, 16, 17, 19, 20), 1, 0)
      dsoil_type1 <-
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
      dsoil_type2 <- ifelse(soil_type %in% c(0), 1, 0)
      dsoil_type3 <- ifelse(soil_type %in% c(0), 1, 0)
      dsoil_type4 <- ifelse(soil_type %in% c(0), 1, 0)
      dvegetation1 <-
        ifelse(vegetation %in% c(3, 8, 9, 18, 19), 1, 0)
      dvegetation2 <-
        ifelse(vegetation %in% c(1, 2, 4, 5, 6, 12), 1, 0)
      dvegetation3 <- ifelse(vegetation %in% c(0), 1, 0)
      dslope_position1 <- ifelse(slope_position %in% c(0), 1, 0)
      dsoil_moisture1 <- ifelse(soil_moisture %in% c(0), 1, 0)
    }

    if (grep("^[A-Z]", unlist(strsplit(species, " ")), value = TRUE) == "Quercus") {
      #Eiche
      a0 = -0.252064
      b1 = 1.44802
      b2 = -0.00014
      b3 = 0
      c1 = -0.02681
      c2 = 0
      d1 = 0
      d1a = 0
      d2 = 0
      d2a = 0
      d3 = 0.347115
      d4 = 0.188109
      d5 = 0
      d6 = 0
      SD = 0
      sm = 0
      SR = 0
      s1 = 0.597342
      s2 = 0.307345
      s3 = 0.115069
      s4 = 0
      v1 = 0.26517
      v2 = 0
      v3 = 0
      g1 = 0
      g2 = 0
      MSE = 0.534
      X = 0
      dANFI_region1 <- ifelse(ANFI_region %in% c(0), 1, 0)
      dANFI_region2 <- ifelse(ANFI_region %in% c(0), 1, 0)
      dsoil_type1 <- ifelse(soil_type %in% c(4, 12, 16), 1, 0)
      dsoil_type2 <- ifelse(soil_type %in% c(10, 15, 17), 1, 0)
      dsoil_type3 <-
        ifelse(soil_type %in% c(1, 3, 6, 8, 9, 11, 13, 14), 1, 0)
      dsoil_type4 <- ifelse(soil_type %in% c(0), 1, 0)
      dvegetation1 <- ifelse(vegetation %in% c(4, 19, 20), 1, 0)
      dvegetation2 <- ifelse(vegetation %in% c(0), 1, 0)
      dvegetation3 <- ifelse(vegetation %in% c(0), 1, 0)
      dslope_position1 <- ifelse(slope_position %in% c(0), 1, 0)
      dsoil_moisture1 <- ifelse(soil_moisture %in% c(0), 1, 0)
    }

    #Other deciduous trees which is not Oak or Beech..
    if (tree_type("Deciduous") &
        !(grep("^[A-Z]", unlist(strsplit(species, " ")), value = TRUE) == "Quercus") &
        species != "Fagus sylvatica") {
      #sonst. Laubholz
      a0 = 0.936249
      b1 = 1.133501
      b2 = 0
      b3 = 0.517598
      c1 = -0.01935
      c2 = 0
      d1 = 0
      d1a = 0
      d2 = -0.460704
      d2a = 0
      d3 = 0.145024
      d4 = -0.040436
      d5 = 0
      d6 = 0
      SD = 0
      sm = 0
      SR = 0
      s1 = 0
      s2 = 0
      s3 = 0
      s4 = 0
      v1 = 0
      v2 = 0
      v3 = 0
      g1 = 0
      g2 = 0
      MSE = 0.757
      X = 0
      dANFI_region1 <- ifelse(ANFI_region %in% c(0), 1, 0)
      dANFI_region2 <- ifelse(ANFI_region %in% c(0), 1, 0)
      dsoil_type1 <- ifelse(soil_type %in% c(0), 1, 0)
      dsoil_type2 <- ifelse(soil_type %in% c(0), 1, 0)
      dsoil_type3 <- ifelse(soil_type %in% c(0), 1, 0)
      dsoil_type4 <- ifelse(soil_type %in% c(0), 1, 0)
      dvegetation1 <- ifelse(vegetation %in% c(0), 1, 0)
      dvegetation2 <- ifelse(vegetation %in% c(0), 1, 0)
      dvegetation3 <- ifelse(vegetation %in% c(0), 1, 0)
      dslope_position1 <- ifelse(slope_position %in% c(0), 1, 0)
      dsoil_moisture1 <- ifelse(soil_moisture %in% c(0), 1, 0)
    }

    #Fraxinus spp..
    if (grep("^[A-Z]", unlist(strsplit(species, " ")), value = TRUE) == "Fraxinus") {
      #Esche
      a0 = 1.28677
    }

    #Poplar hybrids.. if Populus and not one of the named.
    if (grep("^[A-Z]", unlist(strsplit(species, " ")), value = TRUE) == "Populus" &
        !(species %in% c("Populus tremula", "Populus nigra", "Populus alba"))) {
      a0 = 1.879919
    }

    Exposition_rad <- (aspect_degree / 180) * pi
    Bodentiefe1 <- ifelse(soil_depth_cm <= 30, 1, 0)

    BAI = exp(
      a0 + b1 * log(diameter_cm) + b2 * diameter_cm ^ 2 + b3 * log(crown_ratio) + c1 * BAL +
        c2 * crown_competition_factor + d1 * (altitude / 100 - X) ^ 2 +
        d1a * (altitude / 100) + d2 * (incline_percent / 100) ^ 2 +
        d2a * incline_percent / 100 + d3 * (incline_percent / 100) * sin(Exposition_rad) +
        d4 * (incline_percent / 100) * cos(Exposition_rad) + d5 * Humus_F_cm + d6 * Humus_H_cm +
        SD * Bodentiefe1 + sm * dsoil_moisture1 + SR * dslope_position1 +
        s1 * dsoil_type1 + s2 * dsoil_type2 +
        s3 * dsoil_type3 + s4 * dsoil_type4 +
        v1 * dvegetation1 + v2 * dvegetation2 +
        v3 * dvegetation3 + g1 * dANFI_region1 +
        g2 * dANFI_region2 + 0.5 * MSE * log_correction
    )

    BAI <- period * BAI / 5

  }

Monserud_1996_BA_increment_Austria <-
  Vectorize(Monserud_1996_BA_increment_Austria)
