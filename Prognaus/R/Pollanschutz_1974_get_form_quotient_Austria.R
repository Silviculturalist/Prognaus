#'Form factors for different tree species as used in Prognaus.
#'
#'
#'@description Calculates the form factor;
#'form factors are dimensionless. For use together with tree_volume_Prognaus().
#'
#'@source Pollanschütz, J. (1974): Formzahlfunktionen der Hauptbaumarten
#' Österreichs. AFZ 85, 341- 343.
#'
#'@param species One of: "Picea abies", "Abies alba", "Larix decidua",
#'"Pinus sylvestris","Pinus nigra","Pinus cembra","Fagus sylvatica","Quercus",
#'"Fraxinus","Acer", or another tree species.
#'
#'@param diameter_cm Diameter of tree at breast height.
#'@param height_m Tree height in metres.
#'
#'@export
#'
#'@examples
#'#Volume of an individual tree in m3, see also vPoll
#'(dbh/100)^2*pi/4*height*Pollanschutz_1974_get_form_quotient_Austria(species="Picea abies",diameter_cm=30,height_m=30)

Pollanschutz_1974_get_form_quotient_Austria <- function(species,diameter_cm,height_m) {
  BHD = diameter_cm / 10 # in dm.
  Hoehe = height_m * 10 #in dm.

  #Not named broadleaf.. if not one of named species or belonging to named family.
  other_broadleaved <- ifelse(tree_type(species)=="Deciduous" & !(species%in%c("Fagus sylvatica","Castanea sativa","Robinia pseudoacacia","Populus tremula","Populus alba","Populus nigra")) & !(grep("^[A-Z]", unlist(strsplit(paste0(species), " ")),value=TRUE))%in%c("Sorbus","Acer","Tilia","Quercus","Fraxinus","Ulmus","Betula","Alnus","Corylus"),
                              1,
                              0
                              )

  #Not named conifer.. if not one of named species.
  other_coniferous <- ifelse(tree_type(species)=="Coniferous" & !(species%in%c("Picea abies","Abies alba","Larix decidua","Pinus sylvestris","Pinus nigra","Pinus mugo","Pinus cembra")),
                             1,
                             0
                             )

  if (species == "Picea abies" & BHD > 1.05 | other_coniferous & BHD > 1.05) { #Fichte, sonst. Nadelholz
    b1 = 0.46818
    b2 = -0.013919
    b3 = -28.213
    b4 = 0.37474
    b5 = -0.28875
    b6 = 28.279
    b7 = 0
  }
  if (species == "Picea abies" & BHD <= 1.05 | other_coniferous & BHD <=1.05) { #Fichte, sonst. Nadelholz
    b1 = 0.563443
    b2 = -0.12731
    b3 = -8.55022
    b4 = 0
    b5 = 0
    b6 = 7.6331
    b7 = 0
  }
  if (species == "Abies alba" & BHD > 1.05) { #Tanne
    b1 = 0.580223
    b2 = -0.0307373
    b3 = -17.1507
    b4 = 0.089869
    b5 = -0.080557
    b6 = 19.661
    b7 = -2.45844
  }
  if (species == "Abies alba" & BHD <= 1.05) { #Tanne
    b1 = 0.560673
    b2 = 0.15468
    b3 = -0.65583
    b4 = 0.03321
    b5 = 0
    b6 = 0
    b7 = -0
  }
  if (species == "Larix decidua" & BHD > 1.05) { #L?rche
    b1 = 0.609443
    b2 = -0.0455748
    b3 = -18.6631
    b4 = -0.248736
    b5 = 0.126594
    b6 = 36.9783
    b7 = -14.204
  }
  if (species == "Larix decidua" & BHD <= 1.05) { #L?rche
    b1 = 0.48727
    b2 = 0
    b3 = -2.04291
    b4 = 0
    b5 = 0
    b6 = 5.9995
    b7 = 0
  }
  if (species == "Pinus sylvestris") { #Kiefer
    b1 = 0.435949
    b2 = -0.0149083
    b3 = 5.21091
    b4 = 0
    b5 = 0.028702
    b6 = 0
    b7 = 0
  }
  if (species == "Pinus nigra") { #Schwarzkiefer
    b1 = 0.53438
    b2 = -0.00763
    b3 = 0
    b4 = 0
    b5 = 0
    b6 = 0
    b7 = 2.2414
  }
  if (species == "Pinus cembra") { #Zirbe
    b1 = 0.525744
    b2 = -0.0334896
    b3 = 7.38943
    b4 = -0.10646
    b5 = 0
    b6 = 0
    b7 = 3.34479
  }

  #If European Beech, European chestnut, Black Locust or Sorbus spp.
  if (species %in%c("Fagus sylvatica","Castanea sativa","Robinia pseudoacacia") & BHD > 1.05 | BHD >1.05 & (grep("^[A-Z]", unlist(strsplit(paste0(species), " ")),value=TRUE))=="Sorbus") { #Buche, Kastanie, Robinie, S|bus
    b1 = 0.686253
    b2 = -0.0371508
    b3 = -31.0674
    b4 = -0.386321
    b5 = 0.219462
    b6 = 49.6163
    b7 = -22.3719
  }
  if (species %in%c("Fagus sylvatica","Castanea sativa","Robinia pseudoacacia") & BHD <= 1.05 | BHD <=1.05 & (grep("^[A-Z]", unlist(strsplit(paste0(species), " ")),value=TRUE))=="Sorbus") { #Buche, Kastanie, Robinie, S|bus
    b1 = 0.5173
    b2 = 0
    b3 = -13.62144
    b4 = 0
    b5 = 0
    b6 = 9.9888
    b7 = 0
  }
  if ((grep("^[A-Z]", unlist(strsplit(species, " ")),value=TRUE))=="Quercus" & BHD > 1.05) { #Eiche
    b1 = 0.115631
    b2 = 0
    b3 = 65.9961
    b4 = 1.20321
    b5 = -0.930406
    b6 = -215.758
    b7 = 168.477
  }
  if ((grep("^[A-Z]", unlist(strsplit(species, " ")),value=TRUE))=="Quercus" & BHD <= 1.05) { #Eiche
    b1 = 0.417118
    b2 = 0.21941
    b3 = 13.32594
    b4 = 0
    b5 = 0
    b6 = 0
    b7 = 0
  }
  if (species == "Carpinus betulus") { #Hainbuche
    b1 = 0.32473
    b2 = 0.02432
    b3 = 0
    b4 = 0.23972
    b5 = 0
    b6 = -9.9388
    b7 = 0
  }
  if ((grep("^[A-Z]", unlist(strsplit(species, " ")),value=TRUE))=="Fraxinus") { #Esche
    b1 = 0.48122
    b2 = -0.01489
    b3 = -10.83056
    b4 = 0
    b5 = 0
    b6 = 9.3936
    b7 = 0
  }
  if ((grep("^[A-Z]", unlist(strsplit(species, " ")),value=TRUE))%in%c("Acer","Tilia")) { #Ah|n, Linde
    b1 = 0.50101
    b2 = -0.03521
    b3 = -8.07176
    b4 = 0
    b5 = 0.03521
    b6 = 0
    b7 = 0
  }
  if ((grep("^[A-Z]", unlist(strsplit(species, " ")),value=TRUE))=="Ulmus") { #Ulme
    b1 = 0.44215
    b2 = -0.02446
    b3 = 0
    b4 = 0
    b5 = 0
    b6 = 0
    b7 = 2.87714
  }
  if ((grep("^[A-Z]", unlist(strsplit(species, " ")),value=TRUE))=="Betula") { #Birke
    b1 = 0.42831
    b2 = -0.06643
    b3 = 0
    b4 = 0
    b5 = 0
    b6 = 8.4307
    b7 = 0
  }
  if ((grep("^[A-Z]", unlist(strsplit(species, " ")),value=TRUE))=="Alnus" & BHD > 1.05) { #Erle
    b1 = 0.42937
    b2 = 0
    b3 = -4.10259
    b4 = 0
    b5 = 0
    b6 = 16.7578
    b7 = -5.16631
  }
  if ((grep("^[A-Z]", unlist(strsplit(species, " ")),value=TRUE))=="Alnus" & BHD <= 1.05) { #Erle
    b1 = 0.387399
    b2 = 0
    b3 = 7.17123
    b4 = 0.04407
    b5 = 0
    b6 = 0
    b7 = 0
  }
  if (species=="Populus alba" & BHD > 1.05) { #Weisspappel
    b1 = 0.31525
    b2 = 0
    b3 = 0
    b4 = 0.51079
    b5 = -0.34279
    b6 = -26.08
    b7 = 28.6334
  }
  if (species=="Populus alba" & BHD <= 1.05) { #Weisspappel
    b1 = 0.366419
    b2 = 0
    b3 = 1.13323
    b4 = 0.1306
    b5 = 0
    b6 = 0
    b7 = 0
  }
  if (species%in%c("Populus tremula","Populus nigra")) {  #Schwarzpappel, Zitterppappel
    b1 = 0.4115
    b2 = -0.00989
    b3 = -28.27478
    b4 = 0.35599
    b5 = -0.21986
    b6 = 21.4913
    b7 = 0
  }
  if ((grep("^[A-Z]", unlist(strsplit(species, " ")),value=TRUE))%in%c("Salix","Prunus") | other_broadleaved) { #Salix, Kirsche, Other broadleaved trees
    b1 = 0.54008
    b2 = -0.02716
    b3 = -25.11447
    b4 = 0.08327
    b5 = 0
    b6 = 9.3988
    b7 = 0
  }

  #Latsche == Pinus mugo. Hasel - Corylus spp..
  if (species == "Pinus mugo" | (grep("^[A-Z]", unlist(strsplit(species, " ")),value=TRUE))=="Corylus") { #Hasel, Latsche
    b1 = 0
    b2 = 0
    b3 = 0
    b4 = 0
    b5 = 0
    b6 = 0
    b7 = 0
  }


  #Throw warning if unspecified species
  if(other_broadleaved){
    warning("Unknown broadleaf - using function for 'other broadleaf': ",paste0("f = ",b1," +", b2,"*(log(diameter_dm))^2 + ",b3,"*(1/height_dm) + ",b4,"*(1/diameter_dm) + ",b5,"*(1/(diameter_dm^2)) + ",b6,"*(1/(diameter_dm*height_dm)) + ",b7,"*(1/(diameter_dm^2*height_dm))"))
  }

  if(other_coniferous){
    warning("Unknown conifer - using function for 'other conifer': ",paste0("f = ",b1," +", b2,"*(log(diameter_dm))^2 + ",b3,"*(1/height_dm) + ",b4,"*(1/diameter_dm) + ",b5,"*(1/(diameter_dm^2)) + ",b6,"*(1/(diameter_dm*height_dm)) + ",b7,"*(1/(diameter_dm^2*height_dm))"))
  }


  return(
  b1 + b2*(log(BHD))^2 + b3*(1/Hoehe) + b4*(1/BHD) + b5*(1/(BHD^2)) + b6*(1/(BHD*Hoehe)) + b7*(1/(BHD^2*Hoehe))
  )
}

Pollanschutz_1974_get_form_quotient_Austria <- Vectorize(Pollanschutz_1974_get_form_quotient_Austria)
