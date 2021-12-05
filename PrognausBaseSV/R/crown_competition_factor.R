
#'@title Crown competition factor
#'
#'@description \code{CCF.P} calculates the crown competition factor
#'
#'@details
#'Calculates the crown competition factor according to Krajicek et al. (1961)
#'using the open-grown tree diameter functions developed
#'by Hasenauer (1997) [Prognaus::Hasenauer_1997_crown_diameter_open_grown_Austria()].
#'
#'Circular crown areas are assumed
#'and multiplied by the represented stem number (e.g. in fixed area plots or
#'angle count samples), e.g. [Prognaus::influence_zone()]
#'@param ID Unique ID for each sample plot
#'@inheritParams influence_zone
#'@inheritParams Hasenauer_1997_crown_diameter_open_grown_Austria

##!Achtung!!!! Kein Vectorize!!! erzeugt hier Unfug!!!!!
CCF.P<-function(ID_Punkt=1, BART=1, BHD_cm, nrep=1, MSE_ja=0){

  SolD<-SD.P(BART,BHD_cm,MSE_ja)
  Crownc<-EZ(SolD,nrep)
  dat <- data.frame("Crownc"=Crownc, "ID_Punkt"=ID_Punkt)
  dat1<-aggregate(dat$Crownc, by=list(dat$ID_Punkt), FUN=sum)
  dat2<-merge(dat, dat1, by.x="ID_Punkt", by.y="Group.1", sort="FALSE")
  dat2$CCF<-dat2$x/100
  CCF<-dat2$CCF
}
