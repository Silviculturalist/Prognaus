% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Hasenauer_1996_crown_ratio_Austria.R
\name{Hausenauer_1996_crown_ratio_Austria}
\alias{Hausenauer_1996_crown_ratio_Austria}
\title{Crown ratio model Prognaus}
\source{
Hasenauer, H. and Monserud, R. A. (1996): A Crown Model for Austrian Forests. For. Ecol. and Manage. 84, 49-60.

Hasenauer, H. (2000): Die simultanen Eigenschaften von Waldwachstumsmodellen. Paul Parey Verlag. Berlin, 131 pp.
}
\usage{
Hausenauer_1996_crown_ratio_Austria(
  species = 1,
  diameter_cm,
  height_m,
  BAL,
  crown_competition_factor,
  altitude = 600,
  incline_percent = 50,
  aspect_degree = 180
)
}
\arguments{
\item{species}{Tree species}

\item{diameter_cm}{Diameter at breast height in cm}

\item{height_m}{Tree height in m}

\item{BAL}{Basal area of larger trees (Wykoff, 1990)}

\item{crown_competition_factor}{Crown competition factor (Krajicek, 1961), e.g. [Prognaus::crown_competition_factor()]}

\item{altitude}{Elevation in m (Range: 200-2200 m)}

\item{incline_percent}{Slope in percent}

\item{aspect_degree}{Aspect in degree}
}
\description{
Calculates the crown ratio from tree and site variables. Dynamic crown ratio models predict the
change in crown ratio. The model was developed by
Hasenauer & Monserud (1996). Coefficients are taken from Hasenauer (2000).
}
