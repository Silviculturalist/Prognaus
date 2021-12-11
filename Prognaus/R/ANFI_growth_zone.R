#' Growth Zones for Austria (ANFI)
#'
#' @param Code 1:21.
#'
#' @return Tibble.
#' @export

Austria_growth_zone <- function(
  Code=1:21
){

  stopifnot(Code%in%c(1:21))

  dplyr::tibble("Code"=c(1:21),"ANFI Wuchzbezirk Growth Region"=c(
    "Austrian Part of the Bohemian Massif",
    "Eastern pannonic semiarid region",
    "Hills and plains between the Alps and the Danube, eastern part",
    "Hills and plains between the Danube, western part",
    "Kobernausserwald",
    "Eastern edge of the Alps",
    "Eastern Flysch Alps",
    "Western Flysch Alps with humid climate",
    "Northern calcareous Alps, eastern part",
    "Northern calcareous Alps, western part",
    "Northern central Alps, eastern part",
    "Northern central Alps, western part",
    "Central Alps",
    "Inner central Alps with continental climate",
    "Southern central Alps",
    "Klagenfurt valley",
    "Austrian Southern Alps",
    "South-eastern edge of the Austrian Alps",
    "Granite hills on the eastern edge of the Alps",
    "South-eastern Hills and terraces",
    "Mountains of the middle 'Burgenland'"
  ))[Code,]

}
