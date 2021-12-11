#' Soil groups of the Austrian National Forest Inventory (Forstliche Bundesversuchsanstalt 1981) transcribed to the FAO-UNESCO soil groups
#'
#' @description From transcription by Sonja Vospernik. \url{https://homepage.boku.ac.at/sonja/ANFI_E.pdf}
#'
#' @param Code 1-26. Default : 1:26.
#'
#' @return A tibble with the selected code(s).
#' @export
#'
#' @examples
#' ANFI_soil_codes()
ANFI_soil_codes <- function(
  Code=1:26
  ){
  stopifnot(Code%in%c(1:26))

    dplyr::tibble("Code"=c(1:26),"ANFI FAO-UNESCO"=c(
      "Leptosols from noncalcareous parent material (Lithic, Umbric and Arenosols)",
      "Cambisols (Dystric, Ferralic) and colluvial soils from dystric silicate parent material",
      "Cambisols (Eutric), colluvial soils from eutric silicate material and Calcareous cambisols",
      "Spodi-Dystric cambisol on silicate material.",
      "Climate-induced podzol from dystric silicate material.",
      "Substrate-induced podzol (quartzite, quartz-phyllite, quartz-sand,quartz-sandstone, Arkose)",
      "Substrate-induced Gleyic podzol.",
      "Light-textured cambisol and spodic cambisols from unconsolidated sediment.",
      "Heavy-textured cambisol and luvisols from moraine, non-calcareous loess or mudstone.",
      "Cambisols and luvisols derived from calcareous loess.",
      "(Eutric) Planosols and stagnic Gleysols derived from flysch or mudstone.",
      "(Eutric) Planosols and stagnic Gleysols derived from loess.",
      "Temporarily waterlogged (Stagno-gleyic) soils on unconsolidated sediments.",
      "Stagnic cambisols or gleysols with marked interflow.",
      "Relic soil material showing ferralic properties (Ferallic cambisols)",
      "Chernozems",
      "Leptosols derived from calcareous material (Rendzic Leptosols and Lithic leptosols)",
      "Colluvial soils showing properties of both Rendzic Leptosols and Chromic Cambisols (Terra Fusca)",
      "Chromic Cambisols on calcareous bedrock (Terra Fusca)",
      "Gleysols",
      "Fluvisols along small rivers",
      "Fluvisols",
      "Mollic and Umbric Gleysols",
      "Histic Gleysols and Terric Histosols",
      "Fibric Histosols",
      "Anthrosols"))[Code,]
}

