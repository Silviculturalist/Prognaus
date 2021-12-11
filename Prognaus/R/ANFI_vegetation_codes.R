#' Vegetation types of ANFI (1981), from Monserud and Sterba (1996)
#'
#' @param Code 1-20. Default (1:20)
#'
#' @return Tibble.
#' @export
#'
#' @examples
#' ANFI_vegetation_codes()
ANFI_vegetation_codes <- function(
  Code=1:20
){

  stopifnot(Code%in%c(1:20))

      dplyr::tibble("Code"=c(1:20),"Type"=c(
        "Shade-tolerant herbs",
        "Moderately moist herb",
        "Thermophilic herb",
        "Oxalis acetosella",
        "Luxuriant Moss-Vaccinium-Avenella",
        "Sparse Moss-Vaccinium-Avenella",
        "Moss",
        "Avenella",
        "Dry Blueberry-Cranberry",
        "Calluna",
        "Sphagnum-Vaccinium-Avenella",
        "Competing grass cover",
        "Depletion or litter erosion",
        "Subalpine dwarf shrubs",
        "Erica",
        "Pasture forest",
        "Pioneer vegetation",
        "Seep vegetation",
        "Hydrophytic perennial shrub",
        "Floodplain or alluvial forest"
        ),
        "Site"=c(
          "Moist but not wet sites with a high decomposition rate and mull humus",
          "Moderately moist soils with a high decomposition rate. Humus type on these sites is mull, sometimes moder",
          "Dry or moderately moist soils",
          "On moder humus in conifer stands",
          "On degraded soils of secondary pine forests Decomposition rate is low, humus type is usually mor. Vaccinium myrtillus is high and fertile on these sites",
          "On degraded soils, but slightly better than type 5. Humus type is mainly moder. Vaccinium myrtillus is small and infertile.",
          "Moderately warm and moderately moist sites",
          "On degraded soils in open stands",
          "Highly leached degradation sites or sites with dry soil surface type",
          "Similar to type 9, but even more degraded",
          "Moist or wet degraded sites. Humus type is mor",
          "Contains various ecologically different types, which inhibit regeneration",
          "High litter erosion sites",
          "Sites close to the timberline",
          "Dry sun-exposed sites on dolomite or limestone",
          "Pastures",
          "Extreme sites such as rock outcrops and ridges",
          "Interflows, springs",
          "Moist or wet sites",
          "Flood plains or alluvial forest types"
        ))[Code,]

}
