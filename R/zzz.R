.onLoad <- function(libname, pkgname) {
  utils::globalVariables(c(
    "title", "href", "hot_technology", "in_demand", "percentage",
    "soc_code", "occ_code", "from_soc_code", "from_vintage",
    "to_soc_code", "to_vintage", "element_id", "scale_id",
    "soc_vintage", "mid_soc_code", "mid_vintage"
  ))
}
