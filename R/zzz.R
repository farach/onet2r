.onLoad <- function(libname, pkgname) {
  utils::globalVariables(c(
    "title", "href", "hot_technology", "in_demand", "percentage",
    "soc_code", "occ_code", "onet_soc_code", "domain", "from_soc_code", "from_vintage",
    "to_soc_code", "to_vintage", "from_onet_soc_code",
    "to_onet_soc_code", "element_id", "scale_id", "soc_vintage",
    "mid_soc_code", "mid_onet_soc_code", "mid_vintage", "reference_soc_code",
    "source_taxonomy", "reference_taxonomy", "crosswalk_weight",
    "employment", "weight_share", "source", "year", "measure_key",
    "measure_score", "scenario", "aggregate", "component", "value"
  ))
}
