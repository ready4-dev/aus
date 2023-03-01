#' Get no statistic areas
#' @description get_no_stat_areas() is a Get function that retrieves a pre-existing data object from memory, local file system or online repository. Specifically, this function implements an algorithm to get no statistic areas. Function argument land_sf specifies the where to look for the required object. The function is called for its side effects and does not return a value.
#' @param land_sf Land (a simple features object)
#' @param statistic_sf Statistic (a simple features object)
#' @param land_name_1L_chr Land name (a character vector of length one)
#' @param land_val_vec PARAM_DESCRIPTION
#' @param stat_name_str PARAM_DESCRIPTION
#' @param stat_val_vec PARAM_DESCRIPTION
#' @return NULL
#' @rdname get_no_stat_areas
#' @export 
#' @importFrom dplyr filter
#' @importFrom rlang sym
#' @importFrom sf st_difference st_union st_dimension st_set_geometry st_as_sf
#' @keywords internal
get_no_stat_areas <- function (land_sf, statistic_sf, land_name_1L_chr, land_val_vec, 
    stat_name_str, stat_val_vec) 
{
    land_sf <- land_sf %>% dplyr::filter(!!rlang::sym(land_name_1L_chr) %in% 
        land_val_vec)
    statistic_sf <- statistic_sf %>% dplyr::filter(!!rlang::sym(stat_name_str) %in% 
        stat_val_vec)
    no_stat_sf <- sf::st_difference(land_sf, sf::st_union(statistic_sf))
    no_stat_tb <- statistic_sf %>% dplyr::filter(is.na(sf::st_dimension(geometry))) %>% 
        sf::st_set_geometry(NULL)
    merge(no_stat_sf, no_stat_tb) %>% sf::st_as_sf()
}
