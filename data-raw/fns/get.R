# get_agent_areas <- function(agent_locations_tb, # get_client_suburb_vector
#                             area_var_nm_1L_chr = "Suburb"){
#   included_suburbs <- agent_locations_tb %>%
#     dplyr::select(!!rlang::sym(area_var_nm_1L_chr)) %>% #Suburb
#     unique() %>%
#     dplyr::pull() %>%
#     stringr::str_sort()
#   return(included_suburbs)
# }
get_no_stat_areas <- function(land_sf,
                              statistic_sf,
                              land_name_1L_chr,
                              land_val_vec,
                              stat_name_str,
                              stat_val_vec){
  land_sf <- land_sf %>% dplyr::filter(!!rlang::sym(land_name_1L_chr) %in% land_val_vec)
  statistic_sf <- statistic_sf %>% dplyr::filter(!!rlang::sym(stat_name_str) %in% stat_val_vec)
  no_stat_sf <- sf::st_difference(land_sf,sf::st_union(statistic_sf))
  no_stat_tb <- statistic_sf %>% dplyr::filter(is.na(sf::st_dimension(geometry))) %>% sf::st_set_geometry(NULL)
  merge(no_stat_sf,no_stat_tb) %>%
    sf::st_as_sf()
}
