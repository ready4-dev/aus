remove_empty_tbs <- function(tbs_ls){
 tbs_ls <-  purrr::discard(tbs_ls, ~ nrow(.x)==0)
 return(tbs_ls)
}
remove_empty_lists <- function(tbs_ls){
  tbs_ls <- purrr::discard(tbs_ls, ~ identical(.x,list()))
  return(tbs_ls)
}
remove_feature_no_stat_sf <- function(no_stat_sf, #.x
                                      feature_pfx_1L_chr,
                                      uids_chr){
  #a <- names(.x)[names(.x) %in%  var_nms_chr]
  #b <- dplyr::pull(.x,!!rlang::sym(a)) %>% unique()
  c <- intersect(names(no_stat_sf),uids_chr)
  d <- no_stat_sf[[c]] %>% as.character()
  e <- d[!startsWith(d, feature_pfx_1L_chr)]
  no_stat_sf <- no_stat_sf %>% dplyr::filter(!!rlang::sym(c) == e)
  return(no_stat_sf)
}
remove_feature_by_prefix_sf <- function(data_sf,
                                        var_name_1L_chr,
                                        prefix_str){
  var_vals <- data_sf %>% dplyr::pull(!!rlang::sym(var_name_1L_chr)) %>% as.character()
  logic_vec <- !startsWith(var_vals, prefix_str)
  data_sf <- data_sf %>% dplyr::filter(logic_vec)
  return(data_sf)
}
