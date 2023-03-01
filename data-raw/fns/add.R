add_year_2016_to_ACT_ppr <- function(aus_sa3_ACT_ppr_2018_tb, #
                                     aus_sa2_ACT_erp_age_sex_2016_tb){
  act_2016_erp_tb <- aus_sa2_ACT_erp_age_sex_2016_tb %>%
    dplyr::group_by(`SA3 name`) %>%
    dplyr::select(dplyr::starts_with("y2016")) %>%
    dplyr::summarise_all(sum) %>%
    dplyr::select(-dplyr::starts_with("y2016.total")) %>%
    dplyr::rename_all( ~ stringr::str_replace(.x,"y2016.","")) %>%
    dplyr::rename(SA3_NAME16 = `SA3 name`) %>%
    dplyr::mutate(year_chr = "2016") %>%
    dplyr::select(year_chr, dplyr::everything())
  common_vars <- intersect(act_2016_erp_tb %>% dplyr::pull(SA3_NAME16) %>% sort(),
                           aus_sa3_ACT_ppr_2018_tb$y2017 %>% dplyr::pull(SA3_NAME16) %>% sort())
  vars_to_change <- setdiff(act_2016_erp_tb %>% dplyr::pull(SA3_NAME16),
                            common_vars) %>% sort()
  target_vars <- setdiff(aus_sa3_ACT_ppr_2018_tb$y2017 %>% dplyr::pull(SA3_NAME16),
                         common_vars) %>% sort()
  target_vars <- target_vars[target_vars != "Unallocated"]
  act_2016_erp_tb <- act_2016_erp_tb %>%
    dplyr::mutate(SA3_NAME16 = purrr::map_chr(SA3_NAME16,
                                              ~ ifelse(.x %in% vars_to_change,
                                                       target_vars[vars_to_change == .x],
                                                       .x)))
  act_2016_erp_tb <- act_2016_erp_tb %>%
    tibble::add_case(year_chr = "2016",
                     SA3_NAME16 = "Unallocated")
  act_2016_erp_tb[is.na(act_2016_erp_tb)] <- 0
  act_2016_erp_tb <- act_2016_erp_tb[match(aus_sa3_ACT_ppr_2018_tb$y2017$SA3_NAME16, act_2016_erp_tb$SA3_NAME16),]
  new_act_ppr_ls <- purrr::prepend(aus_sa3_ACT_ppr_2018_tb, list(y2016 = act_2016_erp_tb))
  vicinity::transform_sf_ls(new_act_ppr_ls)
}

