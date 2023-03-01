#' Add year 2016 to ACT population projection
#' @description add_year_2016_to_ACT_ppr() is an Add function that updates an object by adding data to that object. Specifically, this function implements an algorithm to add year 2016 to act population projection. Function argument aus_sa3_ACT_ppr_2018_tb specifies the object to be updated. The function is called for its side effects and does not return a value.
#' @param aus_sa3_ACT_ppr_2018_tb Australia Statistical Area 3 ACT population projection 2018 (a tibble)
#' @param aus_sa2_ACT_erp_age_sex_2016_tb Australia Statistical Area 2 ACT Estimatedesident Population age sex 2016 (a tibble)
#' @return NULL
#' @rdname add_year_2016_to_ACT_ppr
#' @export 
#' @importFrom dplyr group_by select starts_with summarise_all rename_all rename mutate everything pull
#' @importFrom stringr str_replace
#' @importFrom purrr map_chr prepend
#' @importFrom tibble add_case
#' @importFrom vicinity transform_sf_ls
#' @keywords internal
add_year_2016_to_ACT_ppr <- function (aus_sa3_ACT_ppr_2018_tb, aus_sa2_ACT_erp_age_sex_2016_tb) 
{
    act_2016_erp_tb <- aus_sa2_ACT_erp_age_sex_2016_tb %>% dplyr::group_by(`SA3 name`) %>% 
        dplyr::select(dplyr::starts_with("y2016")) %>% dplyr::summarise_all(sum) %>% 
        dplyr::select(-dplyr::starts_with("y2016.total")) %>% 
        dplyr::rename_all(~stringr::str_replace(.x, "y2016.", 
            "")) %>% dplyr::rename(SA3_NAME16 = `SA3 name`) %>% 
        dplyr::mutate(year_chr = "2016") %>% dplyr::select(year_chr, 
        dplyr::everything())
    common_vars <- intersect(act_2016_erp_tb %>% dplyr::pull(SA3_NAME16) %>% 
        sort(), aus_sa3_ACT_ppr_2018_tb$y2017 %>% dplyr::pull(SA3_NAME16) %>% 
        sort())
    vars_to_change <- setdiff(act_2016_erp_tb %>% dplyr::pull(SA3_NAME16), 
        common_vars) %>% sort()
    target_vars <- setdiff(aus_sa3_ACT_ppr_2018_tb$y2017 %>% 
        dplyr::pull(SA3_NAME16), common_vars) %>% sort()
    target_vars <- target_vars[target_vars != "Unallocated"]
    act_2016_erp_tb <- act_2016_erp_tb %>% dplyr::mutate(SA3_NAME16 = purrr::map_chr(SA3_NAME16, 
        ~ifelse(.x %in% vars_to_change, target_vars[vars_to_change == 
            .x], .x)))
    act_2016_erp_tb <- act_2016_erp_tb %>% tibble::add_case(year_chr = "2016", 
        SA3_NAME16 = "Unallocated")
    act_2016_erp_tb[is.na(act_2016_erp_tb)] <- 0
    act_2016_erp_tb <- act_2016_erp_tb[match(aus_sa3_ACT_ppr_2018_tb$y2017$SA3_NAME16, 
        act_2016_erp_tb$SA3_NAME16), ]
    new_act_ppr_ls <- purrr::prepend(aus_sa3_ACT_ppr_2018_tb, 
        list(y2016 = act_2016_erp_tb))
    vicinity::transform_sf_ls(new_act_ppr_ls)
}
