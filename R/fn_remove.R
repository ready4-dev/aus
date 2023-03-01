#' Remove empty lists
#' @description remove_empty_lists() is a Remove function that edits an object, removing a specified element or elements. Specifically, this function implements an algorithm to remove empty lists. Function argument tbs_ls specifies the object to be updated. The function returns Tibbles (a list).
#' @param tbs_ls Tibbles (a list)
#' @return Tibbles (a list)
#' @rdname remove_empty_lists
#' @export 
#' @importFrom purrr discard
#' @keywords internal
remove_empty_lists <- function (tbs_ls) 
{
    tbs_ls <- purrr::discard(tbs_ls, ~identical(.x, list()))
    return(tbs_ls)
}
#' Remove empty tibbles
#' @description remove_empty_tbs() is a Remove function that edits an object, removing a specified element or elements. Specifically, this function implements an algorithm to remove empty tibbles. Function argument tbs_ls specifies the object to be updated. The function returns Tibbles (a list).
#' @param tbs_ls Tibbles (a list)
#' @return Tibbles (a list)
#' @rdname remove_empty_tbs
#' @export 
#' @importFrom purrr discard
#' @keywords internal
remove_empty_tbs <- function (tbs_ls) 
{
    tbs_ls <- purrr::discard(tbs_ls, ~nrow(.x) == 0)
    return(tbs_ls)
}
#' Remove feature by prefix simple features object
#' @description remove_feature_by_prefix_sf() is a Remove function that edits an object, removing a specified element or elements. Specifically, this function implements an algorithm to remove feature by prefix simple features object. Function argument data_sf specifies the object to be updated. Argument var_name_1L_chr provides the object to be updated. The function returns Data (a simple features object).
#' @param data_sf Data (a simple features object)
#' @param var_name_1L_chr Variable name (a character vector of length one)
#' @param prefix_str PARAM_DESCRIPTION
#' @return Data (a simple features object)
#' @rdname remove_feature_by_prefix_sf
#' @export 
#' @importFrom dplyr pull filter
#' @importFrom rlang sym
#' @keywords internal
remove_feature_by_prefix_sf <- function (data_sf, var_name_1L_chr, prefix_str) 
{
    var_vals <- data_sf %>% dplyr::pull(!!rlang::sym(var_name_1L_chr)) %>% 
        as.character()
    logic_vec <- !startsWith(var_vals, prefix_str)
    data_sf <- data_sf %>% dplyr::filter(logic_vec)
    return(data_sf)
}
#' Remove feature no statistic simple features object
#' @description remove_feature_no_stat_sf() is a Remove function that edits an object, removing a specified element or elements. Specifically, this function implements an algorithm to remove feature no statistic simple features object. Function argument no_stat_sf specifies the object to be updated. Argument feature_pfx_1L_chr provides the object to be updated. The function returns No statistic (a simple features object).
#' @param no_stat_sf No statistic (a simple features object)
#' @param feature_pfx_1L_chr Feature prefix (a character vector of length one)
#' @param uids_chr Unique identifiers (a character vector)
#' @return No statistic (a simple features object)
#' @rdname remove_feature_no_stat_sf
#' @export 
#' @importFrom dplyr filter
#' @importFrom rlang sym
#' @keywords internal
remove_feature_no_stat_sf <- function (no_stat_sf, feature_pfx_1L_chr, uids_chr) 
{
    c <- intersect(names(no_stat_sf), uids_chr)
    d <- no_stat_sf[[c]] %>% as.character()
    e <- d[!startsWith(d, feature_pfx_1L_chr)]
    no_stat_sf <- no_stat_sf %>% dplyr::filter(!!rlang::sym(c) == 
        e)
    return(no_stat_sf)
}
