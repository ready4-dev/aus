#' Update area names
#' @description update_area_names() is an Update function that edits an object, while preserving core object attributes. Specifically, this function implements an algorithm to update area names. Function argument area_dss_by_year_ls specifies the object to be updated. Argument area_1L_chr provides the object to be updated. The function returns Updated area datasets by year (a list).
#' @param area_dss_by_year_ls Area datasets by year (a list)
#' @param area_1L_chr Area (a character vector of length one)
#' @param altv_names_sf Alternative names (a simple features object)
#' @param state_nms_chr State names (a character vector)
#' @param state_short_and_long_nms_chr State short and long names (a character vector)
#' @param remove_symbol_1L_lgl Remove symbol (a logical vector of length one), Default: TRUE
#' @return Updated area datasets by year (a list)
#' @rdname update_area_names
#' @export 
#' @importFrom dplyr pull filter mutate
#' @importFrom rlang sym
#' @importFrom purrr discard pluck map2_chr map_int map_chr map
#' @importFrom stringr str_replace_all str_locate_all str_sub str_replace
#' @importFrom tools toTitleCase
#' @keywords internal
update_area_names <- function (area_dss_by_year_ls, area_1L_chr, altv_names_sf, state_nms_chr, 
    state_short_and_long_nms_chr, remove_symbol_1L_lgl = TRUE) 
{
    area_names_chr <- altv_names_sf %>% dplyr::pull(!!rlang::sym(area_1L_chr))
    if (length(area_names_chr) == (as.numeric(as.character(area_names_chr))[!is.na(as.numeric(as.character(area_names_chr)))] %>% 
        length())) {
        updated_area_dss_by_year_ls <- area_dss_by_year_ls
    }
    else {
        altv_names_chr <- altv_names_sf %>% dplyr::filter(!!rlang::sym(state_nms_chr) == 
            state_short_and_long_nms_chr[2]) %>% dplyr::pull(!!rlang::sym(area_1L_chr)) %>% 
            unique() %>% as.character() %>% purrr::discard(is.na)
        short_altv_names_chr <- altv_names_chr %>% stringr::str_replace_all(paste0(" \\(", 
            state_short_and_long_nms_chr[1], "\\)"), "") %>% 
            stringr::str_replace_all(" Shire", "")
        current_names_chr <- area_dss_by_year_ls %>% purrr::pluck(1) %>% 
            dplyr::pull(!!rlang::sym(area_1L_chr))
        if (remove_symbol_1L_lgl) {
            short_altv_names_chr <- purrr::map2_chr(altv_names_chr, 
                short_altv_names_chr %>% stringr::str_locate_all(" ") %>% 
                  purrr::map_int(~.x[length(.x)]), ~stringr::str_sub(.x, 
                  end = .y - 1) %>% tolower() %>% tools::toTitleCase())
            current_names_chr <- current_names_chr %>% stringr::str_replace_all(paste0(" \\(", 
                "DC", "\\)"), "") %>% stringr::str_replace_all(paste0(" \\(", 
                "AC", "\\)"), "") %>% stringr::str_replace_all(paste0(" \\(", 
                "C", "\\)"), "") %>% stringr::str_replace_all(paste0(" \\(", 
                "T", "\\)"), "") %>% stringr::str_replace_all(paste0(" \\(", 
                "M", "\\)"), "") %>% stringr::str_replace_all(paste0(" \\(", 
                "RC", "\\)"), "") %>% stringr::str_replace_all(paste0(" \\(", 
                "RegC", "\\)"), "") %>% stringr::str_replace_all(paste0(" \\(", 
                "S", "\\)"), "") %>% stringr::str_replace_all(paste0(" \\(", 
                "R", "\\)"), "") %>% stringr::str_replace_all(paste0(" \\(", 
                "Qld", "\\)"), "")
        }
        short_altv_names_chr <- short_altv_names_chr %>% stringr::str_replace_all(" - ", 
            "-") %>% stringr::str_replace("Fyshwick-Pialligo-Hume", 
            "Fyshwick-Pialliago-Hume") %>% stringr::str_replace("Break O'day", 
            "Break Oday") %>% stringr::str_replace("Waratah/Wynyard", 
            "Waratah-Wynyard") %>% stringr::str_replace("Woden Valley", 
            "Woden") %>% stringr::str_replace(" \\(m\\)", "") %>% 
            stringr::str_replace(" \\(r\\)", "") %>% stringr::str_replace(" \\(s\\)", 
            "") %>% stringr::str_replace(" \\(c\\)", "") %>% 
            stringr::str_replace("/", " / ")
        matched_names_chr <- purrr::map_chr(current_names_chr[current_names_chr != 
            "Unallocated" & !startsWith(current_names_chr, "Unincorporated")] %>% 
            tolower() %>% tools::toTitleCase() %>% stringr::str_replace(" - ", 
            "-") %>% stringr::str_replace(paste0(" ", state_short_and_long_nms_chr[1] %>% 
            tolower() %>% tools::toTitleCase()), "") %>% stringr::str_replace_all("'", 
            "") %>% stringr::str_replace_all("/", " / ") %>% 
            stringr::str_replace_all("  ", " ") %>% stringr::str_replace(" \\(b\\)", 
            ""), ~altv_names_chr[short_altv_names_chr == .x])
        updated_area_dss_by_year_ls <- purrr::map(area_dss_by_year_ls, 
            ~.x %>% dplyr::filter(!!rlang::sym(area_1L_chr) != 
                "Unallocated" & !startsWith(!!rlang::sym(area_1L_chr), 
                "Unincorporated")) %>% dplyr::mutate(`:=`(!!rlang::sym(area_1L_chr), 
                matched_names_chr)) %>% rbind(.x %>% dplyr::filter(!!rlang::sym(area_1L_chr) == 
                "Unallocated" | startsWith(!!rlang::sym(area_1L_chr), 
                "Unincorporated"))))
    }
    return(updated_area_dss_by_year_ls)
}
