#' 
#' Manufacture a new object
#' @name manufacture-AusLookup
#' @description manufacture method applied to AusLookup
#' @param x An object of class AusLookup
#' @param area_sf Area (a simple features object), Default: NULL
#' @param att_data_xx Attribute data (an output object of multiple potential types), Default: NULL
#' @param area_unit_1L_chr Area unit (a character vector of length one), Default: character(0)
#' @param include_chr Include (a character vector), Default: character(0)
#' @param match_value_xx Match value (an output object of multiple potential types), Default: character(0)
#' @param population_tb Population (a tibble), Default: NULL
#' @param region_short_long_chr Region short long (a character vector), Default: character(0)
#' @param subdivision_1L_chr Subdivision (a character vector of length one), Default: character(0)
#' @param type_1L_chr Type (a character vector of length one), Default: 'custom'
#' @param what_1L_chr What (a character vector of length one), Default: 'attribute'
#' @param year_1L_chr Year (a character vector of length one), Default: character(0)
#' @param ... Additional arguments
#' @return Object (an output object of multiple potential types)
#' @rdname manufacture-methods
#' @aliases manufacture,AusLookup-method
#' @export 
#' @importFrom ready4 get_from_lup_obj manufacture
#' @importFrom purrr map2 reduce
#' @importFrom stringr str_sub str_replace
#' @importFrom dplyr inner_join rename_at vars starts_with funs ends_with mutate_at pull select mutate_if contains one_of
#' @importFrom rlang sym
#' @importFrom methods callNextMethod
methods::setMethod("manufacture", "AusLookup", function (x, area_sf = NULL, att_data_xx = NULL, area_unit_1L_chr = character(0), 
    include_chr = character(0), match_value_xx = character(0), 
    population_tb = NULL, region_short_long_chr = character(0), 
    subdivision_1L_chr = character(0), type_1L_chr = "custom", 
    what_1L_chr = "attribute", year_1L_chr = character(0), ...) 
{
    if (type_1L_chr == "custom") {
        if (what_1L_chr == "attribute") {
            state_nms_chr <- ready4::get_from_lup_obj(data_lookup_tb = x@vicinity_templates_r3, 
                match_value_xx = "STE", match_var_nm_1L_chr = "area_type_chr", 
                target_var_nm_1L_chr = "subdivision_chr", evaluate_1L_lgl = FALSE)
            att_data_xx <- att_data_xx %>% update_area_names(altv_names_sf = area_sf, 
                area_1L_chr = area_unit_1L_chr, state_nms_chr = state_nms_chr, 
                state_short_and_long_nms_chr = region_short_long_chr, 
                remove_symbol_1L_lgl = ifelse(region_short_long_chr[1] %in% 
                  c("ACT", "NT"), F, T)) %>% purrr::map2(names(att_data_xx) %>% 
                stringr::str_sub(start = 2), ~manufacture(x, 
                population_tb = .x, year_1L_chr = .y, include_chr = NA_character_, 
                subdivision_1L_chr = region_short_long_chr[2], 
                match_value_xx = match_value_xx, type_1L_chr = "custom", 
                what_1L_chr = "population")) %>% purrr::reduce(~dplyr::inner_join(.x, 
                .y))
            object_xx <- att_data_xx
        }
        if (what_1L_chr == "population") {
            PPR_logic_1L_lgl <- x@vicinity_processed_r3 %>% ready4::get_from_lup_obj(match_value_xx = match_value_xx, 
                match_var_nm_1L_chr = "name", target_var_nm_1L_chr = "main_feature_chr", 
                evaluate_1L_lgl = F) == "PPR"
            if (!is.null(subdivision_1L_chr)) {
                if (subdivision_1L_chr %in% c("New South Wales", 
                  "Victoria") & PPR_logic_1L_lgl) {
                  if (subdivision_1L_chr == "New South Wales") {
                    population_tb <- population_tb %>% dplyr::rename_at(dplyr::vars(dplyr::starts_with("Persons")), 
                      dplyr::funs(paste0("y", year_1L_chr, ".", 
                        gsub("-", ".", .)))) %>% dplyr::rename_at(dplyr::vars(dplyr::ends_with("+")), 
                      dplyr::funs(gsub("\\+", ".pl", .)))
                    if (paste0("y", year_1L_chr, ".Persons.0.14") %in% 
                      names(population_tb)) {
                      divide_large_age_band <- function(x, large_band, 
                        divided_by) ifelse(is.na(x), large_band/divided_by, 
                        x)
                      population_tb <- population_tb %>% dplyr::mutate_at(dplyr::vars(dplyr::ends_with(".0.4"), 
                        dplyr::ends_with(".5.9"), dplyr::ends_with(".10.14")), 
                        divide_large_age_band, large_band = population_tb %>% 
                          dplyr::pull(!!rlang::sym(paste0("y", 
                            year_1L_chr, ".Persons.0.14"))), 
                        divided_by = 3)
                      population_tb <- population_tb %>% dplyr::mutate_at(dplyr::vars(dplyr::ends_with(".15.19"), 
                        dplyr::ends_with(".20.24"), dplyr::ends_with(".25.29"), 
                        dplyr::ends_with(".30.34"), dplyr::ends_with(".35.39"), 
                        dplyr::ends_with(".40.44")), divide_large_age_band, 
                        large_band = population_tb %>% dplyr::pull(!!rlang::sym(paste0("y", 
                          year_1L_chr, ".Persons.15.44"))), divided_by = 6)
                      population_tb <- population_tb %>% dplyr::mutate_at(dplyr::vars(dplyr::ends_with(".45.49"), 
                        dplyr::ends_with(".50.54"), dplyr::ends_with(".55.59"), 
                        dplyr::ends_with(".60.64")), divide_large_age_band, 
                        large_band = population_tb %>% dplyr::pull(!!rlang::sym(paste0("y", 
                          year_1L_chr, ".Persons.45.64"))), divided_by = 4)
                      population_tb <- population_tb %>% dplyr::mutate_at(dplyr::vars(dplyr::ends_with(".65.69"), 
                        dplyr::ends_with(".70.74"), dplyr::ends_with(".75.79"), 
                        dplyr::ends_with(".80.84"), dplyr::ends_with(".85.pl")), 
                        divide_large_age_band, large_band = population_tb %>% 
                          dplyr::pull(!!rlang::sym(paste0("y", 
                            year_1L_chr, ".Persons.65.pl"))), 
                        divided_by = 5)
                      population_tb <- population_tb %>% dplyr::select(-c(!!rlang::sym(paste0("y", 
                        year_1L_chr, ".Persons.0.14")), !!rlang::sym(paste0("y", 
                        year_1L_chr, ".Persons.15.44")), !!rlang::sym(paste0("y", 
                        year_1L_chr, ".Persons.45.64")), !!rlang::sym(paste0("y", 
                        year_1L_chr, ".Persons.65.pl")), year_1L_chr))
                    }
                    else {
                      population_tb <- population_tb %>% dplyr::select(-year_1L_chr)
                    }
                    population_tb <- population_tb %>% dplyr::mutate_if(is.numeric, 
                      .funs = list(Females = ~. * 0.5, Males = ~. * 
                        0.5)) %>% dplyr::rename_at(dplyr::vars(dplyr::ends_with("_Females")), 
                      .funs = dplyr::funs(stringr::str_replace(., 
                        "Persons", "Females") %>% stringr::str_replace("_Females", 
                        ""))) %>% dplyr::rename_at(dplyr::vars(dplyr::ends_with("_Males")), 
                      .funs = dplyr::funs(stringr::str_replace(., 
                        "Persons", "Males") %>% stringr::str_replace("_Males", 
                        ""))) %>% dplyr::select(-dplyr::contains("Persons"))
                  }
                  if (subdivision_1L_chr == "Victoria") {
                    if (!is.na(include_chr[1])) {
                      population_tb <- population_tb %>% dplyr::select(dplyr::one_of(include_chr), 
                        which(grepl("Female|Male", names(population_tb)) & 
                          grepl("0-4|5-9|10-14|15-19|20-24|25-29", 
                            names(population_tb))))
                    }
                    names(population_tb) <- names(population_tb) %>% 
                      gsub("\r\n", "", .) %>% gsub("Females", 
                      paste0("y", year_1L_chr, ".Females"), .) %>% 
                      gsub("Males", paste0("y", year_1L_chr, 
                        ".Males"), .) %>% gsub(" ", ".", .) %>% 
                      gsub("-", ".", .)
                    population_tb <- population_tb %>% dplyr::select(2, 
                      dplyr::contains("Females"), dplyr::contains("Males"))
                    population_tb <- population_tb %>% dplyr::rename_at(names(population_tb)[names(population_tb) %>% 
                      stringr::str_sub(start = -9) == ".and.over"], 
                      dplyr::funs(gsub("and.over", "pl", .)))
                  }
                }
                else {
                  if (PPR_logic_1L_lgl) {
                    population_tb <- population_tb %>% dplyr::rename_at(dplyr::vars(dplyr::starts_with("Male"), 
                      dplyr::starts_with("Female")), dplyr::funs(paste0("y", 
                      year_1L_chr, ".", .)))
                    if (subdivision_1L_chr %in% c("Australian Capital Territory")) 
                      population_tb <- population_tb %>% dplyr::select(-year_1L_chr)
                  }
                }
            }
            object_xx <- population_tb
        }
    }
    else {
        object_xx <- methods::callNextMethod()
    }
    return(object_xx)
})
