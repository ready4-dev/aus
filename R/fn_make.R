#' Make abbreviations lookup table
#' @description make_abbreviations_lup() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make abbreviations lookup table. The function returns Abbreviations (a ready4 S3 extension of lookup table).

#' @return Abbreviations (a ready4 S3 extension of lookup table)
#' @rdname make_abbreviations_lup
#' @export 
#' @importFrom tibble tibble
#' @importFrom vicinity vicinity_abbreviations
#' @keywords internal
make_abbreviations_lup <- function () 
{
    abbreviations_lup_r3 <- tibble::tibble(long_nm_chr = c("Australian Capital Territory", 
        "New South Wales", "Northern Territory", "Queensland", 
        "South Australia", "Tasmania", "Victoria", "Western Australia"), 
        short_nm_chr = c("ACT", "NSW", "NT", "QLD", "SA", "TAS", 
            "VIC", "WA"), type_chr = "State & Territory") %>% 
        vicinity::vicinity_abbreviations()
    return(abbreviations_lup_r3)
}
#' Make correspondences lookup table
#' @description make_correspondences_lup() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make correspondences lookup table. The function is called for its side effects and does not return a value.

#' @return NULL
#' @rdname make_correspondences_lup
#' @export 
#' @importFrom tibble tribble
#' @importFrom ready4show ready4show_correspondences
#' @keywords internal
make_correspondences_lup <- function () 
{
    correspondences_lup <- tibble::tribble(~old_nms_chr, ~new_nms_chr, 
        "Kingswood", "Kingswood (Penrith - NSW)", "Kurrajong East", 
        "East Kurrajong", "Parramatta North", "North Parramatta", 
        "Penrith South", "South Penrith", "Maryland", "Maryland (Newcastle - NSW)", 
        "Ropes Creek", "Ropes Crossing", "St Clair", "St Clair (Penrith - NSW)", 
        "Wood Park", "Woodpark") %>% ready4show::ready4show_correspondences()
}
#' Make geometry imports lookup table
#' @description make_geom_imports_lup() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make geometry imports lookup table. The function is called for its side effects and does not return a value.

#' @return NULL
#' @rdname make_geom_imports_lup
#' @export 
#' @importFrom vicinity vicinity_raw
#' @importFrom tibble add_case
#' @importFrom dplyr mutate_if
#' @importFrom purrr map
#' @keywords internal
make_geom_imports_lup <- function () 
{
    vicinity::vicinity_raw() %>% tibble::add_case(name_chr = NA_character_, 
        country_chr = "Australia", area_type_chr = c("CED"), 
        region_chr = "National", data_type_chr = "Geometry", 
        main_feature_chr = "Boundary", year_chr = c("2018"), 
        uid_chr = "CED_NAME18", add_boundaries_ls = list("STE_NAME16"), 
        download_url_chr = c("http://www.abs.gov.au/ausstats/subscriber.nsf/log?openagent&1270055003_ced_2018_aust_shp.zip&1270.0.55.003&Data%20Cubes&BF4D23C712D492CFCA2582F600180556&0&July%202018&28.08.2018&Latest"), 
        file_type_chr = ".zip", file_name_chr_chr = c("1270055003_ced_2018_aust_shp"), 
        inc_file_main_chr = c("CED_2018_AUST.shp"), inc_fls_to_rename_ls = list(NA_character_), 
        new_nms_for_inc_fls_ls = list(NA_character_)) %>% tibble::add_case(name_chr = NA_character_, 
        country_chr = "Australia", area_type_chr = c("LGA"), 
        region_chr = "National", data_type_chr = "Geometry", 
        main_feature_chr = "Boundary", year_chr = c("2016"), 
        uid_chr = "LGA_NAME16", add_boundaries_ls = list(NULL), 
        download_url_chr = c("http://www.abs.gov.au/ausstats/subscriber.nsf/log?openagent&1270055003_lga_2016_aust_shape.zip&1270.0.55.003&Data%20Cubes&6A6A6E8944937276CA25802C00142DD2&0&July%202016&13.09.2016&Latest"), 
        file_type_chr = ".zip", file_name_chr = c("1270055003_lga_2016_aust_shape"), 
        inc_file_main_chr = c("LGA_2016_AUST.shp"), inc_fls_to_rename_ls = list(NA_character_), 
        new_nms_for_inc_fls_ls = list(NA_character_)) %>% tibble::add_case(name_chr = NA_character_, 
        country_chr = "Australia", area_type_chr = c("PHN"), 
        region_chr = "National", data_type_chr = "Geometry", 
        main_feature_chr = "Boundary", year_chr = c("2017"), 
        uid_chr = "PHN_NAME", add_boundaries_ls = list(NULL), 
        download_url_chr = c("http://www.health.gov.au/internet/main/publishing.nsf/Content/20FD74ABB14A1297CA257F150001FD3B/$File/PHN_boundaries_AUS_May2017_V7_Shapefile.zip"), 
        file_type_chr = ".zip", file_name_chr = c("PHN_boundaries_AUS_May2017_V7_Shapefile"), 
        inc_file_main_chr = c("PHN_boundaries_AUS_May2017_V7.shp"), 
        inc_fls_to_rename_ls = list(NA_character_), new_nms_for_inc_fls_ls = list(NA_character_)) %>% 
        tibble::add_case(name_chr = NA_character_, country_chr = "Australia", 
            area_type_chr = c("POA"), region_chr = "National", 
            data_type_chr = "Geometry", main_feature_chr = "Boundary", 
            year_chr = c("2016"), uid_chr = "POA_NAME16", add_boundaries_ls = list(NULL), 
            download_url_chr = c("https://www.abs.gov.au/ausstats/subscriber.nsf/log?openagent&1270055003_poa_2016_aust_shape.zip&1270.0.55.003&Data%20Cubes&4FB811FA48EECA7ACA25802C001432D0&0&July%202016&13.09.2016&Previous"), 
            file_type_chr = ".zip", file_name_chr = c("1270055003_poa_2016_aust_shape"), 
            inc_file_main_chr = c("POA_2016_AUST.shp"), inc_fls_to_rename_ls = list(NA_character_), 
            new_nms_for_inc_fls_ls = list(NA_character_)) %>% 
        tibble::add_case(name_chr = NA_character_, country_chr = "Australia", 
            area_type_chr = c("SA1"), region_chr = "National", 
            data_type_chr = "Geometry", main_feature_chr = "Boundary", 
            year_chr = c("2016"), uid_chr = c("SA1_NAME16"), 
            add_boundaries_ls = list(NULL), download_url_chr = c("http://www.abs.gov.au/AUSSTATS/subscriber.nsf/log?openagent&1270055001_sa1_2016_aust_shape.zip&1270.0.55.001&Data%20Cubes&6F308688D810CEF3CA257FED0013C62D&0&July%202016&12.07.2016&Latest"), 
            file_type_chr = ".zip", file_name_chr = c("1270055001_sa1_2016_aust_shape"), 
            inc_file_main_chr = c("SA1_2016_AUST.shp"), inc_fls_to_rename_ls = list(NA_character_), 
            new_nms_for_inc_fls_ls = list(NA_character_)) %>% 
        tibble::add_case(name_chr = NA_character_, country_chr = "Australia", 
            area_type_chr = c("SA2"), region_chr = "National", 
            data_type_chr = "Geometry", main_feature_chr = "Boundary", 
            year_chr = c("2016"), uid_chr = c("SA2_NAME16"), 
            add_boundaries_ls = list(NULL), download_url_chr = c("http://www.abs.gov.au/AUSSTATS/subscriber.nsf/log?openagent&1270055001_sa2_2016_aust_shape.zip&1270.0.55.001&Data%20Cubes&A09309ACB3FA50B8CA257FED0013D420&0&July%202016&12.07.2016&Latest"), 
            file_type_chr = ".zip", file_name_chr = c("1270055001_sa2_2016_aust_shape"), 
            inc_file_main_chr = c("SA2_2016_AUST.shp"), inc_fls_to_rename_ls = list(NA_character_), 
            new_nms_for_inc_fls_ls = list(NA_character_)) %>% 
        tibble::add_case(name_chr = NA_character_, country_chr = "Australia", 
            area_type_chr = c("SA3"), region_chr = "National", 
            data_type_chr = "Geometry", main_feature_chr = "Boundary", 
            year_chr = c("2011", "2016"), uid_chr = c("SA3_NAME11", 
                "SA3_NAME16"), add_boundaries_ls = list(NULL), 
            download_url_chr = c("http://www.abs.gov.au/ausstats/subscriber.nsf/log?openagent&1270055001_sa3_2011_aust_shape.zip&1270.0.55.001&Data%20Cubes&AD2BD90E5DC0F4C7CA257801000D59E3&0&July%202011&23.12.2010&Latest", 
                "http://www.abs.gov.au/AUSSTATS/subscriber.nsf/log?openagent&1270055001_sa3_2016_aust_shape.zip&1270.0.55.001&Data%20Cubes&43942523105745CBCA257FED0013DB07&0&July%202016&12.07.2016&Latest"), 
            file_type_chr = ".zip", file_name_chr = c("1270055001_sa3_2011_aust_shape.zip", 
                "1270055001_sa3_2016_aust_shape.zip"), inc_file_main_chr = c("SA3_2011_AUST.shp", 
                "SA3_2016_AUST.shp"), inc_fls_to_rename_ls = list(NA_character_), 
            new_nms_for_inc_fls_ls = list(NA_character_)) %>% 
        tibble::add_case(name_chr = NA_character_, country_chr = "Australia", 
            area_type_chr = c("SSC"), region_chr = "National", 
            data_type_chr = "Geometry", main_feature_chr = "Boundary", 
            year_chr = c("2016"), uid_chr = "SSC_NAME16", add_boundaries_ls = list(NULL), 
            download_url_chr = c("https://www.abs.gov.au/ausstats/subscriber.nsf/log?openagent&1270055003_ssc_2016_aust_shape.zip&1270.0.55.003&Data%20Cubes&5698C77C925DC4FACA25802C001439C5&0&July%202016&13.09.2016&Previous"), 
            file_type_chr = ".zip", file_name_chr = c("1270055003_ssc_2016_aust_shape"), 
            inc_file_main_chr = c("SSC_2016_AUST.shp"), inc_fls_to_rename_ls = list(NA_character_), 
            new_nms_for_inc_fls_ls = list(NA_character_)) %>% 
        tibble::add_case(name_chr = NA_character_, country_chr = "Australia", 
            area_type_chr = c("STE"), region_chr = "National", 
            data_type_chr = "Geometry", main_feature_chr = "Boundary", 
            year_chr = c("2016"), uid_chr = "STE_NAME16", add_boundaries_ls = list(NULL), 
            download_url_chr = c("http://www.abs.gov.au/AUSSTATS/subscriber.nsf/log?openagent&1270055001_ste_2016_aust_shape.zip&1270.0.55.001&Data%20Cubes&65819049BE2EB089CA257FED0013E865&0&July%202016&12.07.2016&Latest"), 
            file_type_chr = ".zip", file_name_chr = c("1270055001_ste_2016_aust_shape.zip"), 
            inc_file_main_chr = c("STE_2016_AUST.shp"), inc_fls_to_rename_ls = list(NA_character_), 
            new_nms_for_inc_fls_ls = list(NA_character_)) %>% 
        tibble::add_case(name_chr = NA_character_, country_chr = "Australia", 
            area_type_chr = c("SMH"), region_chr = "Victoria", 
            data_type_chr = "Geometry", main_feature_chr = "Boundary", 
            year_chr = c("2016"), uid_chr = c("SMH_NAME16"), 
            add_boundaries_ls = list(c("LGA_NAME16", "SA1_NAME16")), 
            local_file_src_chr = NA_character_, path_to_make_script_chr = "add_aus_oyh_data", 
            download_url_chr = NA_character_, file_type_chr = NA_character_, 
            file_name_chr = NA_character_, inc_file_main_chr = NA_character_, 
            inc_fls_to_rename_ls = list(NA_character_), new_nms_for_inc_fls_ls = list(NA_character_)) %>% 
        tibble::add_case(name_chr = NA_character_, country_chr = "Australia", 
            area_type_chr = c("HSS"), region_chr = "National", 
            data_type_chr = "Geometry", main_feature_chr = "PNT", 
            year_chr = c("2019"), uid_chr = NA_character_, add_boundaries_ls = list(c("PHN_NAME", 
                "STE_NAME16")), local_file_src_chr = "data-raw/hss_pnt_dp.csv", 
            path_to_make_script_chr = "add_aus_hss_data", download_url_chr = NA_character_, 
            file_type_chr = NA_character_, file_name_chr = NA_character_, 
            inc_file_main_chr = NA_character_, inc_fls_to_rename_ls = list(NA_character_), 
            new_nms_for_inc_fls_ls = list(NA_character_)) %>% 
        dplyr::mutate_if(is.list, .funs = ~purrr::map(., ~if (all(is.na(.x))) {
            NULL
        }
        else {
            .x
        }))
}
#' Make no statistic list
#' @description make_no_stat_ls() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make no statistic list. The function is called for its side effects and does not return a value.
#' @param area_atts_ls Area attributes (a list)
#' @param area_geoms_ls Area geometries (a list)
#' @param feature_pfx_1L_chr Feature prefix (a character vector of length one)
#' @param land_sf Land (a simple features object)
#' @param land_name_1L_chr Land name (a character vector of length one)
#' @param statistic_sfs_ls Statistic simple features objects (a list)
#' @param uids_chr Unique identifiers (a character vector)
#' @param var_nms_chr Variable names (a character vector)
#' @return NULL
#' @rdname make_no_stat_ls
#' @export 
#' @importFrom purrr pmap map flatten
#' @keywords internal
make_no_stat_ls <- function (area_atts_ls, area_geoms_ls, feature_pfx_1L_chr, land_sf, 
    land_name_1L_chr, statistic_sfs_ls, uids_chr, var_nms_chr) 
{
    no_stat_ls <- purrr::pmap(list(a = area_geoms_ls, b = area_atts_ls, 
        c = statistic_sfs_ls, d = var_nms_chr), ~make_spine_no_stat_ls(area_atts_chr = ..2, 
        area_geoms_chr = ..1, land_sf = land_sf, statistic_sf = ..3, 
        land_name_1L_chr = land_name_1L_chr, stat_name_str = ..4))
    no_stat_ls <- purrr::map(no_stat_ls, ~remove_empty_tbs(.x)) %>% 
        remove_empty_lists() %>% purrr::flatten()
    no_stat_ls %>% purrr::map(~remove_feature_no_stat_sf(no_stat_sf = .x, 
        feature_pfx_1L_chr = feature_pfx_1L_chr, uids_chr = uids_chr))
}
#' Make spine no statistic list
#' @description make_spine_no_stat_ls() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make spine no statistic list. The function is called for its side effects and does not return a value.
#' @param area_atts_chr Area attributes (a character vector)
#' @param area_geoms_chr Area geometries (a character vector)
#' @param land_sf Land (a simple features object)
#' @param statistic_sf Statistic (a simple features object)
#' @param land_name_1L_chr Land name (a character vector of length one)
#' @param stat_name_str PARAM_DESCRIPTION
#' @return NULL
#' @rdname make_spine_no_stat_ls
#' @export 
#' @importFrom purrr map2
#' @importFrom stats setNames
#' @keywords internal
make_spine_no_stat_ls <- function (area_atts_chr, area_geoms_chr, land_sf, statistic_sf, 
    land_name_1L_chr, stat_name_str) 
{
    purrr::map2(area_geoms_chr, area_atts_chr, ~get_no_stat_areas(land_sf = land_sf, 
        statistic_sf = statistic_sf, land_name_1L_chr = land_name_1L_chr, 
        land_val_vec = .x, stat_name_str = stat_name_str, stat_val_vec = .y)) %>% 
        stats::setNames(area_geoms_chr)
}
