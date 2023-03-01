make_abbreviations_lup <- function(){ # Do not migrate
  abbreviations_lup_r3 <- tibble::tibble(long_nm_chr = c("Australian Capital Territory",
                                                         "New South Wales",
                                                         "Northern Territory",
                                                         "Queensland",
                                                         "South Australia",
                                                         "Tasmania",
                                                         "Victoria",
                                                         "Western Australia"),
                                         short_nm_chr = c("ACT", "NSW", "NT", "QLD", "SA","TAS", "VIC", "WA"),
                                         type_chr = "State & Territory") %>%
    vicinity::vicinity_abbreviations()
  return(abbreviations_lup_r3)
}
make_correspondences_lup <- function(){ # Do not migrate
  correspondences_lup <- tibble::tribble(~old_nms_chr, ~new_nms_chr,
                                         "Kingswood", "Kingswood (Penrith - NSW)",
                                         "Kurrajong East", "East Kurrajong",
                                         "Parramatta North", "North Parramatta",
                                         "Penrith South", "South Penrith",
                                         "Maryland", "Maryland (Newcastle - NSW)",
                                         "Ropes Creek", "Ropes Crossing",
                                         "St Clair", "St Clair (Penrith - NSW)",
                                         "Wood Park", "Woodpark") %>%
    ready4show::ready4show_correspondences()
}
make_geom_imports_lup <- function(){ #make_aus_geom_import_lup
  vicinity::vicinity_raw() %>%
    ### CED BOUNDARY IMPORT TABLE
    tibble::add_case(name_chr = NA_character_,# c("aus_ced_nat_shp_bound_2018"),
                     country_chr = "Australia",
                     area_type_chr = c("CED"),
                     region_chr = "National",
                     data_type_chr = "Geometry",
                     main_feature_chr = "Boundary",
                     year_chr = c("2018"),
                     uid_chr = "CED_NAME18",
                     add_boundaries_ls = list("STE_NAME16"),
                     download_url_chr = c("http://www.abs.gov.au/ausstats/subscriber.nsf/log?openagent&1270055003_ced_2018_aust_shp.zip&1270.0.55.003&Data%20Cubes&BF4D23C712D492CFCA2582F600180556&0&July%202018&28.08.2018&Latest"),
                     file_type_chr = ".zip",
                     file_name_chr_chr = c("1270055003_ced_2018_aust_shp"),
                     inc_file_main_chr = c("CED_2018_AUST.shp"),
                     inc_fls_to_rename_ls = list(NA_character_),
                     new_nms_for_inc_fls_ls = list(NA_character_)) %>%
    ### LGA BOUNDARY IMPORT TABLE
    tibble::add_case(name_chr = NA_character_,# c("aus_ced_nat_shp_bound_2018"),
                     country_chr = "Australia",
                     area_type_chr = c("LGA"),
                     region_chr = "National",
                     data_type_chr = "Geometry",
                     main_feature_chr = "Boundary",
                     year_chr = c("2016"),
                     uid_chr = "LGA_NAME16",
                     add_boundaries_ls = list(NULL),
                     download_url_chr = c("http://www.abs.gov.au/ausstats/subscriber.nsf/log?openagent&1270055003_lga_2016_aust_shape.zip&1270.0.55.003&Data%20Cubes&6A6A6E8944937276CA25802C00142DD2&0&July%202016&13.09.2016&Latest"),
                     file_type_chr = ".zip",
                     file_name_chr = c("1270055003_lga_2016_aust_shape"),
                     inc_file_main_chr = c("LGA_2016_AUST.shp"),
                     inc_fls_to_rename_ls = list(NA_character_),
                     new_nms_for_inc_fls_ls = list(NA_character_)) %>%
    ### PHN BOUNDARY IMPORT TABLE
    tibble::add_case(name_chr = NA_character_,# c("aus_ced_nat_shp_bound_2018"),
                     country_chr = "Australia",
                     area_type_chr = c("PHN"),
                     region_chr = "National",
                     data_type_chr = "Geometry",
                     main_feature_chr = "Boundary",
                     year_chr = c("2017"),
                     uid_chr = "PHN_NAME",
                     add_boundaries_ls = list(NULL),
                     download_url_chr = c("http://www.health.gov.au/internet/main/publishing.nsf/Content/20FD74ABB14A1297CA257F150001FD3B/$File/PHN_boundaries_AUS_May2017_V7_Shapefile.zip"),
                     file_type_chr = ".zip",
                     file_name_chr = c("PHN_boundaries_AUS_May2017_V7_Shapefile"),
                     inc_file_main_chr = c("PHN_boundaries_AUS_May2017_V7.shp"),
                     inc_fls_to_rename_ls = list(NA_character_),
                     new_nms_for_inc_fls_ls = list(NA_character_))  %>%
    ### POA BOUNDARY IMPORT TABLE
    tibble::add_case(name_chr = NA_character_, #c("aus_ste_nat_shp_bound_2016"),
                     country_chr = "Australia",
                     area_type_chr = c("POA"),
                     region_chr = "National",
                     data_type_chr = "Geometry",
                     main_feature_chr = "Boundary",
                     year_chr = c("2016"),
                     uid_chr = "POA_NAME16",
                     add_boundaries_ls = list(NULL),
                     download_url_chr = c("https://www.abs.gov.au/ausstats/subscriber.nsf/log?openagent&1270055003_poa_2016_aust_shape.zip&1270.0.55.003&Data%20Cubes&4FB811FA48EECA7ACA25802C001432D0&0&July%202016&13.09.2016&Previous"),
                     file_type_chr = ".zip",
                     file_name_chr = c("1270055003_poa_2016_aust_shape"), #
                     inc_file_main_chr = c("POA_2016_AUST.shp"),
                     inc_fls_to_rename_ls = list(NA_character_),
                     new_nms_for_inc_fls_ls = list(NA_character_)) %>%
    ### SA1 BOUNDARY IMPORT TABLE
    tibble::add_case(name_chr = NA_character_,# c("aus_ced_nat_shp_bound_2018"),
                     country_chr = "Australia",
                     area_type_chr = c("SA1"),
                     region_chr = "National",
                     data_type_chr = "Geometry",
                     main_feature_chr = "Boundary",
                     year_chr = c("2016"),
                     uid_chr = c("SA1_NAME16"),
                     add_boundaries_ls = list(NULL),
                     download_url_chr = c("http://www.abs.gov.au/AUSSTATS/subscriber.nsf/log?openagent&1270055001_sa1_2016_aust_shape.zip&1270.0.55.001&Data%20Cubes&6F308688D810CEF3CA257FED0013C62D&0&July%202016&12.07.2016&Latest"),
                     file_type_chr = ".zip",
                     file_name_chr = c("1270055001_sa1_2016_aust_shape"),
                     inc_file_main_chr = c("SA1_2016_AUST.shp"),
                     inc_fls_to_rename_ls = list(NA_character_),
                     new_nms_for_inc_fls_ls = list(NA_character_)) %>%
    ### SA2 BOUNDARY IMPORT TABLE
    tibble::add_case(name_chr = NA_character_,# c("aus_ced_nat_shp_bound_2018"),
                     country_chr = "Australia",
                     area_type_chr = c("SA2"),
                     region_chr = "National",
                     data_type_chr = "Geometry",
                     main_feature_chr = "Boundary",
                     year_chr = c("2016"),
                     uid_chr = c("SA2_NAME16"),
                     add_boundaries_ls = list(NULL),
                     download_url_chr = c("http://www.abs.gov.au/AUSSTATS/subscriber.nsf/log?openagent&1270055001_sa2_2016_aust_shape.zip&1270.0.55.001&Data%20Cubes&A09309ACB3FA50B8CA257FED0013D420&0&July%202016&12.07.2016&Latest"),
                     file_type_chr = ".zip",
                     file_name_chr = c("1270055001_sa2_2016_aust_shape"),
                     inc_file_main_chr = c("SA2_2016_AUST.shp"),
                     inc_fls_to_rename_ls = list(NA_character_),
                     new_nms_for_inc_fls_ls = list(NA_character_)) %>%
    ### SA3 BOUNDARY IMPORT TABLE
    tibble::add_case(name_chr = NA_character_,# c("aus_ced_nat_shp_bound_2018"),
                     country_chr = "Australia",
                     area_type_chr = c("SA3"),
                     region_chr = "National",
                     data_type_chr = "Geometry",
                     main_feature_chr = "Boundary",
                     year_chr = c("2011","2016"),
                     uid_chr = c("SA3_NAME11", "SA3_NAME16"),
                     add_boundaries_ls = list(NULL),
                     download_url_chr = c("http://www.abs.gov.au/ausstats/subscriber.nsf/log?openagent&1270055001_sa3_2011_aust_shape.zip&1270.0.55.001&Data%20Cubes&AD2BD90E5DC0F4C7CA257801000D59E3&0&July%202011&23.12.2010&Latest",
                                          "http://www.abs.gov.au/AUSSTATS/subscriber.nsf/log?openagent&1270055001_sa3_2016_aust_shape.zip&1270.0.55.001&Data%20Cubes&43942523105745CBCA257FED0013DB07&0&July%202016&12.07.2016&Latest"),
                     file_type_chr = ".zip",
                     file_name_chr = c("1270055001_sa3_2011_aust_shape.zip", "1270055001_sa3_2016_aust_shape.zip"),
                     inc_file_main_chr = c("SA3_2011_AUST.shp", "SA3_2016_AUST.shp"),
                     inc_fls_to_rename_ls = list(NA_character_),
                     new_nms_for_inc_fls_ls = list(NA_character_)) %>%
    ### SSC BOUNDARY IMPORT TABLE
    tibble::add_case(name_chr = NA_character_,# c("aus_ced_nat_shp_bound_2018"),
                     country_chr = "Australia",
                     area_type_chr = c("SSC"),
                     region_chr = "National",
                     data_type_chr = "Geometry",
                     main_feature_chr = "Boundary",
                     year_chr = c("2016"),
                     uid_chr = "SSC_NAME16",
                     add_boundaries_ls = list(NULL),
                     download_url_chr = c("https://www.abs.gov.au/ausstats/subscriber.nsf/log?openagent&1270055003_ssc_2016_aust_shape.zip&1270.0.55.003&Data%20Cubes&5698C77C925DC4FACA25802C001439C5&0&July%202016&13.09.2016&Previous"),
                     file_type_chr = ".zip",
                     file_name_chr = c("1270055003_ssc_2016_aust_shape"),#
                     inc_file_main_chr = c("SSC_2016_AUST.shp"),
                     inc_fls_to_rename_ls = list(NA_character_),
                     new_nms_for_inc_fls_ls = list(NA_character_)) %>%
    ### STE BOUNDARY IMPORT TABLE
    tibble::add_case(name_chr = NA_character_, #c("aus_ste_nat_shp_bound_2016"),
                     country_chr = "Australia",
                     area_type_chr = c("STE"),
                     region_chr = "National",
                     data_type_chr = "Geometry",
                     main_feature_chr = "Boundary",
                     year_chr = c("2016"),
                     uid_chr = "STE_NAME16",
                     add_boundaries_ls = list(NULL),
                     download_url_chr = c("http://www.abs.gov.au/AUSSTATS/subscriber.nsf/log?openagent&1270055001_ste_2016_aust_shape.zip&1270.0.55.001&Data%20Cubes&65819049BE2EB089CA257FED0013E865&0&July%202016&12.07.2016&Latest"),
                     file_type_chr = ".zip",
                     file_name_chr = c("1270055001_ste_2016_aust_shape.zip"),
                     inc_file_main_chr = c("STE_2016_AUST.shp"),
                     inc_fls_to_rename_ls = list(NA_character_),
                     new_nms_for_inc_fls_ls = list(NA_character_)) %>%
    ### SMH BOUNDARY IMPORT TABLE (Orygen only)
    tibble::add_case(name_chr = NA_character_,
                     country_chr = "Australia",
                     area_type_chr = c("SMH"),
                     region_chr = "Victoria",
                     data_type_chr = "Geometry",
                     main_feature_chr = "Boundary",
                     year_chr = c("2016"),
                     uid_chr = c("SMH_NAME16"),
                     add_boundaries_ls = list(c("LGA_NAME16","SA1_NAME16")),
                     local_file_src_chr = NA_character_,
                     path_to_make_script_chr = "add_aus_oyh_data",
                     download_url_chr = NA_character_,
                     file_type_chr = NA_character_,
                     file_name_chr = NA_character_,
                     inc_file_main_chr = NA_character_,
                     inc_fls_to_rename_ls = list(NA_character_),
                     new_nms_for_inc_fls_ls = list(NA_character_)) %>%
    tibble::add_case(name_chr = NA_character_,
                     country_chr = "Australia",
                     area_type_chr = c("HSS"),
                     region_chr = "National",
                     data_type_chr = "Geometry",
                     main_feature_chr = "PNT",
                     year_chr = c("2019"),
                     uid_chr = NA_character_,
                     add_boundaries_ls = list(c("PHN_NAME","STE_NAME16")),
                     local_file_src_chr = "data-raw/hss_pnt_dp.csv",
                     path_to_make_script_chr = "add_aus_hss_data",
                     download_url_chr = NA_character_,
                     file_type_chr = NA_character_,
                     file_name_chr = NA_character_,
                     inc_file_main_chr = NA_character_,
                     inc_fls_to_rename_ls = list(NA_character_),
                     new_nms_for_inc_fls_ls = list(NA_character_)) %>%
    dplyr::mutate_if(is.list, .funs = ~ purrr::map(.,~if(all(is.na(.x))){NULL}else{.x}))
}
make_no_stat_ls <- function(area_atts_ls,
                            area_geoms_ls,
                            feature_pfx_1L_chr,
                            land_sf,
                            land_name_1L_chr,
                            statistic_sfs_ls,
                            uids_chr,
                            var_nms_chr){
  no_stat_ls <- purrr::pmap(list(a = area_geoms_ls,
                                 b = area_atts_ls,
                                 c = statistic_sfs_ls,
                                 d = var_nms_chr),
                            ~   make_spine_no_stat_ls(area_atts_chr = ..2,
                                                      area_geoms_chr = ..1,
                                                      land_sf = land_sf,
                                                      statistic_sf =  ..3,
                                                      land_name_1L_chr = land_name_1L_chr,
                                                      stat_name_str = ..4))
  no_stat_ls <- purrr::map(no_stat_ls, ~ remove_empty_tbs(.x)) %>%
    remove_empty_lists() %>% purrr::flatten()
  no_stat_ls %>% purrr::map(~ remove_feature_no_stat_sf(no_stat_sf = .x,
                                                        feature_pfx_1L_chr = feature_pfx_1L_chr,
                                                        uids_chr = uids_chr))
}
make_spine_no_stat_ls <- function(area_atts_chr,
                                  area_geoms_chr,
                                  land_sf,
                                  statistic_sf,
                                  land_name_1L_chr,
                                  stat_name_str){
  purrr::map2(area_geoms_chr,
              area_atts_chr,
              ~ get_no_stat_areas(land_sf = land_sf,
                                  statistic_sf =  statistic_sf,
                                  land_name_1L_chr = land_name_1L_chr,
                                  land_val_vec = .x,
                                  stat_name_str = stat_name_str,
                                  stat_val_vec = .y)) %>%
    stats::setNames(area_geoms_chr)
}
# make_seifa_summary <- function(seifa_sf,#summarise_seifa # Not used
#                             group_by_1L_chr,
#                             area_by_1L_chr){
#   unitnamesym_xx <- rlang::sym(area_by_1L_chr)
#   seifa_sf <- seifa_sf %>%
#     dplyr::mutate(part.pop.weighting=Usual.Res.Pop/resident.pop.all.parts) %>%
#     dplyr::mutate(Percentile.Australia=Percentile.Australia*part.pop.weighting) %>%
#     dplyr::group_by(!!group_by_1L_chr) %>%
#     dplyr::summarize(!!area_by_1L_chr := dplyr::first((!!unitnamesym_xx)),
#                      STE_NAME16 = dplyr::first(STE_NAME16),
#                      AREASQKM16 = mean(AREASQKM16),
#                      seifa.percentile = sum(Percentile.Australia))
#   return(seifa_sf)
# }
