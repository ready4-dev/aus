#' 
#' Author and save files
#' @name author-AusACT
#' @description author method applied to AusACT
#' @param x An object of class AusACT
#' @param what_1L_chr What (a character vector of length one), Default: 'ppr_2018'
#' @return Y (Look up tables for spatiotemporal data)
#' @rdname author-methods
#' @aliases author,AusACT-method
#' @export 
#' @importFrom vicinity VicinityLocalRaw
#' @importFrom dplyr mutate
#' @importFrom purrr map2_chr
#' @importFrom ready4 author
methods::setMethod("author", "AusACT", function (x, what_1L_chr = "ppr_2018") 
{
    if (what_1L_chr == "ppr_2018") {
        path_1L_chr <- paste0(x@processed_fls_dir_1L_chr, "/", 
            x@a_VicinityLookup@vicinity_raw_r3$name_chr, ".RDS")
        processed_file_exists_lgl <- file.exists(path_1L_chr)
        y_VicinityLookup <- vicinity::VicinityLocalRaw(merge_itms_chr = x@merge_itms_chr, 
            raw_fls_dir_1L_chr = x@raw_fls_dir_1L_chr, pkg_1L_chr = x@pkg_1L_chr, 
            overwrite_1L_lgl = x@overwrite_1L_lgl, write_1L_lgl = x@write_1L_lgl, 
            a_VicinityLookup = x@a_VicinityLookup) %>% author(processed_fls_dir_1L_chr_chr = x@processed_fls_dir_1L_chr, 
            crs_nbr_dbl = x@crs_nbr_dbl)
        if (x@overwrite_1L_lgl | !processed_file_exists_lgl) {
            new_act_ppr_ls <- add_year_2016_to_ACT_ppr(aus_sa3_ACT_ppr_2018_tb = path_1L_chr %>% 
                readRDS(), aus_sa2_ACT_erp_age_sex_2016_tb = parse(text = x@merge_itms_chr) %>% 
                eval())
            saveRDS(new_act_ppr_ls, file = path_1L_chr)
        }
        y_VicinityLookup <- renewSlot(y_VicinityLookup, "vicinity_processed_r3", 
            y_VicinityLookup@vicinity_processed_r3 %>% dplyr::mutate(year_start_chr = purrr::map2_chr(name_chr, 
                year_start_chr, ~ifelse(.x == "aus_sa3_ACT_ppr_2018_tb", 
                  "2016", .y))))
    }
    return(y_VicinityLookup)
})
#' 
#' Author and save files
#' @name author-AusHeadspace
#' @description author method applied to AusHeadspace
#' @param x An object of class AusHeadspace
#' @return Y (Look up tables for spatiotemporal data)
#' @rdname author-methods
#' @aliases author,AusHeadspace-method
#' @export 
#' @importFrom ready4use assert_single_row_tb assert_matches_chr
#' @importFrom dplyr mutate pull select mutate_at inner_join
#' @importFrom purrr pluck reduce map2_chr
#' @importFrom vicinity make_paths_to_fls_for_ingest make_intersecting_geometries vicinity_points
#' @importFrom stringr str_sub
#' @importFrom sf st_as_sf st_set_geometry
#' @importFrom tibble as_tibble
#' @importFrom stats setNames
#' @importFrom ready4 author
methods::setMethod("author", "AusHeadspace", function (x) 
{
    y_VicinityLookup <- x@a_VicinityLookup
    vicinity_raw_r3 <- y_VicinityLookup@vicinity_raw_r3
    ready4use::assert_single_row_tb(vicinity_raw_r3)
    import_type_ls <- procure(vicinity_raw_r3)
    ready4use::assert_matches_chr(import_type_ls[[1]], "Aus_16_31_hss")
    dv_import_r3 <- manufacture(vicinity_raw_r3)
    hss_lup_r3_pt1 <- dv_import_r3 %>% dplyr::mutate(data_repo_ui_chr = NA_character_) %>% 
        procure()
    author(vicinity_raw_r3, match_vals_xx = vicinity_raw_r3 %>% 
        dplyr::pull(name_chr), path_1L_chr = paste0(x@raw_fls_dir_1L_chr, 
        "/Geometries"), overwrite_1L_lgl = x@overwrite_1L_lgl)
    bndys_to_add_chr <- vicinity_raw_r3 %>% dplyr::pull(add_boundaries) %>% 
        purrr::pluck(1)
    path_to_centres_sf_1L_chr <- vicinity::make_paths_to_fls_for_ingest(processed_fls_dir_1L_chr = x@processed_fls_dir_1L_chr, 
        name_chr = vicinity_raw_r3$name_chr, data_type_chr = "Geometry")
    path_to_seed_sf_1L_chr <- x@merge_itms_chr[2] %>% stringr::str_sub(start = 10, 
        end = -3)
    x <- renewSlot(x, "path_to_seed_sf_1L_chr", path_to_seed_sf_1L_chr)
    y_VicinityLookup <- renew(y_VicinityLookup, path_1L_chr = x@path_to_seed_sf_1L_chr, 
        what_1L_chr = "templates") %>% renew(what_1L_chr = "identifiers")
    inc_cols_vec <- c(names(hss_lup_r3_pt1)[!names(hss_lup_r3_pt1) %in% 
        c("lng_dbl", "lat_dbl")], bndys_to_add_chr)
    if (x@overwrite_1L_lgl | !file.exists(path_to_centres_sf_1L_chr)) {
        centres_sf <- purrr::reduce(x@merge_itms_chr, .init = hss_lup_r3_pt1 %>% 
            sf::st_as_sf(coords = c("long", "lat"), crs = x@crs_nbr_dbl[1]), 
            ~vicinity::make_intersecting_geometries(geometry_one_sf = .x, 
                geometry_two_sf = eval(parse(text = .y)), crs_nbr_dbl = x@crs_nbr_dbl, 
                validate_1L_lgl = F))
        saveRDS(centres_sf, file = path_to_centres_sf_1L_chr)
    }
    else {
        centres_sf <- paste0(path_to_centres_sf_1L_chr) %>% readRDS()
    }
    hss_lup_r3_pt2 <- centres_sf %>% dplyr::select(inc_cols_vec) %>% 
        sf::st_set_geometry(NULL) %>% dplyr::mutate_at(.vars = bndys_to_add_chr, 
        .funs = as.character())
    hss_lup_r3 <- dplyr::inner_join(hss_lup_r3_pt1, hss_lup_r3_pt2) %>% 
        tibble::as_tibble() %>% dplyr::mutate_at(.vars = inc_cols_vec, 
        .funs = as.character) %>% vicinity::vicinity_points()
    y_VicinityLookup <- renewSlot(y_VicinityLookup, "vicinity_points_r3", 
        hss_lup_r3)
    y_VicinityLookup <- y_VicinityLookup %>% renew(template_ls = list(centres_sf) %>% 
        stats::setNames(vicinity_raw_r3$name_chr), tbl_data_type_1L_chr = "Geometry", 
        package_1L_chr = x@pkg_1L_chr, what_1L_chr = "processed")
    y_VicinityLookup <- renewSlot(y_VicinityLookup, "vicinity_processed_r3", 
        y_VicinityLookup@vicinity_processed_r3 %>% dplyr::mutate(source_reference_chr = purrr::map2_chr(area_type_chr, 
            source_reference_chr, ~ifelse(.x == "HSS", paste0(.y, 
                "_sf"), .y))))
    return(y_VicinityLookup)
})
#' 
#' Author and save files
#' @name author-AusProjections
#' @description author method applied to AusProjections
#' @param x An object of class AusProjections
#' @return Y (Look up tables for spatiotemporal data)
#' @rdname author-methods
#' @aliases author,AusProjections-method
#' @export 
#' @importFrom ready4use assert_single_row_tb assert_matches_chr
#' @importFrom vicinity make_paths_to_fls_for_ingest get_name_from_path_chr make_valid_new_sf
#' @importFrom dplyr pull select mutate filter rename
#' @importFrom purrr map map_lgl
#' @importFrom tibble rownames_to_column add_row
#' @importFrom stats setNames
#' @importFrom ready4 author
methods::setMethod("author", "AusProjections", function (x) 
{
    y_VicinityLookup <- x@a_VicinityLookup
    z_vicinity_raw <- y_VicinityLookup@vicinity_raw_r3
    ready4use::assert_single_row_tb(z_vicinity_raw)
    import_type_ls <- procure(z_vicinity_raw)
    ready4use::assert_matches_chr(import_type_ls[[1]], "Aus_16_31_xx1")
    path_to_seed_sf_1L_chr <- vicinity::make_paths_to_fls_for_ingest(processed_fls_dir_1L_chr = x@processed_fls_dir_1L_chr, 
        name_chr = z_vicinity_raw$name_chr, data_type_chr = "Geometry")
    x <- renewSlot(x, "path_to_seed_sf_1L_chr", path_to_seed_sf_1L_chr)
    y_VicinityLookup <- renew(y_VicinityLookup, path_1L_chr = x@path_to_seed_sf_1L_chr, 
        what_1L_chr = "templates") %>% renew(what_1L_chr = "identifiers")
    starter_sf_name <- vicinity::get_name_from_path_chr(path_to_seed_sf_1L_chr, 
        with_ext_1L_lgl = F)
    if (x@overwrite_1L_lgl | !file.exists(path_to_seed_sf_1L_chr)) {
        aus_ste_nat_bnd_2016_sf <- eval(parse(text = x@merge_itms_chr[1]))
        aus_lga_nat_bnd_2016_sf <- eval(parse(text = x@merge_itms_chr[2]))
        aus_lga_nat_bnd_2011_sf <- eval(parse(text = x@merge_itms_chr[3]))
        aus_sa3_nat_bnd_2016_sf <- eval(parse(text = x@merge_itms_chr[4]))
        lga_2016_states_vec <- c("New South Wales", "Queensland", 
            "Western Australia")
        lga_2011_states_vec <- c("South Australia", "Tasmania", 
            "Victoria")
        sa3_2016_states_vec <- c("Australian Capital Territory", 
            "Northern Territory")
        states_vec <- aus_ste_nat_bnd_2016_sf %>% dplyr::pull(STE_NAME16) %>% 
            as.character() %>% unique()
        var_nms_chr <- c("STE_NAME16", "STE_NAME11", "STE_NAME16")
        uids_chr <- c("LGA_NAME16", "LGA_NAME11", "SA3_NAME16")
        no_stat_ls <- make_no_stat_ls(area_geoms_ls = list(lga_2016_states_vec, 
            lga_2011_states_vec, sa3_2016_states_vec), area_atts_ls = list(lga_2016_states_vec, 
            lga_2011_states_vec, sa3_2016_states_vec), statistic_sfs_ls = list(aus_lga_nat_bnd_2016_sf, 
            aus_lga_nat_bnd_2011_sf, aus_sa3_nat_bnd_2016_sf), 
            var_nms_chr = var_nms_chr, land_sf = aus_ste_nat_bnd_2016_sf, 
            land_name_1L_chr = "STE_NAME16", feature_pfx_1L_chr = "No usual address", 
            uids_chr = uids_chr)
        no_stat_sf <- purrr::map(no_stat_ls, ~.x %>% dplyr::select(-c("STE_CODE11", 
            "STE_NAME11", "AREA_SQKM", "AREASQKM16", "STE_CODE16")) %>% 
            dplyr::mutate(LGA_CODE16 = NA_character_, LGA_NAME16 = NA_character_, 
                SA3_CODE16 = NA_character_, SA3_NAME16 = NA_character_)) %>% 
            do.call(what = rbind)
        full_coverage_uids_vec <- uids_chr[intersect(uids_chr, 
            names(no_stat_sf)) %>% purrr::map_lgl(~no_stat_sf %>% 
            dplyr::pull(.x) %>% is.na() %>% all())]
        starter_sf <- list(lga_2016_sf = aus_lga_nat_bnd_2016_sf %>% 
            dplyr::filter(STE_NAME16 %in% lga_2016_states_vec) %>% 
            dplyr::select(-c("STE_CODE16", "AREASQKM16")) %>% 
            dplyr::mutate(LGA_CODE11 = NA_character_, LGA_NAME11 = NA_character_, 
                SA3_CODE16 = NA_character_, SA3_NAME16 = NA_character_), 
            lga_2011_sf = aus_lga_nat_bnd_2011_sf %>% dplyr::filter(STE_NAME11 %in% 
                lga_2011_states_vec) %>% dplyr::select(-c("STE_CODE11", 
                "AREA_SQKM")) %>% dplyr::mutate(LGA_CODE16 = NA_character_, 
                LGA_NAME16 = NA_character_, SA3_CODE16 = NA_character_, 
                SA3_NAME16 = NA_character_) %>% dplyr::rename(STE_NAME16 = STE_NAME11), 
            sa3_2016_sf = aus_sa3_nat_bnd_2016_sf %>% dplyr::filter(STE_NAME16 %in% 
                sa3_2016_states_vec) %>% dplyr::select(-c("SA4_CODE16", 
                "SA4_NAME16", "GCC_CODE16", "GCC_NAME16", "STE_CODE16", 
                "AREASQKM16")) %>% dplyr::mutate(LGA_CODE11 = NA_character_, 
                LGA_NAME11 = NA_character_, LGA_CODE16 = NA_character_, 
                LGA_NAME16 = NA_character_)) %>% append(list(no_stat_sf)) %>% 
            do.call(what = rbind)
        starter_sf <- starter_sf %>% tibble::rownames_to_column() %>% 
            dplyr::rename(XX1_NAME16 = rowname) %>% vicinity::make_valid_new_sf()
        saveRDS(starter_sf, file = path_to_seed_sf_1L_chr)
    }
    else {
        starter_sf <- "SKIP_IMPORT"
    }
    y_VicinityLookup <- x@a_VicinityLookup
    starter_sf_lup_r3 <- tibble::add_row(y_VicinityLookup@vicinity_templates_r3, 
        country_chr = y_VicinityLookup@vicinity_raw_r3 %>% dplyr::pull(country_chr), 
        area_type_chr = y_VicinityLookup@vicinity_raw_r3 %>% 
            dplyr::pull(area_type_chr), area_bndy_yr_chr = y_VicinityLookup@vicinity_raw_r3 %>% 
            dplyr::pull(area_bndy_yr_chr), starter_sf = starter_sf_name, 
        subdivision_chr = y_VicinityLookup@vicinity_raw_r3 %>% 
            dplyr::pull(uid_chr))
    y_VicinityLookup <- renewSlot(y_VicinityLookup, "vicinity_templates_r3", 
        starter_sf_lup_r3) %>% renew(what_1L_chr = "identifiers")
    y_VicinityLookup <- y_VicinityLookup %>% renew(template_ls = list(starter_sf) %>% 
        stats::setNames(y_VicinityLookup@vicinity_raw_r3 %>% 
            dplyr::pull(name_chr)), tbl_data_type_1L_chr = "Geometry", 
        package_1L_chr = x@pkg_1L_chr, what_1L_chr = "processed")
    return(y_VicinityLookup)
})
#' 
#' Author and save files
#' @name author-AusOrygen
#' @description author method applied to AusOrygen
#' @param x An object of class AusOrygen
#' @return Y (Look up tables for spatiotemporal data)
#' @rdname author-methods
#' @aliases author,AusOrygen-method
#' @export 
#' @importFrom ready4use assert_single_row_tb assert_matches_chr
#' @importFrom dplyr filter mutate group_by summarise_at vars first select pull
#' @importFrom vicinity get_set_diff_lng_lat_sf
#' @importFrom sf st_area
#' @importFrom units set_units
#' @importFrom tibble add_row
#' @importFrom stats setNames
#' @importFrom ready4 author
methods::setMethod("author", "AusOrygen", function (x) 
{
    y_VicinityLookup <- x@a_VicinityLookup
    vicinity_raw_r3 <- y_VicinityLookup@vicinity_raw_r3
    ready4use::assert_single_row_tb(vicinity_raw_r3)
    import_type_ls <- procure(vicinity_raw_r3)
    ready4use::assert_matches_chr(import_type_ls[[1]], "Aus_16_31_oyh")
    starter_sf_name_1L_chr <- paste0(vicinity_raw_r3$name_chr, 
        "_sf")
    path_to_seed_sf_1L_chr <- paste0(x@processed_fls_dir_1L_chr, 
        "/", starter_sf_name_1L_chr, ".RDS")
    if (x@overwrite_1L_lgl | !file.exists(path_to_seed_sf_1L_chr)) {
        aus_lga_16_sf <- eval(parse(text = x@merge_itms_chr[1]))
        oyh_part_whole_lgas_sf <- aus_lga_16_sf %>% dplyr::filter(LGA_NAME16 %in% 
            c("Brimbank (C)", "Hobsons Bay (C)", "Hume (C)", 
                "Maribyrnong (C)", "Melton (C)", "Moonee Valley (C)", 
                "Moreland (C)", "Wyndham (C)"))
        mel_lga_16_sf <- aus_lga_16_sf %>% dplyr::filter(LGA_NAME16 %in% 
            c("Melbourne (C)"))
        aus_sa1_16_sf <- eval(parse(text = x@merge_itms_chr[2]))
        mel_lga_sa1s_not_in_oyh_vec <- as.character(c(20604111801, 
            20604111802, 20604111808, 20604111814, 20604112501, 
            20604112502, 20604112503, 20604112504, 20604112505, 
            20604112506, 20604112507, 20604112508, 20604112509, 
            20604112510, 20604112511, 20604112512, 20604112513, 
            20604112514, 20604112602, 20604112603, 20604112604, 
            20604112605, 20604112606, 20604112609, 20604112610, 
            20604112611, 20604112612, 20604112613, 20604112614, 
            20604112615, 20604112616, 20604112617, 20604112618, 
            20604112619, 20604112620, 20604112622, 20604112623, 
            20604112624, 20604112625, 20604112626, 20604112627, 
            20604112628, 20604112629, 20604112630, 20604112631, 
            20604112632, 20604112633, 20604112634, 20604112635, 
            20605113102))
        mel_lga_not_oyh_sf <- aus_sa1_16_sf %>% dplyr::filter(SA1_MAIN16 %in% 
            mel_lga_sa1s_not_in_oyh_vec)
        mel_lga_in_oyh_sf <- vicinity::get_set_diff_lng_lat_sf(geometry_sf = mel_lga_16_sf, 
            cut_sf = mel_lga_not_oyh_sf, crs_nbr_dbl = x@crs_nbr_dbl)
        starter_sf <- rbind(oyh_part_whole_lgas_sf, mel_lga_in_oyh_sf) %>% 
            dplyr::mutate(SMH_NAME16 = "Orygen") %>% dplyr::group_by(SMH_NAME16) %>% 
            dplyr::summarise_at(dplyr::vars(c("STE_CODE16", "STE_NAME16")), 
                .funs = dplyr::first) %>% dplyr::mutate(AREA_KMSQ = sf::st_area(.) %>% 
            units::set_units(km^2)) %>% dplyr::select(SMH_NAME16, 
            STE_CODE16, STE_NAME16, AREA_KMSQ)
        saveRDS(starter_sf, file = path_to_seed_sf_1L_chr)
    }
    else {
        starter_sf <- "SKIP_IMPORT"
    }
    starter_sf_lup_r3 <- tibble::add_row(y_VicinityLookup@vicinity_templates_r3, 
        country_chr = y_VicinityLookup@vicinity_raw_r3 %>% dplyr::pull(country_chr), 
        area_type_chr = y_VicinityLookup@vicinity_raw_r3 %>% 
            dplyr::pull(area_type_chr), area_bndy_yr_chr = y_VicinityLookup@vicinity_raw_r3 %>% 
            dplyr::pull(area_bndy_yr_chr), starter_sf_nm_chr = starter_sf_name_1L_chr, 
        subdivision_chr = y_VicinityLookup@vicinity_raw_r3 %>% 
            dplyr::pull(uid_chr))
    y_VicinityLookup <- renew(y_VicinityLookup, "vicinity_templates_r3", 
        starter_sf_lup_r3) %>% renew(what_1L_chr = "identifiers")
    y_VicinityLookup <- y_VicinityLookup %>% renew(template_ls = list(starter_sf) %>% 
        stats::setNames(vicinity_raw_r3$name_chr), tbl_data_type_1L_chr = "Geometry", 
        package_1L_chr = x@pkg_1L_chr, what_1L_chr = "processed")
    return(y_VicinityLookup)
})
