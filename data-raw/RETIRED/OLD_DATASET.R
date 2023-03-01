devtools::load_all()
#source("data-raw/MAKE_CLASS_TB.R")
##
## Note: Data import CSVs need to be placed in Context folder before undertaking the following steps.
## 1. IMPORT GEOMETRY DATA AND CREATE ASSOCIATED LOOKUPS
crs_nbr_dbl <- c(4283,3577)
package_1L_chr <- ""
data_dir <- "../../../../Data" # R.utils::getRelativePath("PATH_TO_DATA_DIRECTORY_ON_YOUR_MACHINE")
raw_fls_dir_1L_chr <- paste0(data_dir,"/Raw_Format")
r_data_dir <- paste0(data_dir,"/R_Format")
raw_metadata_dir <- paste0(raw_fls_dir_1L_chr,"/Metadata")
dv_dir <- paste0(data_dir,"/Dataverse")
dv_model_dir <- paste0(dv_dir,"/Spring To Life SD Australia/Data")
lup_dir <- paste0(raw_metadata_dir,"/LookUps")#"data-raw"
if(!dir.exists(raw_fls_dir_1L_chr))
  dir.create(raw_fls_dir_1L_chr)
if(!dir.exists(r_data_dir))
  dir.create(r_data_dir)
if(!dir.exists(raw_metadata_dir))
  dir.create(raw_metadata_dir)
if(!dir.exists(lup_dir))
  dir.create(lup_dir)
aus_geo_imp_r3 <- dplyr::bind_rows(ready4use::get_r3_from_dv_csv(file_name_chr = "geo_import",
                                                 data_repo_db_ui_chr = "https://doi.org/10.7910/DVN/M1AGN1",#"https://doi.org/10.7910/DVN/MFN3KM",
                                                 r3_fn = Aus_16_31_sp_import_lup))
aus_sp_geo_lup_r4 <- aus_geo_imp_r3 %>%
  metamorphose(init_lookup_r4 = NULL,#vicinity::make_data_packs
                               package_1L_chr = package_1L_chr,
                               raw_fls_dir_1L_chr = raw_fls_dir_1L_chr,
                               processed_fls_dir_1L_chr = r_data_dir,
                               lup_r4_name = "aus_sp_geo_lup_r4",
                               crs_nbr_dbl = crs_nbr_dbl,
                               overwrite_1L_lgl = F)
## 2. IMPORT ATTRIBUTE DATA AND CREATE ASSOCIATED LOOKUPS
aus_sp_att_imp_r3 <- ready4use::get_r3_from_dv_csv(file_name_chr = "dmg_str_import",
                                                   data_repo_db_ui_chr =  "https://doi.org/10.7910/DVN/M1AGN1",
                                                   r3_fn = s2lsd::Aus_16_31_sp_import_lup) %>%
  ready4use::get_r3_from_dv_csv(file_name_chr = "sp_att_import",
                              data_repo_db_ui_chr =  "https://doi.org/10.7910/DVN/M1AGN1",#"https://doi.org/10.7910/DVN/MFN3KM",
                              r3_fn = Aus_16_31_sp_import_lup)
aus_sp_att_lup_r4 <- aus_sp_att_imp_r3 %>%
  #dplyr::filter(region=="VIC" & main_feature_chr =="ERP_ASX") %>%
  metamorphose(init_lookup_r4 = NULL,#vicinity::make_data_packs
                               package_1L_chr = package_1L_chr,
                               raw_fls_dir_1L_chr = raw_fls_dir_1L_chr,
                               processed_fls_dir_1L_chr = r_data_dir,
                               lup_r4_name = "aus_sp_att_lup_r4",
                               overwrite_1L_lgl = T)
## 3. MERGE THE SHAPE AND BOUNDARY LOOKUPS
devtools::load_all() # Need to repeat this as package detached in previous step. Need to fix that to prevent detaching previously attached package.
aus_sp_lups_r4 <- ready4::rowbind_all_tbs_in_r4_obj(tbs_r4 = aus_sp_geo_lup_r4,#ready4fun::add_all_tbs_in_r4
                                                    second_tbs_r4 = aus_sp_att_lup_r4,
                                                    r4_name_1L_chr = "VicinityLookup") %>% as("Aus_16_31_lookup")
## 4. ADD ABBREVIATIONS AND RESOLUTION LUP DATA
aus_sp_lups_r4 <- vicinity::`sp_abbreviations_lup<-`(aus_sp_lups_r4,
                                                        ready4use::get_r3_from_dv_csv(file_name_chr = "aus_abbrev",
                                                                                      data_repo_db_ui_chr =  "https://doi.org/10.7910/DVN/M1AGN1",
                                                                                      r3_fn = vicinity::vicinity_abbreviations)) %>%
  renew(path_1L_chr = r_data_dir,
        what_1L_chr = "resolutions")#vicinity::add_resolutions_lup

## 5. ADD LOOKUP TABLES OBJECT TO PROFILED AREA OBJECT
library(vicinity)
aus_x_VicinityProfile <- vicinity::profiled_area() %>%
  renewSlot("a_VicinityLookup", aus_sp_lups_r4) %>% #  vicinity::`lookup_tb<-`(vicinity::VicinityProfile(), aus_sp_lups_r4) %>%
  renewSlot("crs_dbl", crs_nbr_dbl) %>% # vicinity::`crs_nbr<-`(crs_nbr_dbl) %>%
  renewSlot("country_chr", "Australia") %>% # vicinity::`country<-`("Australia") %>%
  renewSlot("data_ymds_dtm", lubridate::ymd_hms("2016-07-01 12:00:00", tz = "Australia/Melbourne")) %>% # vicinity::`data_ymds<-`(lubridate::ymd_hms("2016-07-01 12:00:00", tz = "Australia/Melbourne")) %>%
  renewSlot("data_year_1L_chr", "2016") %>% # vicinity::`data_year<-`("2016") %>%
  renewSlot("geomc_dist_limit_km_dbl", c(10,50)) %>% # vicinity::`geom_dist_limit_km<-`(c(10,50)) %>%
  renewSlot("drive_time_limit_mins_dbl", c(15,60)) %>% # vicinity::`drive_time_limit_mins<-`(c(15,60)) %>%
  renewSlot("region_type_chr", "STE") %>% # vicinity::`region_type<-`("STE") %>%
  renewSlot("region_bndy_yr_dbl", 2016) %>% # vicinity::`region_bound_year<-`(2016) %>%
  renewSlot("features_chr", NA_character_) %>% # vicinity::`features<-`(NA_character_) %>%
  renewSlot("temporal_min_dtm", lubridate::ymd_hms("2016-07-01 12:00:00", tz = "Australia/Melbourne")) %>% # vicinity::`temporal_min<-`(lubridate::ymd_hms("2016-07-01 12:00:00", tz = "Australia/Melbourne")) %>%
  renewSlot("temporal_max_dtmp", lubridate::ymd_hms("2016-07-01 12:00:00", tz = "Australia/Melbourne") + lubridate::years(15)) # vicinity::`temporal_max<-`(lubridate::ymd_hms("2016-07-01 12:00:00", tz = "Australia/Melbourne") + lubridate::years(15))
## 11. SAVE TO LOOKUP TABLES OBJECT TO PACKAGE DATA DIRECTORY
usethis::use_data(aus_x_VicinityProfile,overwrite = T)
## 12. SAVE CSV VERSIONS OF EACH LOOKUP TO RAW DATA LOOKUPS DIRECTORY
ready4fun::export_as_csv_all_tbs(r4_1 = aus_sp_lups_r4,
                                   r4_name = "VicinityLookup",
                                   lup_dir = lup_dir,
                                   prefix = "AusSpR4c")

