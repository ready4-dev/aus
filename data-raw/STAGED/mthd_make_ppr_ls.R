
reshape_act_ppr_one_sex <- function(one_sex_tb,
                                    sex,
                                    age_bands,
                                    age_band_contents_ls){
  purrr::map2(age_bands,
              age_band_contents_ls,
              ~ one_sex_tb %>%
                dplyr::rowwise() %>%
                dplyr::mutate(!!rlang::sym(paste0(sex,.x)) := sum(!!!rlang::syms(.y))) %>%
                dplyr::ungroup() %>%
                dplyr::select(year_chr,SA3_NAME16,!!rlang::sym(paste0(sex,.x))) # SA3_NAME11 for previous version

  ) %>%
    stats::setNames(age_bands) %>%
    purrr::reduce(~ dplyr::inner_join(.x,.y))
}
make_act_pop_projs_ls <- function(path_1L_chr,
                                  var_value_vec){
  sheets_to_read <- c("Table 9", "Table 8")
  sex_vec <- c("Female","Male")
  years_vec <- c("2017(a)", # "2016(e)" for 2016_41 projections
                 "2021","2026","2031","2036","2041", # 2016_41 projections stop here
                 "2045","2049","2053","2058")
  age_band_contents_ls <- split(0:85,ceiling(seq_along(0:85)/5)) %>%
    purrr::map(~ as.character(.x) %>% stringr::str_replace("85","85+"))
  age_bands <- purrr::map_chr(age_band_contents_ls,
                              ~ paste0(".",.x[1],".",.x[5])) %>%
    stringr::str_replace("\\+.NA",".pl")
  # col_titles <- c(paste0("age.",0:85)
  import_obj_init_ls <- purrr::map2(sheets_to_read,
                                    sex_vec,
                                    ~  readxl::read_excel(path_1L_chr,
                                                          sheet = .x,
                                                          range = "B4:CK466", # "B3:CK246" for 2016_41 projections
                                                          col_names = TRUE) %>%
                                      dplyr::rename(year_chr = ...1,
                                                    SA3_NAME16 = ...2) %>% # "SA3_NAME11" for 2016_41 projections
                                      dplyr::filter(year_chr %in% years_vec)) %>%

    purrr::map2(paste0(sex_vec,"s"),
                ~ reshape_act_ppr_one_sex(one_sex_tb = .x,
                                          sex = .y,
                                          age_bands = age_bands,
                                          age_band_contents_ls = age_band_contents_ls)) %>%
    stats::setNames(sex_vec)
  import_obj_init_tb <- dplyr::inner_join(import_obj_init_ls %>% purrr::pluck(1),
                                          import_obj_init_ls %>% purrr::pluck(2))
  purrr::map(years_vec,
             ~ import_obj_init_tb %>%
               dplyr::filter(year_chr == .x)) %>%
    stats::setNames(paste0("y",years_vec) %>% stringr::str_replace("\\(a\\)", # "\\(e\\)" for 2016_41 projections
                                                                   ""))
}
get_nt_ppr_one_sex_tb <- function(sex_str,
                                  import_obj_init_one_sex,
                                  look_up_index_nbr,
                                  var.names,
                                  sa3_df,
                                  sheets_to_read_vec){
  one_sex_yrx <- purrr::map_dfc(sheets_to_read_vec,
                                ~ import_obj_init_one_sex %>% purrr::pluck(.x) %>% purrr::pluck(look_up_index_nbr))
  tibble::as.tibble(t(one_sex_yrx)) %>%
    dplyr::bind_cols(sa3_df)%>%
    dplyr::select(19, dplyr::everything()) %>%
    dplyr::rename_all(~var.names) %>% dplyr::rename_at(dplyr::vars(2:19), dplyr::funs(paste0(sex_str,".", .)))
}
transform_one_nt_ppr_tb <- function(look_up_index_nbr,
                                    import_obj_init_females,
                                    import_obj_init_males,
                                    var.names,
                                    sa3_df,
                                    sheets_to_read_vec){
  nt_all_yrx <- purrr::map2(c("Females", "Males"),
                            list(import_obj_init_females, import_obj_init_males),
                            ~ get_nt_ppr_one_sex_tb(sex_str = .x,
                                                    import_obj_init_one_sex = .y,
                                                    look_up_index_nbr = look_up_index_nbr,
                                                    var.names = var.names,
                                                    sa3_df = sa3_df,
                                                    sheets_to_read_vec = sheets_to_read_vec))
  nt_all_yrx <- dplyr::full_join(nt_all_yrx[[1]], nt_all_yrx[[2]], by="SA3_NAME16")
}

make_nt_pop_projs_ls <- function(path_1L_chr,
                                 var_value_vec){
  sheets_to_read_vec <- c("Darwin City Projections",
                          "Darwin Suburbs Projections",
                          "Litchfield Projections",
                          "Palmerston Projections",
                          "Alice Springs Projections",
                          "Barkly Projections",
                          "Daly-Tiwi-WArnhem Projections",
                          "East Arnhem Projections",
                          "Katherine Projections")
  import_obj_init_ls <- purrr::map(c("A173:F191", "A149:F167"),
                                   ~ ready4fun::import_xl_sheets_as_ls(range_str = .x,
                                                                         sheets_to_read_vec = sheets_to_read_vec,
                                                                         path_1L_chr =  path_1L_chr))
  var.names <- c("SA3_NAME16",	"0.4",  	"5.9",	"10.14",	"15.19",	"20.24",	"25.29",	"30.34",
                 "35.39",	"40.44",	"45.49",	"50.54",	"55.59",	"60.64",	"65.69",	"70.74",	"75.79",	"80.84",	"85.pl")
  sa3_df <- tibble::tibble(SA3_NAME16 = c("Darwin City",
                                          "Darwin Suburbs",
                                          "Litchfield",
                                          "Palmerston",
                                          "Alice Springs",
                                          "Barkly",
                                          "Daly-Tiwi-West Arnhem",
                                          "East Arnhem",
                                          "Katherine"))
  purrr::map(2:6,
             ~ transform_one_nt_ppr_tb(look_up_index_nbr =  .x,
                                       import_obj_init_females = import_obj_init_ls[[1]],
                                       import_obj_init_males = import_obj_init_ls[[2]],
                                       var.names = var.names,
                                       sa3_df = sa3_df,
                                       sheets_to_read_vec = sheets_to_read_vec)) %>%
    stats::setNames(paste0("y",c("2016","2021","2026","2031","2036")))
}
make_qld_pop_projs_ls <- function(path_1L_chr,
                                  var_value_vec){
  var.names <- c("LGA_NAME16",	"0.4",  	"5.9",	"10.14",	"15.19",	"20.24",	"25.29",	"30.34",
                 "35.39",	"40.44",	"45.49",	"50.54",	"55.59",	"60.64",	"65.69",	"70.74",	"75.79",	"80.84",	"85.pl")

  #QLD 2016
  qld_males_2016 <- readxl::read_excel(path_1L_chr,
                                       sheet = "proj-pop-agegp-males-lga", range = "A7:S85", col_names = FALSE)

  qld_females_2016 <- readxl::read_excel(path_1L_chr,
                                         sheet = "proj-pop-agegp-females-lga", range = "A7:S85", col_names = FALSE)
  qld_males_2016 <- qld_males_2016 %>%
    dplyr::rename_at(names(qld_males_2016), ~ var.names) %>%
    dplyr::rename_at(dplyr::vars(2:19), dplyr::funs(paste0("Males.", .)))
  qld_females_2016 <- qld_females_2016 %>%
    dplyr::rename_at(names(qld_females_2016), ~ var.names) %>%
    dplyr::rename_at(dplyr::vars(2:19), dplyr::funs(paste0("Females.", .)))
  qld_all_2016 <- dplyr::full_join(qld_females_2016, qld_males_2016)
  qld_LGA_vec <- qld_all_2016$LGA_NAME16
  ## qld 2021
  qld_males_2021 <- readxl::read_excel(path_1L_chr,
                                       sheet = "proj-pop-agegp-males-lga", range = c("V7:AM85"), col_names = FALSE)
  qld_females_2021 <- readxl::read_excel(path_1L_chr,
                                         sheet = "proj-pop-agegp-females-lga", range = c("V7:AM85"), col_names = FALSE)

  qld_males_2021 <- cbind(qld_LGA_vec, qld_males_2021)
  qld_males_2021 <- qld_males_2021 %>%
    dplyr::rename_at(names(qld_males_2021), ~ var.names) %>%
    dplyr::rename_at(dplyr::vars(2:19), dplyr::funs(paste0("Males.", .)))
  qld_females_2021 <- cbind(qld_LGA_vec, qld_females_2021)
  qld_females_2021<- qld_females_2021 %>%
    dplyr::rename_at(names(qld_females_2021), ~ var.names) %>%
    dplyr::rename_at(dplyr::vars(2:19), dplyr::funs(paste0("Females.", .)))
  qld_all_2021<- dplyr::full_join(qld_females_2021, qld_males_2021)
  ##qld 2026
  qld_males_2026 <- readxl::read_excel(path_1L_chr,
                                       sheet = "proj-pop-agegp-males-lga", range = c("AP7:BG85"), col_names = FALSE)
  qld_females_2026 <- readxl::read_excel(path_1L_chr,
                                         sheet = "proj-pop-agegp-females-lga", range = c("AP7:BG85"), col_names = FALSE)
  qld_males_2026 <- cbind(qld_LGA_vec, qld_males_2026)
  qld_males_2026 <- qld_males_2026 %>%
    dplyr::rename_at(names(qld_males_2026), ~ var.names) %>%
    dplyr::rename_at(dplyr::vars(2:19), dplyr::funs(paste0("Males.", .)))
  qld_females_2026 <- cbind(qld_LGA_vec, qld_females_2026)
  qld_females_2026<- qld_females_2026 %>%
    dplyr::rename_at(names(qld_females_2026), ~ var.names) %>%
    dplyr::rename_at(dplyr::vars(2:19), dplyr::funs(paste0("Females.", .)))
  qld_all_2026<- dplyr::full_join(qld_females_2026, qld_males_2026)
  ##qld 2031
  qld_males_2031 <- readxl::read_excel(path_1L_chr,
                                       sheet = "proj-pop-agegp-males-lga", range = c("BJ7:CA85"), col_names = FALSE)
  qld_females_2031 <- readxl::read_excel(path_1L_chr,
                                         sheet = "proj-pop-agegp-females-lga", range = c("BJ7:CA85"), col_names = FALSE)
  qld_males_2031 <- cbind(qld_LGA_vec, qld_males_2031)
  qld_males_2031 <- qld_males_2031 %>%
    dplyr::rename_at(names(qld_males_2031), ~ var.names) %>%
    dplyr::rename_at(dplyr::vars(2:19), dplyr::funs(paste0("Males.", .)))
  qld_females_2031 <- cbind(qld_LGA_vec, qld_females_2031)
  qld_females_2031<- qld_females_2031 %>%
    dplyr::rename_at(names(qld_females_2031), ~ var.names) %>%
    dplyr::rename_at(dplyr::vars(2:19), dplyr::funs(paste0("Females.", .)))
  qld_all_2031<- dplyr::full_join(qld_females_2031, qld_males_2031)
  list(y2016 = qld_all_2016 %>% tibble::as.tibble(),
       y2021 = qld_all_2021 %>% tibble::as.tibble(),
       y2026 = qld_all_2026 %>% tibble::as.tibble(),
       y2031 = qld_all_2031 %>% tibble::as.tibble()) %>%
    purrr::map( ~ .x %>%
                  dplyr::filter(LGA_NAME16 != "Queensland (c)") %>%
                  dplyr::mutate(LGA_NAME16 = as.character(LGA_NAME16)))
}
make_sa_pop_projs_ls <- function(path_1L_chr,
                                 var_value_vec){
  var.names <- c("LGA_NAME11",	"Sex", "0.4",  	"5.9",	"10.14",	"15.19",	"20.24",	"25.29",	"30.34",
                 "35.39",	"40.44",	"45.49",	"50.54",	"55.59",	"60.64",	"65.69",	"70.74",	"75.79",	"80.84",	"85.pl")
  #SA 2016
  sa_males_2016 <- readxl::read_excel(path_1L_chr,
                                      sheet = "2016", range = c("C101:V185"), col_names = FALSE)
  sa_females_2016 <- readxl::read_excel(path_1L_chr,
                                        sheet = "2016", range = "C191:V275", col_names = FALSE)
  sa_males_2016 <- sa_males_2016 %>%
    dplyr::rename_at(names(sa_males_2016), ~ var.names) %>%
    dplyr::rename_at(dplyr::vars(3:20), dplyr::funs(paste0("Males.", .))) %>%
    dplyr::select(-Sex)
  sa_females_2016 <- sa_females_2016 %>%
    dplyr::rename_at(names(sa_females_2016), ~ var.names) %>%
    dplyr::rename_at(dplyr::vars(3:20), dplyr::funs(paste0("Females.", .))) %>%
    dplyr::select(-Sex)
  sa_all_2016 <- dplyr::full_join(sa_females_2016, sa_males_2016)
  ## SA 2021
  sa_males_2021 <- readxl::read_excel(path_1L_chr,
                                      sheet = "2021", range = c("C101:V185"), col_names = FALSE)
  sa_females_2021 <- readxl::read_excel(path_1L_chr,
                                        sheet = "2021", range = c("C101:V185"), col_names = FALSE)
  sa_males_2021 <- sa_males_2021 %>%
    dplyr::rename_at(names(sa_males_2021), ~ var.names) %>%
    dplyr::rename_at(dplyr::vars(3:20), dplyr::funs(paste0("Males.", .))) %>%
    dplyr::select(-Sex)
  sa_females_2021 <- sa_females_2021 %>%
    dplyr::rename_at(names(sa_females_2021), ~ var.names) %>%
    dplyr::rename_at(dplyr::vars(3:20), dplyr::funs(paste0("Females.", .))) %>%
    dplyr::select(-Sex)
  sa_all_2021<- dplyr::full_join(sa_females_2021, sa_males_2021)
  ##SA 2026
  sa_males_2026 <- readxl::read_excel(path_1L_chr,
                                      sheet = "2026", range = c("C101:V185"), col_names = FALSE)
  sa_females_2026 <- readxl::read_excel(path_1L_chr,
                                        sheet = "2026", range = c("C101:V185"), col_names = FALSE)
  sa_males_2026 <- sa_males_2026 %>%
    dplyr::rename_at(names(sa_males_2026), ~ var.names) %>%
    dplyr::rename_at(dplyr::vars(3:20), dplyr::funs(paste0("Males.", .))) %>%
    dplyr::select(-Sex)
  sa_females_2026 <- sa_females_2026 %>%
    dplyr::rename_at(names(sa_females_2026), ~ var.names) %>%
    dplyr::rename_at(dplyr::vars(3:20), dplyr::funs(paste0("Females.", .))) %>%
    dplyr::select(-Sex)
  sa_all_2026<- dplyr::full_join(sa_females_2026, sa_males_2026)
  ##SA 2031
  sa_males_2031 <- readxl::read_excel(path_1L_chr,
                                      sheet = "2031", range = c("C101:V185"), col_names = FALSE)
  sa_females_2031 <- readxl::read_excel(path_1L_chr,
                                        sheet = "2031", range = c("C101:V185"), col_names = FALSE)
  sa_males_2031 <- sa_males_2031 %>%
    dplyr::rename_at(names(sa_males_2031), ~ var.names) %>%
    dplyr::rename_at(dplyr::vars(3:20), dplyr::funs(paste0("Males.", .))) %>%
    dplyr::select(-Sex)
  sa_females_2031 <- sa_females_2031 %>%
    dplyr::rename_at(names(sa_females_2031), ~ var.names) %>%
    dplyr::rename_at(dplyr::vars(3:20), dplyr::funs(paste0("Females.", .))) %>%
    dplyr::select(-Sex)
  sa_all_2031 <- dplyr::full_join(sa_females_2031, sa_males_2031)
  list(y2016 = sa_all_2016,
       y2021 = sa_all_2021,
       y2026 = sa_all_2026,
       y2031 = sa_all_2031) %>%
    purrr::map(~ {
      sa_ppr_tb <- .x %>% dplyr::filter(!startsWith(LGA_NAME11,"TOTAL"))
      sa_ppr_tb %>%
        dplyr::summarise_if(is.numeric, ~ sum(.[stringr::str_detect(LGA_NAME11, 'Unincorp')])) %>%
        dplyr::mutate(LGA_NAME11 = 'Unincorporated SA') %>%
        dplyr::bind_rows(sa_ppr_tb %>%
                           dplyr::filter(!startsWith(LGA_NAME11,"Unincorp")),
                         .)
    }
    )
}
make_one_tas_age_band_tb <- function(age_range_str,
                                     path_1L_chr,
                                     years_vec){
  by_sex_ls <- purrr::map(c("female","male"),
                          ~ readxl::read_excel(path_1L_chr,
                                               sheet = paste0(age_range_str,.x), range="A6:AA35", col_names = TRUE) %>%
                            dplyr::select(...1, years_vec) %>% # "X__1"
                            dplyr::rename("LGA_NAME11" = ...1) %>% # "X__1"
                            dplyr::rename_at(dplyr::vars(2:6),
                                             dplyr::funs(paste0(age_range_str %>%
                                                                  stringr::str_sub(start=2) %>%
                                                                  stringr::str_replace("_","."), .))) %>%
                            dplyr::mutate_at(dplyr::vars(2:6),
                                             ~ stringr::str_replace(.," ","")) %>%
                            dplyr::mutate_at(dplyr::vars(2:6),
                                             ~ as.numeric(.)))
  dplyr::full_join(by_sex_ls[[1]], by_sex_ls[[2]], by="LGA_NAME11")
}
make_one_tas_by_year_age_band_ls <- function(age_band_all_years_tb,
                                             years_vec){
  purrr::map(years_vec,
             ~ age_band_all_years_tb %>%
               dplyr::select(LGA_NAME11, dplyr::contains(.x))) %>%
    stats::setNames(years_vec)
}
make_one_year_tas_tb <- function(year_chr,
                                 by_year_and_age_band_tbs_ls,
                                 var_names){
  purrr::map(by_year_and_age_band_tbs_ls,
             ~ .x %>% purrr::pluck(year_chr)) %>%
    Reduce(function(x,y) merge(x = x, y = y, by = "LGA_NAME11"),
           .) %>%
    dplyr::rename_at(dplyr::vars(2:37), ~ var_names) %>%
    tibble::as.tibble()
}
make_tas_pop_projs_ls <- function(path_1L_chr,
                                  var_value_vec){
  years_vec <- c("2016", "2021", "2026", "2031", "2036")
  age_range_str_vec <- c(paste0("_",seq(0,80,5),"_",seq(4,84,5),"_"),"_85plus_")#c("_0_4_", "_5_9_","_10_14_", "_15_19_","_20_24_", "_25_29_","_30plus_")
  by_age_band_all_tbs_ls <- purrr::map(age_range_str_vec,
                                       ~ make_one_tas_age_band_tb(age_range_str = .x,
                                                                  path_1L_chr = path_1L_chr,
                                                                  years_vec = years_vec)) %>%
    stats::setNames(age_range_str_vec)
  by_year_and_age_band_tbs_ls <- purrr::map(by_age_band_all_tbs_ls,
                                            ~ .x %>% make_one_tas_by_year_age_band_ls(years_vec = years_vec))
  var_names <- purrr::map(c("Females","Males"),
                              ~ paste0(.x,
                                       c(paste0(".",seq(0,80,5),".",seq(4,84,5)),".85.pl"))) %>%
    purrr::flatten_chr()
    # c(	"Females.0.4", "Females.5.9",	"Females.10.14",	"Females.15.19", "Females.20.24",	"Females.25.29", "Females.30.pl",
    #               "Males.0.4", "Males.5.9",	"Males.10.14",	"Males.15.19", "Males.20.24",	"Males.25.29",	"Males.30.pl")
  purrr::map(years_vec,
             ~ make_one_year_tas_tb(.x,
                                    by_year_and_age_band_tbs_ls = by_year_and_age_band_tbs_ls,
                                    var_names = var_names)) %>%
    stats::setNames(paste0("y",years_vec))
}
make_wa_pop_projs_ls <- function(path_1L_chr,
                                 var_value_vec){
  var.names <- c("LGA_NAME16",	"0.4",  	"5.9",	"10.14",	"15.19",	"20.24",	"25.29",	"30.34",
                 "35.39",	"40.44",	"45.49",	"50.54",	"55.59",	"60.64",	"65.69",	"70.74",	"75.79",	"80.84",	"85.pl")
  wa_allsex_allyears <- readxl::read_excel(path_1L_chr,
                                           sheet = "LGA age sex dataset", col_names = TRUE)
  wa_allsex_allyears <- wa_allsex_allyears %>% dplyr::filter(Band == "Band C") %>%
    dplyr::select(Region, dplyr::everything(), -Band) %>%
    tidyr::spread(Age, Persons)
  #WA 2016
  wa_males_2016 <- wa_allsex_allyears %>%
    dplyr::filter(Year==2016, Sex=="male") %>%
    dplyr::select(-Year, -Sex) %>%
    dplyr::rename_all(~ var.names) %>%
    dplyr::rename_at(dplyr::vars(2:19), dplyr::funs(paste0("Males.", .)))
  wa_females_2016 <- wa_allsex_allyears %>%
    dplyr::filter(Year==2016, Sex=="male") %>%
    dplyr::select(-Year, -Sex) %>%
    dplyr::rename_all(~ var.names) %>%
    dplyr::rename_at(dplyr::vars(2:19), dplyr::funs(paste0("Females.", .)))
  wa_all_2016 <- dplyr::full_join(wa_females_2016, wa_males_2016)
  ## wa 2021
  wa_males_2021 <- wa_allsex_allyears %>%
    dplyr::filter(Year==2021, Sex=="male") %>%
    dplyr::select(-Year, -Sex) %>%
    dplyr::rename_all(~ var.names) %>%
    dplyr::rename_at(dplyr::vars(2:19), dplyr::funs(paste0("Males.", .)))
  wa_females_2021 <- wa_allsex_allyears %>%
    dplyr::filter(Year==2021, Sex=="male") %>%
    dplyr::select(-Year, -Sex) %>%
    dplyr::rename_all(~ var.names) %>%
    dplyr::rename_at(dplyr::vars(2:19), dplyr::funs(paste0("Females.", .)))
  wa_all_2021 <- dplyr::full_join(wa_females_2021, wa_males_2021)
  ##wa 2026
  wa_males_2026 <- wa_allsex_allyears %>%
    dplyr::filter(Year==2026, Sex=="male") %>%
    dplyr::select(-Year, -Sex) %>%
    dplyr::rename_all(~ var.names) %>%
    dplyr::rename_at(dplyr::vars(2:19), dplyr::funs(paste0("Males.", .)))
  wa_females_2026 <- wa_allsex_allyears %>%
    dplyr::filter(Year==2026, Sex=="male") %>%
    dplyr::select(-Year, -Sex) %>%
    dplyr::rename_all(~ var.names) %>%
    dplyr::rename_at(dplyr::vars(2:19), dplyr::funs(paste0("Females.", .)))
  wa_all_2026 <- dplyr::full_join(wa_females_2026, wa_males_2026)
  ##wa 2031
  wa_males_2031 <- wa_allsex_allyears %>%
    dplyr::filter(Year==2031, Sex=="male") %>%
    dplyr::select(-Year, -Sex) %>%
    dplyr::rename_all(~ var.names) %>%
    dplyr::rename_at(dplyr::vars(2:19), dplyr::funs(paste0("Males.", .)))
  wa_females_2031 <- wa_allsex_allyears %>%
    dplyr::filter(Year==2031, Sex=="male") %>%
    dplyr::select(-Year, -Sex) %>%
    dplyr::rename_all(~ var.names) %>%
    dplyr::rename_at(dplyr::vars(2:19), dplyr::funs(paste0("Females.", .)))
  wa_all_2031 <- dplyr::full_join(wa_females_2031, wa_males_2031)
  list(y2016 = wa_all_2016,
       y2021 = wa_all_2021,
       y2026 = wa_all_2026,
       y2031 = wa_all_2031)
}
make_nsw_pop_projs_ls <- function(path_1L_chr,
                                  var_value_vec){
  sheets_to_read <- readxl::excel_sheets(path_1L_chr)[readxl::excel_sheets(path_1L_chr) != "Notes"]
  import_obj_init <- purrr::map(sheets_to_read,
                                ~  readxl::read_excel(path_1L_chr,
                                                      sheet = .x,
                                                      range = ifelse(.x %in% c("CUMBERLAND",
                                                                               "HORNSBY",
                                                                               "PARRAMATTA",
                                                                               "THE HILLS"),
                                                                     "A20:G38",
                                                                     "A22:G40"),
                                                      col_names = TRUE)) %>%
    stats::setNames(sheets_to_read)
  uses_five_year_bands_vec <- purrr::map_lgl(import_obj_init,
                                             ~ .x[1,1] == "0-4")
  uses_large_bands_vec <- names(uses_five_year_bands_vec)[!uses_five_year_bands_vec]
  uses_five_year_bands_vec <- names(uses_five_year_bands_vec)[uses_five_year_bands_vec]
  import_obj_five_yr_bands <- import_obj_init[uses_five_year_bands_vec]
  import_obj_large_bands <- import_obj_init[uses_large_bands_vec]
  import_obj_large_bands <- purrr::map(import_obj_large_bands,
                                       ~ .x %>% dplyr::slice(1:4))
  proj_years_vec <- names(import_obj_init[[1]])[!names(import_obj_init[[1]]) %in% c("AGE GROUPS:", "2011")]
  import_obj_by_bands_ls <- purrr::map2(list(import_obj_five_yr_bands, import_obj_large_bands),
                                        list(uses_five_year_bands_vec, uses_large_bands_vec),
                                        ~ make_import_obj_by_bands_ls(import_obj_by_x_yr_bands = .x,
                                                                      included_areas_names_vec = .y,
                                                                      var_value_vec = var_value_vec,
                                                                      proj_years_vec = proj_years_vec)) %>%
    stats::setNames(c("five_yr_bands", "large_bands"))
  purrr::map2(import_obj_by_bands_ls$five_yr_bands,
              import_obj_by_bands_ls$large_bands,
              ~ dplyr::bind_rows(.x,.y))
}
make_import_obj_by_bands_ls <- function(import_obj_by_x_yr_bands,
                                        included_areas_names_vec,
                                        var_value_vec,
                                        proj_years_vec){
  all_year_summary_by_bands_tb_ls <- purrr::map(import_obj_by_x_yr_bands,
                                                ~ vicinity::transform_shape_of_popl_predns_ds(.x,
                                                                                              prefix_1L_chr = "Persons.")) %>%
    purrr::map2_dfr(.,
                    included_areas_names_vec,
                    ~ .x %>%
                      dplyr::mutate(!!rlang::sym(paste0(var_value_vec[1],
                                                        "_NAME",
                                                        var_value_vec[3] %>% stringr::str_sub(start = 3))) := .y) %>%
                      dplyr::select(!!rlang::sym(paste0(var_value_vec[1],
                                                        "_NAME",
                                                        var_value_vec[3] %>% stringr::str_sub(start = 3))),
                                    dplyr::everything()))

  # proj_years_vec <- names(import_obj[[1]])[!names(import_obj[[1]]) %in% c("AGE GROUPS:", "2011")]
  purrr::map(proj_years_vec,
             ~ all_year_summary_by_bands_tb_ls %>%
               dplyr::filter(year == .x)) %>%
    stats::setNames(paste0("y",proj_years_vec))
}

