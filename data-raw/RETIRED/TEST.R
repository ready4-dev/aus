
<<<<<<< HEAD
=======
                                   }) %>% any()
    if(update_ds_lgl & F) # TEMPORARILY TURNED OFF AS UPDATE NOT WORKING
      dataverse::update_dataset(dataset = ds_ls,
                                body = ds_meta_ls,
                                key = key_chr,
                                server = server_chr
                                )
    dv_ls <- dataverse::dataverse_contents(dv)
  }
  dv_ls[[1]]$persistentUrl
}
add_dv_meta_to_imp_lup <- function(x,
                                   db_ui_chr,
                                   file_type_chr,
                                   save_type_chr){
  ready4use::assert_single_row_tb(x)
  x %>%
    dplyr::mutate(data_repo_db_ui_chr = db_ui_chr,
                  data_repo_file_ext_chr = file_type_chr,
                  data_repo_save_type_chr = save_type_chr)
}
make_files_tb <- function(dir_chr_vec,
                          recode_ls){
  files_tb <- purrr::map_dfr(dir_chr_vec,
                             ~{
                               files_chr_vec <- list.files(.x)
                               if(!identical(files_chr_vec,character(0))){
                                 tb <- tibble::tibble(dir_chr = rep(.x,length(files_chr_vec)),
                                                      file_chr = files_chr_vec %>%
                                                        purrr::map_chr(~stringr::str_sub(.x,
                                                                                         end = as.vector(stringi::stri_locate_last_regex(.x, "\\.")[,1])-1)),
                                                      file_type_chr = files_chr_vec %>%
                                                        purrr::map_chr(~stringr::str_sub(.x,
                                                                                         start = as.vector(stringi::stri_locate_last_regex(.x, "\\.")[,1]))))

                               tb
                               }
                             })
  description_chr <- purrr::map_chr(files_tb$file_chr,
                                    ~ {
                                      arg_ls <- append(list(EXPR=.x),recode_ls)
                                      rlang::exec(.fn = switch, !!!arg_ls)
                                    })
  files_tb <- files_tb %>%
    dplyr::mutate(description_chr = description_chr,
                       ds_file_ext = purrr::map_chr(file_type_chr,
                                                    ~ ifelse(.x %in% c(".csv", ".xls",".xlsx"),
                                                             ".tab",
                                                             ".zip")))
  assertthat::are_equal(nrow(files_tb),
                        paste0(files_tb$file_chr,
                               files_tb$file_type_chr) %>%
                          unique() %>%
                          length())
  files_tb
}
copy_files_to_dv_dir <- function(files_tb,
                                 dv_model_dir_chr){
  purrr::pwalk(files_tb,
               ~ file.copy(paste0(..1,"/",..2,..3),
                           dv_model_dir_chr))
}
add_files_to_dv <- function(files_tb, ## NEED TO ADD IGNORE/DELETE & REPLACE LOGIC IF FILES ALREADY IN ONLINE REPO
                            dv_model_dir_chr,
                            ds_url_chr,
                            key_chr,
                            server_chr){
  purrr::pmap_int(files_tb,
                  ~ dataverse::add_dataset_file(file = paste0(dv_model_dir_chr,"/",..2,..3),
                                                dataset = ds_url_chr,
                                                description = ..4,
                                                key = key_chr,
                                                server = server_chr))
}
devtools::load_all()
crs_nbr_dbl <- c(4283,3577)
package_1L_chr <- ""
data_dir_chr <- "../../../../Data"
raw_fls_dir_1L_chr <- paste0(data_dir_chr,"/Raw_Format")
processed_fls_dir_1L_chr <- paste0(data_dir_chr,"/R_Format")
#raw_metadata_dir_chr <- paste0(raw_fls_dir_1L_chr,"/Metadata")
dv_dir_chr <- paste0(data_dir_chr,"/Dataverse")
key_chr <- Sys.getenv("DATAVERSE_KEY")
server_chr <- Sys.getenv("DATAVERSE_SERVER")
#dv <- dataverse::create_dataverse("rfwn")
ds_meta_ls <- list(title = "Replication Data for s2lsd",
                  creator = "Hamilton, Matthew",
                  description = "Geometry, spatial attribute and metadata to replicate implementation of the Spring To Life model prototype within the Australian context. Spring To Life SD is a systems dynamics spatial simulation of area demographic characteristics. The current version of the model is quite rudimentary and is designed to be used in conjunction with other models developed with the readyforwhatsnext open science framework for mental health simulations.",
                  subject = "Mental health, health economics, spatial simulation, systems dynamics")
ds_url_chr <- add_ds_to_dv_repo(dv_chr = "readyforwhatsnext",
                    ds_meta_ls = ds_meta_ls)
ds_ls <- dataverse::get_dataset(ds_url_chr)
dv_model_dir_chr <- paste0(dv_dir_chr,"/",ds_meta_ls$title)
if(!dir.exists(dv_model_dir_chr)){
  dir.create(dv_model_dir_chr)
}
files_tb <- make_files_tb(dir_chr_vec = c("data-raw/Metadata","data-raw/Attributes","data-raw/Geometries"),
                          recode_ls = list("aus_abbrev" = "Abbreviations for Australian data objects",
                                            "dmg_str_import" = "Metadata on global environment data imports",
                                            "env_str_param_tb" = "Global environment structural parameter values",
                                            "geo_import" = "Metadata for geometry data imports",
                                            "sp_att_import" = "Metadata for spatial attribute data imports"))
copy_files_to_dv_dir(files_tb,
                     dv_model_dir_chr = dv_model_dir_chr)
f_int_vec <- add_files_to_dv(files_tb,
                             dv_model_dir_chr = dv_model_dir_chr,
                             ds_url_chr = ds_url_chr,
                             key_chr = key_chr,
                             server_chr = server_chr)

# purrr::pwalk(files_tb,
#      ~ file.copy(paste0(..1,"/",..2,..3),
#                 dv_model_dir_chr))
#dir_chr <- paste0(dv_dir_chr,"/Spring To Life SD Australia/Data")
# files_tb <- tibble::tibble(dir_chr = paste0(dir_chr,"/",c("Geometries","Attributes")),
#                            file_chr = c("PHN_boundaries_AUS_May2017_V7_Shapefile",
#                                         "TESTFILE"),
#                            file_type_chr = c(".zip",".xlsx"),
#                            description_chr = c("Test shape file",
#                                                "Test xlsx file"),
#                            ds_file_ext_chr = c(".zip",".tab"))

## Note: Shape files or other zip files will only be downloaded for a local copy if dataset is published
purrr::walk(1:length(f_int_vec),
            ~{
              if(!(ds_ls$versionState=="DRAFT" & files_tb$file_type_chr[.x]==".zip")){
                save_dv_file(database_ui_chr = ds_url_chr,
                             filename_chr = files_tb$file_chr[.x],
                             repo_file_format = files_tb$ds_file_ext_chr[.x],
                             dataverse_chr = Sys.getenv("DATAVERSE_SERVER"),
                             save_type_chr = "original",
                             destination_path_chr = ready4::local_path_to_dv_data(save_dir_path_1L_chr = dv_model_dir_chr,
                                                                                     fl_nm_1L_chr = files_tb$file_chr[.x],
                                                                                     save_fmt_1L_chr = files_tb$file_type_chr[.x]))
              }
            })
## UPDATE dv import lup
>>>>>>> master
