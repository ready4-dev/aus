## Script to create the object from which S3 classes will be made.
##
## 1. Prerequisites
##    None
##
## 2. Make the ready4_class_make_tb object summarising the metadata about the S3 classes that we wish to create and export with this package.
aus_16_31_s3_make_tb <- ready4class::ready4_class_make_tb() %>%
  dplyr::bind_rows(tibble::tribble(
    ~ make_s3, ~ name_stub, ~ prototype, ~ prototype_checker_prefix, ~ prototype_namespace, ~ values, ~ allowed_values, ~ min_max_values, ~ start_end_values, ~ class_desc, ~ parent_class, ~ class_slots, ~ meaningful_names, ~include_classes,
    TRUE, "sp_import_lup", list("tibble"), list("is_"), list("tibble"),list(st_context = "character(0)"), NULL, NULL, NULL, "Readyforwhatsnext S3 class for tibble object lookup table of metadata about raw (un-processed) Australian spatial data to import.","vicinity_raw", NULL, NULL, NULL))

