library(xml2)
library(dplyr)
library(purrr)

extract_value <- function(nodeset, xpath) {
  node <- xml_find_all(nodeset, xpath)
  value <- ifelse(is_empty(node), as.character(NA), xml_text(node))
  return(value)
}

extract_run <- function(filename) {
  doc <- read_xml(file(filename))
  run_id <- extract_value(doc, "/ns0:run/id")
  obj_id <- extract_value(doc, "/ns0:run/objectIdentifier")
  suite_id <- extract_value(doc, "/ns0:run/suiteId")
  run_status <- extract_value(doc, "/ns0:run/runStatus")

  origin_mn <- extract_value(doc, "/ns0:run/sysmeta/originMemberNode")
  date_uplaoded <- extract_value(doc, "/ns0:run/sysmeta/dateUploaded")
  format_id <- extract_value(doc, "/ns0:run/sysmeta/formatId")
  obsoletes <- extract_value(doc, "/ns0:run/sysmeta/obsoletes")
  series_id <- extract_value(doc, "/ns0:run/sysmeta/seriesId")

  runs_df <- data.frame(run_id, obj_id, series_id, suite_id, run_status, origin_mn, date_uplaoded, format_id, obsoletes, stringsAsFactors = FALSE)
  return(runs_df)
}

extract_checks <- function(filename) {
  doc <- read_xml(file(filename))
  run_id <- extract_value(doc, "/ns0:run/id")
  results <- xml_find_all(doc, "/ns0:run/result")
  check_id <- xml_text(xml_find_all(results, "./check/id"))
  check_name <- xml_text(xml_find_all(results, "./check/name"))
  check_type <- xml_text(xml_find_all(results, "./check/type"))
  check_level <- xml_text(xml_find_all(results, "./check/level"))
  check_status <- xml_text(xml_find_all(results, "./status"))
  check_output <- xml_text(xml_find_all(results, "./output"))

  check_df <- data.frame(check_id, check_name, check_type, check_level, check_status, check_output, stringsAsFactors = FALSE) %>%
    mutate(run_id = run_id)
  return(check_df)
}

file_list <- list.files(path = ".", pattern = "\\.xml")
runs <- map_df(.x = file_list, .f = extract_run)
checks <- map_df(.x = file_list, .f = extract_checks)
