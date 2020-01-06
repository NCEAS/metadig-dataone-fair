library(xml2)
library(dplyr)
library(purrr)
library(parallel)
library(ggplot2)
library(pbmcapply)

extract_value <- function(nodeset, xpath) {
  node <- xml_find_all(nodeset, xpath)
  value <- ifelse(is_empty(node), as.character(NA), xml_text(node))
  return(value)
}

extract_run <- function(filename) {
  if (is.na(filename)) {
    return(data.frame())
  }
  doc <- read_xml(file(paste0("reports/", filename)))
  run_id <- extract_value(doc, "/ns0:run/id")
  obj_id <- extract_value(doc, "/ns0:run/objectIdentifier")
  suite_id <- extract_value(doc, "/ns0:run/suiteId")
  run_status <- extract_value(doc, "/ns0:run/runStatus")

  origin_mn <- extract_value(doc, "/ns0:run/sysmeta/originMemberNode")
  date_uplaoded <- extract_value(doc, "/ns0:run/sysmeta/dateUploaded")
  format_id <- extract_value(doc, "/ns0:run/sysmeta/formatId")
  obsoletes <- extract_value(doc, "/ns0:run/sysmeta/obsoletes")
  series_id <- extract_value(doc, "/ns0:run/sysmeta/seriesId")

  runs_df <- data.frame(run_id, obj_id, series_id, suite_id, run_status, origin_mn, date_uplaoded, format_id, obsoletes, filename, stringsAsFactors = FALSE)
  return(runs_df)
}

extract_checks <- function(filename) {
  if (is.na(filename)) {
    return(data.frame())
  }
  doc <- read_xml(file(paste0("reports/", filename)))
  run_id <- extract_value(doc, "/ns0:run/id")
  results <- xml_find_all(doc, "/ns0:run/result")
  check_df <- map_df(.x = results, .f = extract_result) %>%
    mutate(run_id = run_id)

  return(check_df)
}

extract_result <- function(result) {
  check_id <- xml_text(xml_find_all(result, "check/id"))
  check_name <- xml_text(xml_find_all(result, "check/name"))
  check_type <- xml_text(xml_find_all(result, "check/type"))
  check_level <- xml_text(xml_find_all(result, "check/level"))
  check_status <- xml_text(xml_find_all(result, "status"))
  # Output nodes can be repeated, so concatenate them together into one string
  output_nodes <- xml_find_all(result, "output")
  check_output <- paste(xml_text(output_nodes), sep = ' ', collapse = '')
  result_df <- data.frame(check_id, check_name, check_type, check_level, check_status, check_output, stringsAsFactors = FALSE)
  return(result_df)
}

#' Split up a vector into equal sized chunks
#'
#' @param input An input vector.
#' @param by The length of the resulting vectors.
#' @examples
#' vec <- 1:13
#' slice(vec, 3)
slice <- function(input, by=2) {
  starts <- seq(1,length(input),by)
  tt <- lapply(starts, function(y) input[y:(y+(by-1))])
  lmap(tt, function(x) x[!is.na(x)])
}

process_reports <- function(file_chunk) {
  runs <- map_df(.x = file_chunk, .f = extract_run)
  checks <- map_df(.x = file_chunk, .f = extract_checks)
  return(list(runs,checks))
}

par_process <- function(suite_name) {
  file_list_full <- list.files(path = "reports", pattern = "\\.xml")
  #file_list <- sample(file_list_full, size=100, replace=F)
  slices <- c(seq(1300, 3900, 100), 4026)
  slice_min <- 1201
  for (slice_max in slices) {
    print(paste0("Processing slices: ", slice_min, " - ", slice_max))
    file_chunks <- slice(file_list_full, 200)[slice_min:slice_max]

    # Time it serially for a benchmark
    # system.time({
    #   output_serial <- process_reports(file_list)
    # })

    # Time it in parallel and compare results
    timedata = data.frame()
    for (numCores in (c(50))) {
      print(paste0("Using cores: ", numCores))
      t <- system.time({
        output_parallel <- mclapply(file_chunks, process_reports, mc.cores = numCores)
        #output_parallel <- pbmclapply(file_chunks, process_reports, mc.cores = numCores)
        runs <- bind_rows(map(output_parallel, 1))
        checks <- bind_rows(map(output_parallel, 2))
      })
      timings <- list(numCores=numCores, user=t[[1]]+t[[4]], system=t[[2]]+t[[5]], elapsed=t[[3]])
      print(as.data.frame(timings))
      timedata <- bind_rows(timedata, timings)
    }
    write.csv(runs, paste0(suite_name, "-runs-", slice_max, ".csv"))
    write.csv(checks, paste0(suite_name, "-checks-", slice_max, ".csv"))
    runs_and_checks <- list(runs=runs, checks=checks)
    save(runs_and_checks, file = paste0(suite_name, "-runs-and-checks-", slice_max, ".rda"))
    print(paste0("Finished slice: ", slice_max))
    slice_min <- slice_max + 1
  }
}

suite_name <- "check-data/fair-2.1"
par_process(suite_name)

# ggplot(timedata_all, mapping = aes(x = numCores, y = elapsed)) +
#   geom_point() +
#   geom_line() +
#   facet_wrap(facets = ~chunksize, ncol = 5) +
#   theme_bw()
#geom_point(mapping = aes(y=user+system))
