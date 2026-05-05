library(xml2)
library(dplyr)
library(purrr)

out_csv <- "check-results-0.4.0.csv"
out_run <- "run-results-0.4.0.csv"
input_docs <- "../metadig-data/docs-run-0.4.0"

extract_value <- function(nodeset, xpath) {
    node <- xml_find_all(nodeset, xpath)
    value <- ifelse(is_empty(node), as.character(NA), xml_text(node))
    return(value)
}

extract_run <- function(doc, filename) {
    run_id <- extract_value(doc, "/ns0:run/id")
    obj_id <- extract_value(doc, "/ns0:run/objectIdentifier")
    suite_id <- extract_value(doc, "/ns0:run/suiteId")
    run_status <- extract_value(doc, "/ns0:run/runStatus")
    
    origin_mn <- extract_value(doc, "/ns0:run/sysmeta/originMemberNode")
    date_uploaded <- extract_value(doc, "/ns0:run/sysmeta/dateUploaded") # Fixed typo
    format_id <- extract_value(doc, "/ns0:run/sysmeta/formatId")
    obsoletes <- extract_value(doc, "/ns0:run/sysmeta/obsoletes")
    series_id <- extract_value(doc, "/ns0:run/sysmeta/seriesId")
    
    data.frame(run_id, obj_id, series_id, suite_id, run_status, origin_mn, 
               date_uploaded, format_id, obsoletes, filename, stringsAsFactors = FALSE)
}

extract_checks <- function(doc) {
    run_id <- extract_value(doc, "/ns0:run/id")
    results <- xml_find_all(doc, "/ns0:run/result")
    
    # If no results, return empty dataframe early to prevent map_df errors
    if(length(results) == 0) return(data.frame()) 
    
    map_df(.x = results, .f = extract_result) %>%
        mutate(run_id = run_id)
}

extract_result <- function(result) {
    check_id <- xml_text(xml_find_all(result, "check/id"))
    check_status <- xml_text(xml_find_all(result, "status"))
    # Output nodes can be repeated, so concatenate them together into one string
    output_nodes <- xml_find_all(result, "output")
    # for each output node extract the text and the id attribute
    check_output <- xml_text(output_nodes)
    data_pids <- xml_attr(output_nodes, "identifier")
    n_pids <- length(data_pids)
    result_df <- data.frame(check_id = rep(check_id, n_pids),
                            check_status = rep(check_status,n_pids),
                            data_pids,
                            check_output,
                            stringsAsFactors = FALSE)
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

process_xml_file <- function(filename) {
    # Read and parse the XML exactly once
    doc <- read_xml(filename)
    
    # Extract both pieces
    run_data <- extract_run(doc, filename)
    check_data <- extract_checks(doc)
    
    # Return as a named list
    list(run = run_data, check = check_data)
}

files <- dir(input_docs, full.names = TRUE)

plan(multisession, workers = 10)

all_results <- future_lapply(files, process_xml_file)

runs_df <- bind_rows(lapply(all_results, `[[`, "run"))
checks_df <- bind_rows(lapply(all_results, `[[`, "check"))

out_run_rds <- "run-results-0.4.0.rds"
out_csv_rds <- "check-results-0.4.0.rds"

# Save them as compressed binary files
saveRDS(runs_df, out_run_rds)
saveRDS(checks_df, out_csv_rds)

    
