
library(xml2)
library(purrr)
library(dplyr)

extract_value <- function(nodeset, xpath) {
    node <- xml2::xml_find_all(nodeset, xpath)
    value <- ifelse(purrr::is_empty(node), as.character(NA), xml_text(node))
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
    results <- xml2::xml_find_all(doc, "/ns0:run/result")
    
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
#' 
#' 
#' @examples
#' vec <- 1:13
#' slice(vec, 3)
slice <- function(input, by=2) {
    starts <- seq(1,length(input),by)
    tt <- lapply(starts, function(y) input[y:(y+(by-1))])
    lmap(tt, function(x) x[!is.na(x)])
}

process_xml_file <- function(filename) {
    tryCatch({
        doc <- read_xml(filename)
        
        run_data <- extract_run(doc, filename)
        check_data <- extract_checks(doc)
        
        list(run = run_data, check = check_data)
        
    }, error = function(e) {
        # Log the problematic filename to your console
        message(paste("CRITICAL ERROR in file:", filename, "\nReason:", e$message))
        
        # Return empty data frames so bind_rows() won't break later
        list(run = data.frame(run_id = "",
                              obj_id = "",
                              series_id = "",
                              suite_id = "",
                              run_status = "error",
                              origin_mn = "",
                              date_uploaded = "", 
                              format_id = "", 
                              obsoletes = "",
                              filename = filename), check = data.frame())
    })
}

extract_check_data <- function(input_docs, out_runs, out_checks){

    files <- dir(input_docs, full.names = TRUE, recursive = TRUE)
    
    existing_runs <- data.frame()
    existing_checks <- data.frame()
    
    # if output files already exist, read them and filter
    if (file.exists(out_runs)) {
        existing_runs <- readRDS(out_runs)
        processed_files <- unique(existing_runs$filename)
        files <- setdiff(files, processed_files)
    }
    
    if (file.exists(out_checks)) {
        existing_checks <- readRDS(out_checks)
    }
    
    # if there is nothing new to process, stop here
    if (length(files) == 0) {
        message("All files have already been processed! Nothing to do.")
        return(invisible(NULL))
    }
    
    message(paste("Processing", length(files), "new XML files..."))
    
    plan(multisession, workers = 5)
    all_results <- future_lapply(files, process_xml_file)
    
    new_runs_df <- bind_rows(lapply(all_results, `[[`, "run"))
    new_checks_df <- bind_rows(lapply(all_results, `[[`, "check"))
    
    # combine old data with new data
    runs_df <- bind_rows(existing_runs, new_runs_df)
    checks_df <- bind_rows(existing_checks, new_checks_df)
    
    # save the combined dataset back down
    saveRDS(runs_df, out_runs)
    saveRDS(checks_df, out_checks)
    
    message("Processing complete and data appended.")
}

    
