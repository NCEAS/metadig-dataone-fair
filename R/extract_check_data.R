#' Extract a character text value from a nodeset using an XPath
#'
#' @importFrom xml2 xml_find_all xml_text
#' @importFrom purrr is_empty
extract_value <- function(nodeset, xpath) {
    node <- xml2::xml_find_all(nodeset, xpath)
    value <- ifelse(purrr::is_empty(node), as.character(NA), xml_text(node))
    return(value)
}

#' Extract root run metadata attributes from an XML document
extract_run <- function(doc, filename) {
    run_id <- extract_value(doc, "/ns0:run/id")
    obj_id <- extract_value(doc, "/ns0:run/objectIdentifier")
    suite_id <- extract_value(doc, "/ns0:run/suiteId")
    run_status <- extract_value(doc, "/ns0:run/runStatus")

    origin_mn <- extract_value(doc, "/ns0:run/sysmeta/originMemberNode")
    date_uploaded <- extract_value(doc, "/ns0:run/sysmeta/dateUploaded") # Fixed typo
    format_id <- extract_value(doc, "/ns0:run/sysmeta/formatId")
    obsoletes <- extract_value(doc, "/ns0:run/sysmeta/obsoletes")
    obsoleted_by <- extract_value(doc, "/ns0:run/sysmeta/obsoletedBy")
    series_id <- extract_value(doc, "/ns0:run/sysmeta/seriesId")

    data.frame(run_id, obj_id, series_id, suite_id, run_status, origin_mn,
               date_uploaded, format_id, obsoletes, obsoleted_by, filename, stringsAsFactors = FALSE)
}

#' Parse and aggregate check results nested within a run block
#'
#' @importFrom purrr map_df
#' @importFrom dplyr mutate %>%
#' @importFrom xml2 xml_find_all
extract_checks <- function(doc) {
    run_id <- extract_value(doc, "/ns0:run/id")
    results <- xml2::xml_find_all(doc, "/ns0:run/result")

    # If no results, return empty dataframe early to prevent map_df errors
    if(length(results) == 0) return(data.frame())

    map_df(.x = results, .f = extract_result) %>%
        mutate(run_id = run_id)
}

#' Map XML result properties into a single row data frame
#'
#' @importFrom xml2 xml_text xml_find_all
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
#'
#' @importFrom purrr lmap
slice <- function(input, by=2) {
    starts <- seq(1,length(input),by)
    tt <- lapply(starts, function(y) input[y:(y+(by-1))])
    lmap(tt, function(x) x[!is.na(x)])
}

#' Parse single XML report into run and check blocks safely
#'
#' @importFrom xml2 read_xml
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
                              run_status = "ERROR",
                              origin_mn = "",
                              date_uploaded = "",
                              format_id = "",
                              obsoletes = "",
                              filename = filename), check = data.frame())
    })
}

#' Batch Process MetaDIG XML Documents into Arrow Parquet Datasets
#'
#' Scans an input directory for XML documents, cross-references files against an
#' existing Apache Arrow database to prevent redundant parsing, processes new files
#' asynchronously via an isolated parallel background cluster, and appends the final partitioned
#' structures directly into separate Arrow Parquet output tracking hubs.
#'
#' @param input_docs Character string specifying source directory containing raw XML check files.
#' @param out_runs_dir Character string directory path where the run data should be stored (Partitioned by \code{origin_mn}).
#' @param out_checks_dir Character string target directory path where the individual check data should be stored (Partitioned by \code{check_type} and \code{check_level}).
#'
#' @return \code{invisible(NULL)} upon successful validation and file block write completion.
#'
#' @importFrom arrow open_dataset write_dataset
#' @import dplyr
#' @importFrom future plan multisession
#' @importFrom future.apply future_lapply
#' @export
#'
extract_check_data <- function(input_docs, out_runs_dir, out_checks_dir, workers = 10){

    files <- dir(input_docs, full.names = TRUE, recursive = TRUE)

    # check to make sure we don't already have the files
    if (dir.exists(out_runs_dir) && length(dir(out_runs_dir)) > 0) {
        existing_runs <- open_dataset(out_runs_dir)

        processed_files <- existing_runs %>%
            select(filename) %>%
            distinct() %>%
            collect() %>%
            pull(filename)

        files <- files[!(basename(files) %in% basename(processed_files))]
    }

    # stop if nothing new
    if (length(files) == 0) {
        message("All files have already been processed! Nothing to do.")
        return(invisible(NULL))
    }

    message(paste("Processing", length(files), "new XML files..."))

    # parallel process fheck docs
    plan(multisession, workers = workers)
    all_results <- future_lapply(files, process_xml_file)

    new_runs_df  <- bind_rows(lapply(all_results, `[[`, "run"))
    new_checks_df <- bind_rows(lapply(all_results, `[[`, "check"))

    # write_dataset handles creating the folder if it doesn't exist, and safely
    # generates a new unique chunk file inside it without reading or overwriting old data.
    if (nrow(new_runs_df) > 0) {
        write_dataset(
            dataset = new_runs_df,
            path = out_runs_dir,
            format = "parquet",
            partitioning = "origin_mn",
            basename_template = paste0("append-", format(Sys.time(), "%Y%m%d_%H%M%S"), "-{i}.parquet")
        )
    }

    # write checks (partitioned by check type and level)
    if (nrow(new_checks_df) > 0) {
        write_dataset(
            dataset = new_checks_df,
            path = out_checks_dir,
            format = "parquet",
            partitioning = c("check_type", "check_level"),
            basename_template = paste0("append-", format(Sys.time(), "%Y%m%d_%H%M%S"), "-{i}.parquet")
        )
    }

    message("Processing complete and new chunks appended to Arrow datasets.")
}


