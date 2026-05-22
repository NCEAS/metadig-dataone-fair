#' Retrieve MetaDIG Assessment Documents
#'
#' Downloads MetaDIG XML check execution documents for a specified quality suite. 
#' The function first attempts to extract target persistent identifiers (PIDs) and node structures 
#' from a local file. If the CSV is missing, it falls back to a DataONE Quality API lookup.
#' Files are organized hierarchically on disk by repository node name and downloaded in parallel.
#'
#' @param pid_list A character string representing the file path to a local CSV file 
#'   containing columns \code{metadata_id} and \code{data_source}.
#' @param suite A character string specifying the evaluation suite, eg: "FAIR-suite-0.5.0"
#' @param docs_dir A character string defining the root target directory path where downloaded 
#'   XML files should be saved.
#'
#' @return Invisibly returns a list of HTTP status codes or error tokens from the 
#'   parallel batch download operation.
#'
#' @importFrom httr GET add_headers content
#' @importFrom future plan multisession
#' @importFrom future.apply future_lapply
#' @export
get_check_docs <- function(pid_list = NULL, suite, docs_dir){
    if (!dir.exists(docs_dir)){
        dir.create(docs_dir)
    }
    # first try a pid list extracted directly from DB
    if (file.exists(pid_list)){
        
        # kubectl exec metadig-pg-6 -- sh -c "export PGPASSWORD={$PASS}; psql -U metadig -d metadig -h 127.0.0.1 -c \"COPY (SELECT runs.metadata_id,  runs.timestamp, data_source FROM runs JOIN identifiers ON runs.metadata_id = identifiers.metadata_id WHERE suite_id = 'FAIR-suite-0.5.0') TO STDOUT WITH CSV HEADER\"" > output.csv
        
        pid_df <- read.csv(pid_list)
        pids <- pid_df$metadata_id
    } else { # if it doesn't exist, get it from the scorer
        response <- GET(
            url = "http://api.dataone.org/quality/scores/",
            query = list(
                id = "urn:node:CN",
                suite = suite
            ),
            add_headers(Accept = "text/csv")
        )
        data <- content(response, as = "parsed", type = "text/csv")
        pids <- data$pid
    }
    
    # create URLs to retrieve docs from
    url_stub <- paste0("https://api.dataone.org/quality/runs/", suite, "/")
    urls <- paste0(url_stub, pids)
    
    # create dirs to store files in (by node)
    clean_source <- gsub("^urn:node:", "", pid_df$data_source)
    
    destinations <- file.path(
        docs_dir, 
        clean_source, 
        paste0(gsub("/", "_", pids), ".xml")
    )
    
    unique_dirs <- unique(dirname(destinations))
    lapply(unique_dirs, dir.create, recursive = TRUE, showWarnings = FALSE)
    
    # don't download files we have already retrieved
    already_downloaded <- file.exists(destinations)
    
    urls_to_get <- urls[!already_downloaded]
    pids_to_get <- pids[!already_downloaded]
    dests_to_get <- destinations[!already_downloaded]
    
    cat("Skipping", sum(already_downloaded), "files. Downloading", length(urls_to_get), "files...\n")
    
    # download files in parallel
    plan(multisession, workers = 10)
    
    url <- urls_to_get[i]
    file_name <- dests_to_get[i]
    
    system.time(results <- future_lapply(seq_along(urls_to_get), download_file, url = url, file_name = file_name))
}

#' Download a Single XML File Safely to Disk
#'
#' @importFrom httr GET add_headers write_disk timeout status_code
download_file <- function(url, file_name) {
    
    tryCatch({
        response <- GET(url, 
                        add_headers(Accept = "application/xml"), 
                        write_disk(file_name, overwrite = TRUE), 
                        timeout(30))
        
        if (status_code(response) != 200) {
            warning(sprintf("Failed to download %s: HTTP %d", url, status_code(response)))
        }
        return(status_code(response))
        
    }, error = function(e) {
        warning(sprintf("Error downloading %s: %s", url, e$message))
        return(NA)
    })
}


