library(httr)
library(curl)
library(future.apply)

docs_dir <- "../metadig-data/docs-run-0.4.0"
suite <- "FAIR-suite-0.4.0"

if (!dir.exists(docs_dir)){
    dir.create(docs_dir)
}

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


url_stub <- paste0("https://api.dataone.org/quality/runs/", suite, "/")
urls <- paste0(url_stub, pids)

destinations <- file.path(docs_dir, paste0(gsub("/", "_", pids), ".xml"))
already_downloaded <- file.exists(destinations)

# Filter down to only what we need
urls_to_get <- urls[!already_downloaded]
pids_to_get <- pids[!already_downloaded]
dests_to_get <- destinations[!already_downloaded]

cat("Skipping", sum(already_downloaded), "files. Downloading", length(urls_to_get), "files...\n")

plan(multisession, workers = 10)

download_file <- function(i) {
    url <- urls_to_get[i]
    file_name <- dests_to_get[i]
    
    # tryCatch is critical here so one timeout doesn't crash the whole batch
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

results <- future_lapply(seq_along(urls_to_get), download_file)
