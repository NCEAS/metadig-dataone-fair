library(tidyverse)
library(pbmcapply)

set_version_chain <- function(df, pid_target) {
    #print(paste0("Starting pid: ", pid_target))
    row <- filter(df, pid==pid_target)
    # if (!is.na(row$obsoleted_by)) {
    #     next
    # }
    pids <- c(pid_target)
    obsoletes_pid <- row$obsoletes

    # walk the obsoletes tree backwards and set the obsoleted_by field
    while (!is.na(obsoletes_pid)) {
        #print(paste0("Processing pid: ", pid_target))

        # Add the pid to the list of pids
        pids <- c(obsoletes_pid, pids)

        # Record the obsoleted_by relation
        df <- mutate(df, obsoleted_by = ifelse(pid==obsoletes_pid, pid_target, obsoleted_by))

        # Set up to process the next pid in the version chain
        # Get the row for the pid that obsoletes the doc, and look up what it obsoletes
        # If the row is not found, then just set it to NA (this indicates a broken version chain)
        pid_target <- obsoletes_pid
        row <- filter(df, pid==obsoletes_pid)
        obsoletes_pid <- ifelse(nrow(row) > 0,  row$obsoletes, NA)
        if (nrow(row) == 0) {
            print(paste0("Broken version chain. Missing row for pid: ", pid_target))
        }
    }

    # Set the serial version for the returned pids
    # This depends on the pids being in order from oldest to newest
    cur_version <- 1
    for (p in pids) {
        df <- mutate(df, ser_version = ifelse(pid==p, cur_version, ser_version))
        cur_version <- cur_version + 1
    }

    return(df)
}

# Test the function with simulated data
# df <- data.frame(pid = c("p1", "p2", "p3", "p4", "p5"),
#                  obsoletes = c(NA, "p1", "p2", "p0", "p4"),
#                  obsoleted_by = c(NA, NA, NA, NA, NA),
#                  ser_version = c(NA, NA, NA, NA, NA),
#                  stringsAsFactors = FALSE)
# df
# i <- 1
# pb <- progressBar(max = nrow(df), style = "ETA")
# for (pid in df$pid) {
#     df <- set_version_chain(df, pid)
#     setTxtProgressBar(pb, i)
#     i <- i + 1
# }
# close(pb)
# df


runs_temp <- mutate(runs, obsoleted_by = NA, ser_version = NA)

i <- 1
row_slice <- runs_temp[11001:11100,]$pid
pb <- progressBar(max = length(row_slice), style = "ETA")
for (pid in row_slice) {
    runs_temp <- set_version_chain(runs_temp, pid)
    setTxtProgressBar(pb, i)
    i <- i+1
}
close(pb)


