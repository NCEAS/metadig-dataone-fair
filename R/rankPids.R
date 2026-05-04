library(dplyr)
library(uuid)
  
# Recurse either forward or backward along the obsolesence chain
getPidSeries <- function(x, series, forward, backward) {
    if(forward && backward) stop("specify either forward or backward")
    if(forward) {
        #cat(sprintf("forward, pid: %s\n", pid))
        series <- append(series, x)
        currentRow = metrics[which(metrics$pid==x),]
        obsoletedBy <- currentRow[1,"obsoletedBy"]
        if(is.null(obsoletedBy) || is.na(obsoletedBy)) return(series)
        nextRow <- metrics[metrics$pid == obsoletedBy,]
        nextPid <- nextRow[1, "pid"]
        if(is.na(nextPid)) return(series)
        series <- getPidSeries(nextPid, series, forward=TRUE, backward=FALSE)
        return(series)
    } else if (backward) {
        series <- append(series, x)
        currentRow = metrics[which(metrics$pid==x),]
        obsoletes <- currentRow[1,"obsoletes"]
        if(is.null(obsoletes) || is.na(obsoletes)) return(series)
        nextRow <- metrics[metrics$pid == obsoletes,]
        nextPid <- nextRow[1, "pid"]
        if(is.na(nextPid)) return(series)
        series <- getPidSeries(nextPid, series, forward=FALSE, backward=TRUE)
        return(series)
    } else {
        error("must specify forward or backward")
    }
}
  
metrics.orig <- read.csv("FAIR-scores.csv", stringsAsFactors=FALSE)
metrics <- metrics.orig[order(metrics.orig$dateUploaded),]
  
# First order the data by date
for (row in 1:nrow(metrics)) {
    #pid <- "urn:uuid:0f58d5ee-5d84-4fb5-ada9-0318fa46a25f"
    #pid <= "doi:10.5063/F17M0660"
    
    currentPid <- metrics[row, "pid"]
    sid <- metrics[row, "seriesId"]
    # If seriesId is found, then this row has already been processed.
    if(!is.na(sid)) {
      cat(sprintf("skipping %s, row: %d\n", currentPid, row))
      next
    }
    cat(sprintf("pid: %s, row: %d\n", currentPid, row))
    pidSeries <- list()
    pidSeriesForward <-  unlist(getPidSeries(currentPid, pidSeries, forward=TRUE, backward=FALSE))
    #cat(sprintf("rows %d\n", length(pidSeriesForward)))
    pidSeriesBackward <- unlist(getPidSeries(currentPid, pidSeries, forward=FALSE, backward=TRUE))
    #cat(sprintf("rows %d\n", length(pidSeriesBackward)))
    thisPidSeries <- unique(append(pidSeriesForward, pidSeriesBackward))
    
    # Create a df that holds the pids and the upload datetime 
    # This will be sorted and then a version number assinged to each pid
    pids <- data.frame("pid" = thisPidSeries, "dateUploaded" = rep("", length(thisPidSeries)), stringsAsFactors = FALSE)

    # Insert the dateUploaded for each pid
    for (thisRow in 1:nrow(pids)) {
        thisPid <- pids[thisRow, "pid"]
        du <- metrics[metrics$pid == thisPid, "dateUploaded"]
        pids$dateUploaded[pids$pid == thisPid] <- du
    }
    
    sortedPids <- pids[order(pids$dateUploaded),]
    # Now that the pids are sorted by dateUploaded, insert the version and
    # seriesId into the original 'metrics' df
    icnt <- 1
    seriesId = UUIDgenerate()
    for (thisRow in 1:nrow(pids)) {
        thisPid <- pids[thisRow, "pid"]
        metrics$version[metrics$pid == thisPid] <- icnt
        metrics$seriesId[metrics$pid == thisPid] <- seriesId
        icnt = icnt + 1
    }
}

# TODO: write out each line as it is ready, instead of waiting until the very end, 
# as this program takes a really long time to run.
write.csv(metrics, "FAIR-scores-ranked.csv")
