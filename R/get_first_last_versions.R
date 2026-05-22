#' Get first and last versions of a dataset
#'
#' Vectorized way to retrieve the first and last versions of a sequenceId. This
#' is considerably faster than doing it the dplyr way. 
#'
#' @param data data frame from `get_agg_data`
#'
#' @return data.frame with `version_position` column added
#' @importFrom dplyr case_when
#' @export
get_first_last_versions <- function(data) {
    df_sorted <- data[order(data$sequenceId, data$dateUploaded), ]
    # find sequence boundaries
    is_first <- !duplicated(df_sorted$sequenceId)
    is_last  <- !duplicated(df_sorted$sequenceId, fromLast = TRUE)
    
    df_filtered <- df_sorted[is_first | is_last, ]
    
    is_first_f <- !duplicated(df_filtered$sequenceId)
    is_last_f  <- !duplicated(df_filtered$sequenceId, fromLast = TRUE)
    
    df_filtered$version_position <- dplyr::case_when(
        is_first_f & is_last_f ~ "only_version",
        is_first_f             ~ "first",
        TRUE                   ~ "last"
    )
    return(df_filtered)
}