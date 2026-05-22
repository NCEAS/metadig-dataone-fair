#' Assign sequence id to version chains
#'
#' Resolves the root sequence identifier for a dataset of object version chains.
#' Instead of traversing chains row-by-row with recursion or element-wise mapping, 
#' this function uses a highly optimized, vectorized hash-map approach. It maps 
#' all rows up their respective `obsoletes` chains simultaneously via named vector 
#' subsetting. This function is necessary because sometimes sequenceId is not populated
#' correctly in the runs table. This seems largely due to the true root of the sequence not
#' being present in the table.
#'
#' @param df A data frame containing at least \code{pid} and \code{obsoletes} character columns.
#'
#' @return The original data frame mutated with a newly calculated \code{sequenceId} column.
#'
#' @importFrom dplyr mutate %>%
#' @importFrom stats setNames
#' @export
assign_sequence_id <- function(df) {
    
    # create the lookup dictionary
    parent_lookup <- setNames(df$obsoletes, df$pid)
    current_pids <- df$pid
    
    # prevent infinite looping
    max_depth <- 1000 
    iter <- 0
    
    # walk the entire column up the chain simultaneously
    while(iter < max_depth) {
        iter <- iter + 1
        
        next_pids <- unname(parent_lookup[current_pids])
        has_parent <- !is.na(next_pids) & next_pids != ""
        
        # if no parent, we've hit the root
        if (!any(has_parent)) {
            break
        }
        
        # move only rows with parent up chain
        current_pids[has_parent] <- next_pids[has_parent]
    }
    
    if (iter == max_depth) {
        warning("Max depth reached. You may have circular obsolete chains in your data.")
    }
    
    # attach final column
    df_fixed <- df %>%
        dplyr::mutate(sequenceId = current_pids)
    
    return(df_fixed)
}