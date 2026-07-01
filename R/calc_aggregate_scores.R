#' Compute Aggregate FAIR Scores from Apache Arrow Repositories
#' 
#' This is probably not needed but might be nice if we don't trust the scorer
#'
#' @param checks_dir Character string path to the partitioned checks parquet directory.
#' @param runs_dir Character string path to the partitioned execution runs parquet directory.
#'
#' @return A tidy, long-format data frame containing processed, wide-mapped FAIR dimensions.
#'
#' @importFrom arrow open_dataset
#' @import dplyr
#' @importFrom tidyr pivot_wider
#' @export
calc_aggregate_scores <- function(checks_dir, runs_dir, check_exclusions){

    
    checks <- open_dataset(checks_dir)
    runs <- open_dataset(runs_dir)
    
    runs_clean <- runs %>% 
        filter(!(run_status %in% c("ERROR", "failure", "processing")))
    
    checks_calc <- checks %>% 
        # filter out erroring checks
        filter(!(check_name %in% check_exclusions)) %>% 
        mutate(check_status = if_else(grepl("error", tolower(check_output)), "ERROR", check_status)) %>% 
        filter(check_status != "ERROR") %>% 
        mutate(
            # prep for formula
            Rp = if_else(check_level == "REQUIRED" & check_status == "SUCCESS", 1, 0),
            Rf = if_else(check_level == "REQUIRED" & check_status == "FAILURE", 1, 0),
            Op = if_else(check_level == "OPTIONAL" & check_status == "SUCCESS", 1, 0)
        ) %>%
        inner_join(runs_clean) %>% 
        group_by(obj_id, date_uploaded, format_id, origin_mn, obsoletes, obsoleted_by, check_type) %>%
        # FAIR score by category
        summarise(
            type_Rp = sum(Rp, na.rm = TRUE),
            type_Rf = sum(Rf, na.rm = TRUE),
            type_Op = sum(Op, na.rm = TRUE),
            # apply formula: (Rp + Op) / (Rp + Rf + Op)
            fair_score_by_type = (type_Rp + type_Op) / (type_Rp + type_Rf + type_Op),
        ) %>%
        ungroup() %>% 
        # overall FAIR score
        group_by(obj_id, date_uploaded, format_id, obsoletes, obsoleted_by, origin_mn) %>% 
        mutate(
            # sum the type-level counts across all types for this obj_id
            total_Rp = sum(type_Rp, na.rm = TRUE),
            total_Rf = sum(type_Rf, na.rm = TRUE),
            total_Op = sum(type_Op, na.rm = TRUE),
            # calculate the overall score
            fair_score = (total_Rp + total_Op) / (total_Rp + total_Rf + total_Op)
        ) %>%
        select(-total_Rp, -total_Rf, -total_Op, -type_Op, -type_Rp, -type_Rf) %>% 
        collect()
    
    checks_agg <- checks_calc %>% 
        tidyr::pivot_wider(names_from = check_type, values_from = fair_score_by_type) %>% 
        rename(pid = obj_id,
               dateUploaded = date_uploaded,
               formatId = format_id,
               datasource = origin_mn,
               scoreFindable = Findable,
               scoreAccessible = Accessible,
               scoreInteroperable = Interoperable,
               scoreReusable = Reusable,
               obsoletedBy = obsoleted_by
        )
    
    return(checks_agg)
}

    