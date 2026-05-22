#' Extract Latest Metric State and Render a FAIR Flower Plot
#'
#' @param df data frame containing chronological monthly scores with columns \code{metric} and \code{mean}.
#' @param title
#' @param filename
#'
#' @return A polar ggplot object
#'
#' @importFrom dplyr group_by filter rename mutate %>%
#' @importFrom tidyr separate
#' 
fair_flower <- function(df, title = NA, filename = NA) {
    flower_df <- df %>%
        group_by(metric) %>%
        filter(ym == max(ym)) %>%
        rename(score=mean) %>%
        separate(col=metric, into=c(NA, "label"), sep=" ") %>%
        mutate(category = NA, goal = label)
    flower_colors <- c("#c70a61", "#ff582d", "#1a6379", "#60c5e4")
    plot_obj <- plot_flower(flower_df, colors = flower_colors, fixed_colors = TRUE, filename = filename, title = title)
    return(plot_obj)
}



#' Generate a Polar FAIR Flower/Petal Diagram
#' 
#' Derived from `fair_flower` from the `flowerplot` package, which I couldn't get to work
#' correctly and don't have time to maintain.
#'
#' @param .Data A data frame containing individual metric rows with columns \code{score} and \code{label}.
#' @param title Character string title for the plot canvas. Defaults to \code{NA}.
#'
#' @return A theme-void polar coordinate ggplot visual asset with an absolute center score.
#'
#' @importFrom dplyr mutate %>% .data
#' @import ggplot2
#' 
make_flower_plot <- function(.Data, title = NA) {
    
    if(!"weight" %in% colnames(.Data)) .Data$weight <- 1
    
    blank_circle_rad <- 42
    light_line <- "grey90"
    white_fill <- "white"
    dark_line  <- "grey20"
    
    flower_colors <- c(
        "Findable"      = "#c70a61", 
        "Accessible"    = "#ff582d", 
        "Interoperable" = "#1a6379", 
        "Reusable"      = "#60c5e4"
    )
    
    .Data <- .Data %>% 
        dplyr::mutate(
            score_100 = score * 100,
            pos = sum(.data$weight) - (cumsum(.data$weight) - 0.5 * .data$weight),
            pos_end = sum(.data$weight)
        )
    
    p_limits <- c(0, .Data$pos_end[1])
    
    plot_obj <- ggplot2::ggplot(data = .Data, ggplot2::aes(x = pos, y = score_100, fill = label, width = weight)) +
      
        ggplot2::geom_bar(ggplot2::aes(y = 100), stat = "identity", color = light_line, fill = white_fill, size = 0.2) + 
        ggplot2::geom_errorbar(ggplot2::aes(ymin = 100, ymax = 100), size = 0.5, color = light_line) +
        
        ggplot2::geom_bar(stat = "identity", color = dark_line, size = 0.2) + 
        ggplot2::geom_errorbar(ggplot2::aes(ymin = score_100, ymax = score_100), size = 0.5, color = dark_line) + 
        ggplot2::geom_errorbar(ggplot2::aes(ymin = 0, ymax = 0), size = 0.5, color = dark_line) + 
        
        ggplot2::coord_polar(start = 0) + 
        ggplot2::scale_x_continuous(breaks = .Data$pos, limits = p_limits) + 
        ggplot2::scale_y_continuous(limits = c(-blank_circle_rad, 110)) +
        ggplot2::scale_fill_manual(values = flower_colors)
    
    mean_score <- round(mean(.Data$score_100, na.rm = TRUE))
    plot_obj <- plot_obj + ggplot2::annotate(
        "text", 
        label = mean_score, 
        x = 0, y = -blank_circle_rad, 
        hjust = 0.5, vjust = 0.5, size = 9, color = dark_line, fontface = "bold"
    )
    
    if (!is.na(title)) {
        plot_obj <- plot_obj + ggplot2::labs(title = title)
    }
    
    plot_obj <- plot_obj + ggplot2::theme_void() + 
        ggplot2::theme(
            plot.title = ggplot2::element_text(hjust = 0.5, size = 20),
            legend.position = "none"
        )
    
    # Inner score percentages on the petals themselves
    #plot_obj <- plot_obj + ggplot2::geom_text(
    #    ggplot2::aes(label = round(score_100)), x = .data$pos), 
    #    y = 65, hjust = 0.5, vjust = 0.5, size = 3.5, color = "black"
    #)
    return(plot_obj)
}
