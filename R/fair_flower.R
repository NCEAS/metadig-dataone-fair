fair_flower <- function(df, title = NA, filename = NA) {
    flower_df <- df %>%
        group_by(metric) %>%
        filter(ym == max(ym)) %>%
        rename(score=mean) %>%
        separate(col=metric, into=c(NA, "label"), sep=" ") %>%
        mutate(weight = 1, category = NA, goal = label)
    flower_colors <- c("#c70a61", "#ff582d", "#1a6379", "#60c5e4")
    plot_obj <- plot_flower(flower_df, colors = flower_colors, fixed_colors = TRUE, filename = filename, title = title)
    return(plot_obj)
}
