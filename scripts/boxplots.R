# Packages: Install & load as needed
packages <- c('raster', 'data.table', 'ncdf4', 'lubridate', 'ggplot2', 'tidyverse', 'tidyr', 'plyr', 'assertthat', 'quantreg', 'glue', 'vctrs')
to_install <- setdiff(packages, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install)
invisible(lapply(packages, require, character.only = TRUE))

# Read data
dta_boxplots <- readRDS("./data/dta_boxplots.rds")

# Factor order for plotting
position_levels <- c('HWG', 'HWL', 'HWH', 'HWO', 'June-Sep.')

# Custom stat for boxplot with flexible quantiles
stat_boxplot_custom <- function(qs = c(0.1, 0.25, 0.5, 0.75, 0.9), ...) {
  ggplot2::stat_summary(
    fun.data = function(y) {
      stats <- as.numeric(quantile(y, qs, na.rm = TRUE))
      names(stats) <- c("ymin", "lower", "middle", "upper", "ymax")
      data.frame(
        ymin   = stats[1],
        lower  = stats[2],
        middle = stats[3],
        upper  = stats[4],
        ymax   = stats[5]
      )
    },
    geom = "boxplot",
    ...
  )
}

# Plot
boxplots <- ggplot(dta_boxplots, aes(x = factor(Type, levels = position_levels), y = value, fill = variable)) +
  geom_vline(xintercept = 1:4 + 0.5, color = "grey") +
  stat_boxplot_custom(qs = c(0.1, 0.25, 0.5, 0.75, 0.9), outlier.shape = NA, notch = FALSE, width = 0.67, position = position_dodge2(width = 0.67, preserve = "single")) +
  scale_fill_manual(
    values = c(
      "3_days_before" = "#D84315",
      "during_HW"     = "#4CAF50",
      "3_days_after"  = "#0288D1",
      "June-Sep."     = "#9E9E9E"
    ),
    labels = c(
      "3_days_before" = "3 days before",
      "during_HW"     = "during HW",
      "3_days_after"  = "3 days after",
      "June-Sep."     = "June-Sep."
    ),
    breaks = c('3_days_before', 'during_HW', '3_days_after', 'June-Sep.')
  ) +
  scale_y_continuous(breaks = c(-5, -3, -1, 1, 3, 5)) +
  coord_cartesian(ylim = c(-3.5, 5)) +
  labs(y = "PET-P [mm/d]", x = NULL) +
  theme_bw() +
  theme(
    panel.grid.major.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x = element_text(size = 12, face = "bold", colour = "black"),
    legend.position = "bottom",
    legend.title = element_blank()
  )

# Print plot
print(boxplots)

# To save, uncomment the next line:
# ggsave(boxplots, file = "./figures/boxplots.png", height = 10, width = 15, units = "cm", bg = "white")