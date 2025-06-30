# Load/install required packages only once
packages <- c('raster', 'data.table', 'ncdf4', 'lubridate', 'ggplot2', 'tidyverse', 'tidyr', 'plyr', 'dplyr', 'ggnewscale', 'RColorBrewer', 'ggpubr', 'grid')
to.install <- packages[!packages %in% installed.packages()[, 'Package']]
if (length(to.install)) install.packages(to.install)
invisible(lapply(packages, require, character.only = TRUE))

# Common settings
prob_levels <- c("HWG", "HWL", "HWH", "HWO", "June-Sep.")
ct_levels <- c('A', 'C', 'N', 'NE', 'E', 'SE', 'S', 'SW', 'W', 'NW', 'IF')
fill_colors <- c("HWG" = "#E41A1C", "HWL" = "#377EB8", "HWH" = "#4DAF4A", "HWO" = "#FF7F00", "June-Sep." = "#999999", "0 records" = "white")
xintercepts <- c(1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5, 8.5, 9.5, 10.5)
y_limits <- c(0, 66.7)
y_breaks <- seq(10, 70, 10)

# Data and added_CT for each period
plot_info <- list(
  before = list(
    file = "./data/dat2_prob_before.rds",
    added_CT = data.table(x = c(1.6, 1.8, 2.2, 2.8, 3.2, 3.6, 3.8, 4.0, 4.2, 6.0, 6.2, 6.6, 6.8, 7.0, 8.6), y = 0)
  ),
  during = list(
    file = "./data/dat2_prob_during.rds",
    added_CT = data.table(x = c(2.6, 3.8, 4.2, 5, 5.2, 8.6), y = 0)
  ),
  after = list(
    file = "./data/dat2_prob_after.rds",
    added_CT = data.table(x = c(3.8, 4.2, 4.8), y = 0)
  )
)

# Plot function
make_col_plot <- function(data_file, added_CT, show_legend = FALSE) {
  dat <- readRDS(data_file)
  dat$Prob <- factor(dat$Prob, levels = prob_levels)
  dat$CT <- factor(dat$CT, levels = ct_levels)
  p <- ggplot(dat, aes(x = CT, y = Value, fill = Prob)) +
    geom_col(position = "dodge") +
    theme_bw() +
    scale_y_continuous(limits = y_limits, breaks = y_breaks) +
    theme(panel.grid.major.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.x = element_blank(),
          axis.text.x = element_text(size = 17, face = "bold", colour = "black"),
          axis.title.y = element_text(size = 27),
          axis.text.y = element_text(size = 15),
          legend.text = element_text(size = 20),
          legend.position = if (show_legend) c(.88, .75) else "none") +
    geom_vline(xintercept = xintercepts, color = 'grey') +
    labs(y = " ", size = 27) +
    geom_point(data = added_CT, mapping = aes(x = x, y = y, fill = '0 records'), shape = 4, size = 5, color = 'black') +
    scale_fill_manual(name = "", values = fill_colors, breaks = prob_levels)
  return(p)
}

# Generate plots
col_plot_11CT_BEFORE <- make_col_plot(plot_info$before$file, plot_info$before$added_CT, show_legend = TRUE)
col_plot_11CT_DURING <- make_col_plot(plot_info$during$file, plot_info$during$added_CT)
col_plot_11CT_AFTER <- make_col_plot(plot_info$after$file, plot_info$after$added_CT)

# Arrange plots
arrange <- ggarrange(
  col_plot_11CT_BEFORE + theme(legend.key.width = unit(2, 'cm'), axis.text.x = element_blank()),
  col_plot_11CT_DURING + theme(legend.key.width = unit(2, 'cm'), axis.text.x = element_blank()),
  col_plot_11CT_AFTER + theme(legend.key.width = unit(2, 'cm')),
  nrow = 3, ncol = 1, heights = c(1, 1.0, 1.09), 
  labels = c("a", "b", "c"),
  font.label = list(size = 25, color = "black")
) + guides(colour = guide_legend(override.aes = list(size = 5)))
arrange <- annotate_figure(arrange, left = text_grob("Frequency [%]", color = "black", rot = 90, size = 25))
# ggsave(arrange, file = "./figures/col_plot.png", width = 30, height = 35, units = "cm", bg = 'white')