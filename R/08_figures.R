axis_text_size <- 8
title_text_size <- 10
axis_title_size <- 11
subtitle_text_size <- 10
legend_text_size <- 10
strip_text_size <- 10

# ------------------------------------------------------------------------------
# Figure 1
# ------------------------------------------------------------------------------

df <- data.frame(
  region = as.character(c("caudal anterior cingulate",
                          "rostral anterior cingulate",
                          "lateral orbitofrontal",
                          "medial orbitofrontal",
                          "rostral middle frontal")),
  roi = as.factor(c(rep("ACC", 2),
                    rep("OFC", 2),
                    "DLPFC")),
  labels = as.factor(c("Caudal ACC",
                       "Rostral ACC",
                       "Lateral OFC",
                       "Medial OFC",
                       "DLPFC")))

roi_plot <- 
  ggseg(.data = df,
        position = "stacked",
        colour="black",
        size=.7,
        mapping = aes(fill = labels)) +
  scale_fill_manual(
    breaks = c(
      # Cingulate
      "Rostral ACC",
      "Caudal ACC",
      # OFC
      "Lateral OFC",
      "Medial OFC",
      # DLPFC
      "DLPFC"),
    values = c(
      # Cingulate
      "#a1d99b",
      "#31a354",
      # OFC
      "#9ecae1",
      "#3182bd",
      # DLPFC
      "#fc9272"),
    na.value = "#f2f0e4") +
  theme(
    # titles
    plot.title = element_text(size = title_text_size, color = "black", hjust = 0.5, family = "sans"),
    plot.subtitle = element_text(size = subtitle_text_size, color = "black", hjust = 0.5, family = "sans"),
    
    # legends
    legend.position = "right",
    legend.title = element_blank(),
    legend.text = element_text(size = legend_text_size, color = "black", hjust = 0.5, family = "sans"),
    legend.text.align = 0,
    
    # axes
    axis.text = element_text(size = axis_text_size, hjust = 0.5, color = "black", family = "sans"),
    axis.title = element_blank(),
    
    # strips
    strip.background = element_blank(),
    strip.text.x = element_text(size = strip_text_size, color = "black", family = "sans"))

ggsave(here("manuscript/figures/figure_1.pdf"),
       plot = roi_plot,
       device = "pdf",
       width = 220,
       height = 50,
       units = "mm",
       dpi = 300)

# ------------------------------------------------------------------------------
# Figure 2
# ------------------------------------------------------------------------------

top_row <- 
  coef_plot(regression_list[[1]], "left", "General Disinhibition", TRUE) +
  coef_plot(regression_list[[2]], "left", "Callous-Aggression", FALSE) + 
  coef_plot(regression_list[[3]], "left", "Substance Abuse", FALSE) +
  coef_plot(regression_list[[4]], "left", "NoGo accuracy", FALSE)

bottom_row <- 
  coef_plot(regression_list[[5]], "right", "General Disinhibition", TRUE) + 
  coef_plot(regression_list[[6]], "right", "Callous-Aggression", FALSE) +
  coef_plot(regression_list[[7]], "right", "Substance Abuse", FALSE) +
  coef_plot(regression_list[[8]], "right", "NoGo accuracy", FALSE)

top_row <- 
  top_row + plot_annotation(title = "Left hemisphere",
                            theme = theme(plot.title = element_text(size = 12))) +
  plot_layout(ncol = 4)

bottom_row <- 
  bottom_row + plot_annotation(title = "Right hemisphere",
                               theme = theme(plot.title = element_text(size = 12))) +
  plot_layout(ncol = 4)

ggsave(here("manuscript/figures/figure_2a.pdf"),
       plot = top_row,
       device = "pdf",
       width = 220,
       height = 50,
       units = "mm",
       dpi = 300)

ggsave(here("manuscript/figures/figure_2b.pdf"),
       plot = bottom_row,
       device = "pdf",
       width = 220,
       height = 50,
       units = "mm",
       dpi = 300)