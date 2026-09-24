# create_figures.R

library(tidyverse)
library(patchwork)

# Creating a theme function used for visualizations
theme_clean <- function() {
  theme_minimal(base_family = "Arial") +
    theme(panel.grid.minor = element_blank(),
          plot.title = element_text(face = "bold"),
          axis.title = element_text(face = "bold"),
          strip.text = element_text(face = "bold", size = rel(1), hjust = 0),
          strip.background = element_rect(fill = "grey80", color = NA),
          legend.title = element_text(face = "bold"))
}

my_pal <- c("#D22030", "#132E5B")

create_RQ1_figure <- function(
    stop_model = here::here("models","RQ1_model_stops.RDS"), 
    vowel_model = here::here("models","RQ1_model_vowels.RDS"),
    output_file = "figures/RQ1_pretest_x_segmental_variability.png"
) {
  RQ1_stop_model <- readRDS(stop_model) 
  RQ1_vowel_model <- readRDS(vowel_model) 
  stop_observations <- model.frame(RQ1_stop_model)
  vowel_observations <- model.frame(RQ1_vowel_model)
  vowel_scaled <- vowel_observations[["scale(vowel_MCV)"]]
  vowel_observations$vowel_MCV <- as.numeric(
    vowel_scaled * attr(vowel_scaled, "scaled:scale") +
      attr(vowel_scaled, "scaled:center")
  )
  
  # Stop plot
  RQ1_stop_plot <- sjPlot::plot_model(RQ1_stop_model, 
                                      type = "pred",
                                      colors = my_pal,
                                      terms = "vot_CoV") +
    geom_point(data = stop_observations,
               aes(x = vot_CoV, y = pretest),
               inherit.aes = FALSE
               # color = "grey35",
               # alpha = .65,
               # size = 1.5
               ) +
    coord_cartesian(ylim = c(55,95)) +
    labs(x = "VOT CV (%)",
         y = "Pretest Intelligibility (%)",
         title = "Stop variability") +
    theme_clean() +
    theme(panel.border = element_rect(fill = NA),
          aspect.ratio = 1)
  
  # Vowel plot
  RQ1_vowel_plot <- sjPlot::plot_model(RQ1_vowel_model, 
                                       type = "pred",
                                       colors = my_pal,
                                       terms = "vowel_MCV") +
    geom_point(data = vowel_observations,
               aes(x = vowel_MCV, y = pretest),
               inherit.aes = FALSE
               # color = "grey35",
               # alpha = .65, size = 1.5
               ) +
    coord_cartesian(ylim = c(55,95))  +
    labs(x = "F1/F2 MCV",
         y = "Pretest Intelligibility (%)",
         title = "Vowel variability") +
    theme_clean() +
    theme(panel.border = element_rect(fill = NA),
          aspect.ratio = 1)

  RQ1_plot <- RQ1_stop_plot + RQ1_vowel_plot +
    patchwork::plot_layout(ncol = 2, guides = "collect")
  RQ1_plot
  
  ggsave(plot = RQ1_plot,
         filename = output_file,
         height = 4,
         width = 7,
         units = "in",
         scale = .9,
         bg = "white")

}

create_RQ2_figure <- function(
    stop_model = here::here("models","RQ2_parsimonious_model_stops.RDS"),
    vowel_model = here::here("models","RQ2_parsimonious_model_vowels.RDS"),
    output_file = "figures/RQ2_learning_pretest_x_segmental_variability.png") {
  
  RQ2_stop_model <- readRDS(stop_model) 
  RQ2_vowel_model <- readRDS(vowel_model) 
  stop_plot_data <- model.frame(RQ2_stop_model)
  stop_plot_data$pretest <- stop_plot_data$pretest_c +
    attr(RQ2_stop_model, "pretest_mean")
  stop_plot_data$vot_CoV <- stop_plot_data$vot_CoV_c +
    attr(RQ2_stop_model, "vot_CoV_mean")
  RQ2_stop_plot_model <- lm(
    learning ~ vot_CoV * pretest,
    data = stop_plot_data
  )
  vowel_plot_data <- model.frame(RQ2_vowel_model)
  vowel_plot_data$pretest <- vowel_plot_data$pretest_c +
    attr(RQ2_vowel_model, "pretest_mean")
  vowel_plot_data$vowel_MCV <-
    vowel_plot_data$vowel_MCV_z * attr(RQ2_vowel_model, "vowel_MCV_sd") +
    attr(RQ2_vowel_model, "vowel_MCV_mean")
  # Re-express the saved scaled model on the raw MCV scale for plotting. This
  # is an algebraic reparameterization and preserves its fitted values.
  RQ2_vowel_plot_model <- lm(
    learning ~ vowel_MCV * pretest,
    data = vowel_plot_data
  )
  
  RQ2_stop_plot <- sjPlot::plot_model(RQ2_stop_plot_model,
                                      type = "int",
                                      terms = c("vot_CoV", "pretest"),
                                      colors = my_pal) +
    coord_cartesian(ylim = c(-20,50)) +
    labs(x = "VOT CV (%)",
         y = "Learning (%)",
         color = "Pretest",
         title = "Stop variability") +
    theme_clean() +
    theme(panel.border = element_rect(fill = NA),
          aspect.ratio = 1)
  
  RQ2_vowel_plot <- sjPlot::plot_model(RQ2_vowel_plot_model,
                                       type = "int",
                                       terms = c("vowel_MCV", "pretest"),
                                       colors = my_pal) +
    coord_cartesian(ylim = c(-20,50)) +
    labs(x = "F1/F2 MCV",
         y = "Learning (%)",
         color = "Pretest",
         title = "Vowel variability") +
    theme_clean() +
    theme(panel.border = element_rect(fill = NA),
          aspect.ratio = 1)

  
  RQ2_plot <- RQ2_stop_plot + RQ2_vowel_plot +
    patchwork::plot_layout(ncol = 2, guides = "collect")
  RQ2_plot
  
  ggsave(plot = RQ2_plot,
         filename = output_file,
         height = 4,
         width = 7,
         units = "in",
         scale = .9,
         bg = "white")
}
