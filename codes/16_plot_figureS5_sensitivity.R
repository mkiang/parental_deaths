## Imports ----
library(tidyverse)
library(here)
library(ggsci)
library(patchwork)
source(here::here("codes", "utils.R"))

## Data ----
sens_combined <- readRDS(here::here(
    "outputs",
    "data_public",
    "df_cum_orphans_total_at_least_combined.rds"
))
sens_separate <- readRDS(here::here(
    "outputs", "data_public", "df_cum_orphans_at_least.rds"
)) |> 
    categorize_race() |> 
    dplyr::mutate(cause_cat = factor(cause,
                              levels = c("drugs", "firearms"),
                              labels = c("Drug Poisonings", "Firearm-related"),
                              ordered = TRUE))

## Plots ----
p_tile <- ggplot2::ggplot(sens_combined,
       ggplot2::aes(x = drug,
           y = firearm,
           fill = median)) +
    ggplot2::geom_tile(color = "white") +
    ggplot2::scale_fill_viridis_b(
        "Youth <18y experiencing\nparental death (in thousands)",
        labels = function(x)
            x / 1000,
        guide = ggplot2::guide_colorsteps(barwidth = ggplot2::unit(7, "cm"),
                                 barheight = ggplot2::unit(.25, "cm"))
    ) +
    ggplot2::scale_x_continuous(
        "Fertility rate of drug overdose deaths compared to non-drug-overdose deaths",
        breaks = c(.8, .9, 1, 1.1, 1.2),
        labels = c("-20%", "-10%", "+0%", "+10%", "+20%"),
        expand = c(0, 0)
    ) +
    ggplot2::scale_y_continuous(
        "Fertility rate of firearm deaths compared to non-firearm deaths",
        breaks = c(.8, .9, 1, 1.1, 1.2),
        labels = c("-20%", "-10%", "+0%", "+10%", "+20%"),
        expand = c(0, 0)
    ) +
    ggplot2::coord_equal() +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "bottom")

p_points <- ggplot2::ggplot(sens_separate,
       ggplot2::aes(
           x = scenario.fx,
           y = median,
           ymin = lower95,
           ymax = upper95,
           color = scenario.fx == 1
       )) +
    ggplot2::geom_errorbar(width = 0) +
    ggplot2::geom_point(alpha = .75) +
    ggplot2::facet_grid(race_cat ~ cause_cat, scales = "free") +
    ggplot2::scale_x_continuous(
        "Change in fertility rate",
        breaks = c(.8, .9, 1, 1.1, 1.2),
        labels = c("-20%", "-10%", "+0%", "+10%", "+20%")
    ) + 
    ggplot2::scale_y_continuous("Youth <18y experiencing parental death (in thousands)",
                       labels = function(x) x / 1000) +
    ggsci::scale_color_jama(name = NULL, labels = c("Sensitivity analysis", "Primary analysis")) + 
    ggplot2::theme_bw() + 
    ggplot2::theme(legend.position = "bottom")

p1 <- p_points + p_tile + patchwork::plot_layout(ncol = 2, widths = c(2, 2))

## Save ----
ggplot2::ggsave(
    here::here("outputs",
         "figures",
         "figS5_sensitivity.jpg"),
    p1,
    width = 8,
    height = 5,
    scale = 1.5,
    dpi = 600,
    units = "in"
)
ggplot2::ggsave(
    here::here("outputs",
         "figures",
         "figS5_sensitivity.pdf"),
    p1,
    width = 8,
    height = 5,
    scale = 1.5,
    device = grDevices::cairo_pdf,
    units = "in"
)

readr::write_csv(
    sens_separate |>
        dplyr::ungroup() |>
        dplyr::select(cause_cat,
               race_cat, 
               scenario = scenario.fx, 
               median, 
               lower95, 
               upper95),
    here::here("outputs", "figures_data", "figS5a_data_sensitivity.csv")
)
readr::write_csv(
    sens_combined |>
        dplyr::ungroup() |>
        dplyr::select(drug_fertility = drug,
               firearm_fertility = firearm, 
               median, 
               lower95, 
               upper95),
    here::here("outputs", "figures_data", "figS5b_data_sensitivity.csv")
)
