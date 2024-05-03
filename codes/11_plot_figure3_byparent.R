## Imports ----
library(tidyverse)
library(here)
library(patchwork)
library(ggsci)
source(here::here("codes", "utils.R"))

## Data ----
parental_deaths_bysex <- dplyr::bind_rows(
    readRDS(here::here(
        "outputs",
        "data_public",
        "df_incidence_parental_loss_by_cause_sex.rds"
    )) |>
        dplyr::ungroup() |>
        dplyr::mutate(metric = "percent"),
    readRDS(here::here(
        "outputs",
        "data_public",
        "df_n_loss_by_cause_sex.rds"
    )) |>
        dplyr::ungroup() |>
        dplyr::mutate(metric = "number")
)

## Clean data ----
parental_deaths_bysex <- parental_deaths_bysex |>
    categorize_race() |>
    dplyr::mutate(
        metric_cat = factor(
            metric,
            levels = c("number", "percent"),
            labels = c(
                "Number on youth impacted\nby parental death, thousands",
                 "Youth impacted per\n1,000 population (95% CI)"
            ),
            ordered = TRUE
        ),
        cause_cat = factor(
            cause,
            levels = c("drugs", "firearms", "other"),
            labels = c("Drug Poisonings", "Firearms", "All other causes"),
            ordered = TRUE
        )
    ) |>
    dplyr::mutate(
        sex_cat = factor(
            sex,
            levels = c("father", "mother"),
            labels = c("Father died", "Mother died"),
            ordered = TRUE
        ),
        sex_cat_rev = factor(
            sex,
            levels = rev(c("father", "mother")),
            labels = rev(c("Father died", "Mother died")),
            ordered = TRUE
        )
    )

## Percentage of kids by parent for drugs and guns ----
p1 <- ggplot2::ggplot(
    parental_deaths_bysex |>
        dplyr::filter(cause %in% c("firearms", "drugs"),
               metric == "percent"),
    ggplot2::aes(
        x = year,
        y = median,
        ymin = lower95,
        ymax = upper95,
        color = race_cat,
        fill = race_cat, 
        group = race_cat
    )
) +
    ggplot2::geom_ribbon(color = NA,
                alpha = .25) + 
    ggplot2::geom_point() +
    ggplot2::geom_line() + 
    ggplot2::theme_bw() + 
    ggsci::scale_color_jama(name = "Race and ethnicity") + 
    ggsci::scale_fill_jama(name = "Race and ethnicity") + 
    ggplot2::scale_x_continuous(NULL,
                       expand = c(0, .5)) + 
    ggplot2::scale_y_continuous( "Youth impacted per 1,000 population (95% CI)",
                       expand = c(0, 0),
                       limits = c(0, NA),
                       labels = function(x) round(x * 1000, 1)) + 
    ggplot2::theme(legend.position = "bottom") + 
    ggplot2::facet_grid(cause_cat ~ sex_cat)

## Save ----
ggplot2::ggsave(
    here::here("outputs",
         "figures",
         "fig3_byparent.jpg"),
    p1, 
    width = 8,
    height = 8,
    scale = 1, 
    dpi = 600,
    units = "in"
)
ggplot2::ggsave(
    here::here("outputs",
         "figures",
         "fig3_byparent.pdf"),
    p1,
    width = 8,
    height = 8,
    scale = 1,
    device = grDevices::cairo_pdf,
    units = "in"
)

readr::write_csv(
    parental_deaths_bysex |>
        dplyr::filter(cause %in% c("firearms", "drugs"),
               metric == "percent") |>
        dplyr::select(cause_cat,
               race_cat,
               metric_cat,
               year,
               median,
               lower95,
               upper95),
    here::here("outputs", "figures_data", "fig3_data_byparent.csv")
)