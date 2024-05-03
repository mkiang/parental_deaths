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
                "Percent of youth impacted\nby parental death"
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
            labels = c("Father", "Mother"),
            ordered = TRUE
        ),
        sex_cat_rev = factor(
            sex,
            levels = rev(c("father", "mother")),
            labels = rev(c("Father", "Mother")),
            ordered = TRUE
        )
    )

## Plot number of children losing parents by parent
p1 <- ggplot2::ggplot(
    parental_deaths_bysex |>
        dplyr::filter(cause %in% c("drugs", "firearms"),
               metric == "number"),
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
    ggplot2::scale_y_continuous(
        "Number of youth <18 years impacted (95% CI), thousands",
        expand = c(0, 0),
        limits = c(0, 55000),
        labels = function(x)
            round(x / 1000)
    ) +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::facet_grid(cause_cat ~ sex_cat)

## Save ----
ggplot2::ggsave(
    here::here("outputs",
         "figures",
         "figS4_byparent.jpg"),
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
         "figS4_byparent.pdf"),
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
               metric == "number") |>
        dplyr::select(cause_cat,
               race_cat,
               metric_cat,
               year,
               median,
               lower95,
               upper95),
    here::here("outputs", "figures_data", "figS4_data_byparent.csv")
)
