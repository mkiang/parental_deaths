## Imports ----
library(tidyverse)
library(here)
library(ggsci)
library(patchwork)
source(here::here("codes", "utils.R"))

## Data ----
parental_deaths <- dplyr::bind_rows(
    readRDS(here::here(
        "outputs",
        "data_public",
        "df_incidence_parental_loss_by_cause.rds"
    )) |>
        dplyr::ungroup() |>
        dplyr::mutate(metric = "percent"),
    readRDS(here::here(
        "outputs",
        "data_public",
        "df_n_loss_by_cause.rds"
    )) |>
        dplyr::ungroup() |>
        dplyr::mutate(metric = "number")
)

## Clean data ----
parental_deaths <- parental_deaths |>
    dplyr::mutate(cause_char = dplyr::case_when(
        cause == "firearms" ~ "both",
        cause == "drugs" ~ "both",
        TRUE ~ as.character(cause)
    )) |> 
    dplyr::group_by(year, race, cause_char, metric) |> 
    dplyr::summarise(dplyr::across(median:upper95, ~ sum(.x))) |>
    dplyr::ungroup() |>
    dplyr::rename(cause = cause_char) |>
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
            levels = c("drugs", "firearms", "both", "other"),
            labels = c("Drug Poisonings", "Firearms", "Drugs and Firearms", "All other causes"),
            ordered = TRUE
        )
    ) 

## Plot number of children losing parents
p1 <- ggplot2::ggplot(
    parental_deaths |>
        dplyr::filter(cause == "both",
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
    ggplot2::scale_y_continuous("Number of youth <18 years\nimpacted (95% CI), thousands",
                       expand = c(0, 0),
                       limits = c(0, NA),
                       labels = function(x) round(x / 1000)) + 
    ggplot2::theme(legend.position = "right")

p2 <- ggplot2::ggplot(
    parental_deaths |>
        dplyr::filter(cause == "both",
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
    ggplot2::scale_y_continuous("Youth impacted per\n1,000 population (95% CI)",
                       expand = c(0, 0),
                       limits = c(0, NA),
                       labels = function(x) round(x * 1000, 1)) + 
    ggplot2::theme(legend.position = "none")

p1_full <- p1 + p2 + patchwork::plot_layout(ncol = 1)

## Save ----
ggplot2::ggsave(
    here::here("outputs",
         "figures",
         "figS2_both.jpg"),
    p1_full, 
    width = 8,
    height = 6,
    scale = 1, 
    dpi = 600,
    units = "in"
)
ggplot2::ggsave(
    here::here("outputs",
         "figures",
         "figS2_both.pdf"),
    p1_full,
    width = 8,
    height = 6,
    scale = 1,
    device = grDevices::cairo_pdf,
    units = "in"
)

readr::write_csv(
    parental_deaths |>
        dplyr::filter(cause == "both",
               metric %in% c("percent", "number")) |>
        dplyr::select(
            cause_cat,
            race_cat,
            metric_cat,
            year,
            median,
            lower95,
            upper95
        ),
    here::here("outputs", "figures_data", "figS2_data_both.csv")
)
