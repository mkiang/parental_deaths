## Imports ----
library(tidyverse)
library(here)
library(patchwork)
library(ggsci)
source(here::here("codes", "utils.R"))

## Data ----
lt.US <- readRDS(here::here("outputs",
                      "data_public",
                      "lt_US.rds"))

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
    categorize_race() |>
    dplyr::mutate(
        metric_cat = factor(
            metric,
            levels = c("number", "percent"),
            labels = c(
                "Number on youth impacted\nby parental death, thousands",
                "Youth impacted by parental\ndeath per 1,000 population"
            ),
            ordered = TRUE
        ),
        cause_cat = factor(
            cause,
            levels = c("drugs", "firearms", "other"),
            labels = c("Drug Poisonings", "Firearms", "All other causes"),
            ordered = TRUE
        )
    )

popu18 <- lt.US |>
    dplyr::filter(year %in% seq(2000, 2020, 5),
           race_eth %in% c("black", "white", "hispanic", "total"),
           age < 18) |>
    dplyr::summarise(.by = c(race_eth, year),
              pop = sum(pop)) |>
    dplyr::mutate(race = factor(race_eth,
                         levels = c("white", "black", "hispanic", "total"))) |>
    dplyr::select(-race_eth) |>
    categorize_race() |> 
    dplyr::mutate(pop_text = prettyNum(round(pop / 1000), big.mark = ","))

## Plot number of children losing parents
p1 <- ggplot2::ggplot(
    parental_deaths |>
        dplyr::filter(cause == "drugs",
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
                       limits = c(0, 80000),
                       labels = function(x) round(x / 1000)) + 
    ggplot2::theme(legend.position = c(0.01, 0.99),
          legend.justification = c(0, 1))

p2 <- ggplot2::ggplot(
    parental_deaths |>
        dplyr::filter(cause == "drugs",
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
                       limits = c(0, .0013),
                       labels = function(x) round(x * 1000, 1)) + 
    ggplot2::theme(legend.position = "none")

p3 <- ggplot2::ggplot(popu18,
       ggplot2::aes(x = year,
           y = race_cat_rev,
           label = pop_text,
           color = race_cat)) +
    ggplot2::geom_text(size = 3.5,
              hjust = rep(c(.5, .5, .5, .5, .75), 4), 
              vjust = .5) + 
    ggsci::scale_color_jama(name = "Race and ethnicity") +
    ggplot2::scale_x_continuous(NULL,
                       expand = c(0, .5),
                       limits = c(1999, 2020)) + 
    ggplot2::scale_y_discrete("Number of youth <18\nyears, thousands") + 
    ggplot2::theme_bw() + 
    ggplot2::theme(legend.position = "none",
          axis.ticks.length.y = ggplot2::unit(0, "cm"),
          panel.grid = ggplot2::element_blank(),
          panel.border = ggplot2::element_blank())

p1_full <- p1 + p2 + p3 + 
    patchwork::plot_layout(ncol = 1, heights = c(1, 1, .25))

## Save ----
ggplot2::ggsave(
    here::here("outputs",
         "figures",
         "fig1_drugs.jpg"),
    p1_full, 
    width = 8,
    height = 8,
    scale = 1, 
    dpi = 600,
    units = "in"
)
ggplot2::ggsave(
    here::here("outputs",
         "figures",
         "fig1_drugs.pdf"),
    p1_full,
    width = 8,
    height = 8,
    scale = 1,
    device = grDevices::cairo_pdf,
    units = "in"
)

readr::write_csv(
    parental_deaths |>
        dplyr::filter(cause == "drugs",
               metric %in% c("percent", "number")) |>
        dplyr::left_join(popu18) |>
        dplyr::select(
            cause_cat,
            race_cat,
            metric_cat,
            year,
            median,
            lower95,
            upper95,
            pop
        ),
    here::here("outputs", "figures_data", "fig1_data_drugs.csv")
)
