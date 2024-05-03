## Imports ----
library(tidyverse)
library(here)
library(ggsci)
source(here::here("codes", "utils.R"))

## Data ----
# Prevalence of parental loss from all causes
prev_df <- readRDS(here::here("outputs",
                        "data_public",
                        "df_prevalence_parental_loss.rds"))

df.nsch <- readRDS(here::here("outputs",
                        "data_public",
                        "df_nsch.rds"))


# Model estimates vs NSCH survey
# of the prevalence of parental loss

prev_df <- prev_df |>
    dplyr::mutate(# Express in %
        dplyr::across(median:upper95, ~ .x * 100),
        source = "Matrix Projection Model") |>
    dplyr::bind_rows(df.nsch |>
                  dplyr::mutate(
                      source = "National Survey of Children's Health",
                      race = dplyr::case_when(
                          race == "Total" ~ "total",
                          race == "Hispanic" ~ "hispanic",
                          race == "NH White" ~ "white",
                          race == "NH Black" ~ "black",
                          TRUE ~ race
                      )
                  )) |>
    categorize_race()

p1 <- ggplot2::ggplot(
    prev_df,
    ggplot2::aes(
        x = year,
        y = median,
        ymin = lower95,
        ymax = upper95,
        group = source,
        fill = source,
        color = source
    )
) +
    ggplot2::geom_point(size = 1.75) +
    ggplot2::geom_line(linewidth = 1, alpha = .9) +
    ggplot2::geom_ribbon(color = NA, alpha = .25) +
    ggsci::scale_fill_jama(name = "Source of estimates") +
    ggsci::scale_color_jama(name = "Source of estimates") +
    ggplot2::scale_y_continuous("Youth <18 who experienced the\ndeath of at least one parent, %",
                       limits = c(0, NA),
                       expand = c(0, 0)) + 
    ggplot2::scale_x_continuous(
        NULL, 
        breaks = seq(2016, 2020, 2),
        limits = c(2016, 2020),
        expand = c(0, .01)
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "bottom",
          axis.text.x = ggplot2::element_text(hjust = c(0, .5, 1))) +
    ggplot2::facet_wrap(~ race_cat)

## Save ----
ggplot2::ggsave(
    here::here("outputs",
         "figures",
         "figS5_validation.jpg"),
    p1,
    width = 5,
    height = 3.5,
    scale = 1.5,
    dpi = 600,
    units = "in"
)
ggplot2::ggsave(
    here::here("outputs",
         "figures",
         "figS5_validation.pdf"),
    p1,
    width = 5,
    height = 3.5,
    scale = 1.5,
    device = grDevices::cairo_pdf,
    units = "in"
)

readr::write_csv(
    prev_df |>
        dplyr::ungroup() |>
        dplyr::select(source, race_cat, year, median, lower95, upper95),
    here::here("outputs", "figures_data", "figS6_data_validation.csv")
)
