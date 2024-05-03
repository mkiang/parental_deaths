## Imports ----
library(tidyverse)
library(here)
library(ggsci)
source(here::here("codes", "utils.R"))

## Helper functions ----
read_nsduh <- function(f_path, age_x) {
    res <- readr::read_csv(f_path) |>
        janitor::clean_names()
    
    if (tibble::has_name(res, "illicit_drug_other_than_marijuana_ever_used")) {
        res <- res |>
            dplyr::filter(illicit_drug_other_than_marijuana_ever_used != "Overall") |>
            dplyr::rename(everuse = illicit_drug_other_than_marijuana_ever_used)
    } else {
        res <- res |>
            dplyr::filter(rc_illicit_drug_other_than_marijuana_ever_used != "Overall") |>
            dplyr::rename(everuse = rc_illicit_drug_other_than_marijuana_ever_used)
    }
    
    res <- res |>
        dplyr::filter(recoded_number_rs_children_18_in_household != "Overall") |>
        dplyr::rename(racegender = rc_combined_gender_by_race_indicator,
               n_kids = recoded_number_rs_children_18_in_household) |>
        dplyr::mutate(
            racegender_cat = factor(
                racegender,
                levels = c(
                    "Overall",
                    "1 - Male, White, Not Hisp",
                    "2 - Female, White, Not Hisp",
                    "3 - Male, Black, Not Hisp",
                    "4 - Female, Black, Not Hisp",
                    "5 - Male, Hispanic",
                    "6 - Female, Hispanic",
                    "7 - Male or Female, Other Races"
                ),
                labels = c(
                    "Overall",
                    "Male, Non-Hispanic White",
                    "Female, Non-Hispanic White",
                    "Male, Non-Hispanic Black",
                    "Female, Non-Hispanic Black",
                    "Male, Hispanic",
                    "Female, Hispanic",
                    "Male or Female, Other Races"
                ),
                ordered = TRUE
            ),
            n_kids_cat = factor(
                n_kids,
                levels = c("0", "1", "2", "3 - 3 or more"),
                labels = c("0", "1", "2", "3 or more"),
                ordered = TRUE
            ),
            everuse_cat = factor(
                everuse,
                levels = c(
                    "0 - Never used drug/used only marijuana",
                    "1 - Illicit drug except for marijuana are ever used"
                ),
                labels = c(
                    "Never used drug/\nused only marijuana",
                    "Illicit drug except for \nmarijuana are ever used"
                ),
                ordered = TRUE
            )
        ) |>
        dplyr::select(
            racegender,
            racegender_cat,
            everuse,
            everuse_cat,
            n_kids,
            n_kids_cat,
            dplyr::starts_with("column_"),
            unweighted_count,
            weighted_count
        ) |>
        dplyr::mutate(
            column_percent_ci_lower = as.numeric(column_percent_ci_lower),
            column_percent_ci_upper = as.numeric(column_percent_ci_upper)
        )
    
    res |>
        dplyr::mutate(age_group = age_x, .before = 1)
}

## Data ----
nsduh_2020 <- dplyr::bind_rows(
    read_nsduh(
        here::here(
            "inputs",
            "data_public",
            "nsduh",
            "2020_ages12to17_SEXRACE x ILLEMFLAG x NRCH17_2.csv"
        ),
        "12 to 17"
    ),
    read_nsduh(
        here::here(
            "inputs",
            "data_public",
            "nsduh",
            "2020_ages18to25_SEXRACE x ILLEMFLAG x NRCH17_2.csv"
        ),
        "18 to 25"
    ),
    read_nsduh(
        here::here(
            "inputs",
            "data_public",
            "nsduh",
            "2020_ages26to34SEXRACE x ILLEMFLAG x NRCH17_2.csv"
        ),
        "26 to 34"
    ),
    read_nsduh(
        here::here(
            "inputs",
            "data_public",
            "nsduh",
            "2020_ages35to49_SEXRACE x ILLEMFLAG x NRCH17_2.csv"
        ),
        "35 to 49"
    ),
    read_nsduh(
        here::here(
            "inputs",
            "data_public",
            "nsduh",
            "2020_ages12to49_SEXRACE x ILLEMFLAG x NRCH17_2.csv"
        ),
        "12 to 49"
    )
) |>
    dplyr::mutate(age_cat = factor(
        age_group,
        levels = c("12 to 49",
                   "12 to 17",
                   "18 to 25",
                   "26 to 34",
                   "35 to 49"),
        labels = c("All (12 to 49)",
                   "12 to 17",
                   "18 to 25",
                   "26 to 34",
                   "35 to 49"),
        ordered = TRUE
    ))

## Plot ----
p1 <- ggplot2::ggplot(
    nsduh_2020 |>
        dplyr::filter(
            racegender != "Overall",
            racegender != "7 - Male or Female, Other Races"
        ),
    ggplot2::aes(
        x = n_kids_cat,
        group = everuse_cat,
        color = everuse_cat,
        y = column_percent,
        ymin = column_percent_ci_lower,
        ymax = column_percent_ci_upper
    )
) +
    ggplot2::geom_errorbar(position = ggplot2::position_dodge(width = .5),
                  width = 0) +
    ggplot2::geom_point(position = ggplot2::position_dodge(width = .5),
               alpha = .9) +
    ggsci::scale_color_jama(name = "Respondent's Drug Use") +
    ggplot2::scale_x_discrete("Number of children in household under 18 years of age") +
    ggplot2::scale_y_continuous("Proportion of respondents using 2020 NSDUH data") +
    ggplot2::facet_grid(age_cat ~ racegender_cat) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "bottom",
          strip.background = ggplot2::element_rect(fill = "white"))

## Save ----
ggplot2::ggsave(
    here::here("outputs",
         "figures",
         "figS1_nsduh.jpg"),
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
         "figS1_nsduh.pdf"),
    p1,
    width = 8,
    height = 5,
    scale = 1.5,
    device = grDevices::cairo_pdf,
    units = "in"
)
readr::write_csv(
    nsduh_2020 |>
        dplyr::filter(racegender != "7 - Male or Female, Other Races") |>
        dplyr::select(
            racegender_cat,
            age_cat,
            everuse_cat,
            n_kids_cat,
            column_percent,
            column_percent_ci_lower,
            column_percent_ci_upper,
            unweighted_count,
            weighted_count
        ) |>
        dplyr::arrange(age_cat, racegender_cat),
    here::here("outputs", "figures_data", "figS1_data_nsduh.csv")
)