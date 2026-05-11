# exploration.R
# Quick exploration of the project question:
#   Does municipal income correlate with the share of buildings
#   that are heated with a renewable source?
# Everything in one file, end to end.

library(here)
library(dplyr)
library(readr)
library(readxl)
library(ggplot2)


## 1. Read the GWR building data --------------------------------------------
# The file is tab-separated even though it ends in .csv. We only read the
# columns we actually use (the full file has ~50 columns and ~3.3 M rows).

buildings <- read_tsv(
  here("gwr", "gebaeude_batiment_edificio.csv"),
  col_select = c(GGDENR, GGDENAME, GDEKT,
                 GKAT, GKLAS, GSTAT, GBAUJ,
                 GWAERZH1, GENH1, GWAERSCEH1, GWAERDATH1),
  na = c("", "NA"),
  show_col_types = FALSE
)


## 2. Keep only existing residential buildings -----------------------------
# GKAT: 1020 = exclusively residential, 1030 = residential with side use
# GSTAT 1004 = the building actually exists today.

residential <- buildings %>%
  filter(GKAT %in% c(1020, 1030), GSTAT == 1004)


## 3. Flag each building --------------------------------------------------
# Renewable heating per the project brief:
#   heat pump        GWAERZH1 in 7410, 7411
#   solar thermal    GWAERZH1 in 7420, 7421
#   wood / biomass   GENH1    in 7540..7543
# Low-quality entry = sourced from Volkszählung 2000 (GWAERSCEH1 = 860) OR
# no update date recorded at all.

residential <- residential %>%
  mutate(
    is_renewable = GWAERZH1 %in% c(7410, 7411, 7420, 7421) |
                   GENH1    %in% c(7540, 7541, 7542, 7543),
    is_lowqual   = GWAERSCEH1 %in% 860 | is.na(GWAERDATH1),
    is_efh       = GKLAS == 1110,        # single-family, strict
    is_pre1980   = !is.na(GBAUJ) & GBAUJ < 1980
  )


## 4. Quick data-quality check --------------------------------------------
# When was the heating entry last updated? The brief notes this is a problem,
# many entries still go back to the 2000 census.

residential %>%
  mutate(year = as.integer(substr(GWAERDATH1, 1, 4))) %>%
  filter(!is.na(year), year >= 1990) %>%
  ggplot(aes(year)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Latest update of the GWR heating entry",
       x = NULL, y = "Number of buildings") +
  theme_minimal()


## 5. Aggregate to the municipality ---------------------------------------
# Renewable share is defined over buildings with a heating entry, so the
# denominator is sum(!is.na(GWAERZH1)), not n().

muni <- residential %>%
  group_by(bfs_id = GGDENR, municipality = GGDENAME, canton = GDEKT) %>%
  summarise(
    n_buildings     = n(),
    n_with_heating  = sum(!is.na(GWAERZH1)),
    renewable_share = sum(is_renewable)   / sum(!is.na(GWAERZH1)),
    lowqual_share   = sum(is_lowqual)     / sum(!is.na(GWAERZH1)),
    efh_share       = sum(is_efh)         / n(),
    pre1980_share   = sum(is_pre1980)     / sum(!is.na(GBAUJ)),
    .groups = "drop"
  )


## 6. Read the income data ------------------------------------------------
# The ESTV file has many sheets. We need just two:
#   511 = number of taxpayers per income bracket
#   512 = sum of taxable income (Steuerbares Einkommen) per bracket
# Each sheet has a "Total / Totale" column at the end which we use directly,
# the row total includes privacy-suppressed buckets that summing would miss.

estv_file <- here("estv", "statistik-dbst-np-gden-2022-normalfall.xlsx")

read_total <- function(sheet, value_name) {
  raw <- read_excel(
    estv_file, sheet = sheet, skip = 2,
    col_types = c("text", "text", "numeric", "text", rep("numeric", 11))
  )
  names(raw)[1:4]       <- c("canton_id", "canton_name", "bfs_id", "muni_name")
  names(raw)[ncol(raw)] <- "total"
  raw %>%
    filter(!is.na(bfs_id)) %>%
    select(bfs_id, !!value_name := total)
}

counts <- read_total("511", "n_taxpayers")
sums   <- read_total("512", "sum_taxable")

income <- counts %>%
  left_join(sums, by = "bfs_id") %>%
  mutate(mean_taxable = sum_taxable / n_taxpayers)


## 7. Join the two datasets -----------------------------------------------

data <- muni %>%
  left_join(select(income, bfs_id, mean_taxable), by = "bfs_id")


## 8. First look at the relationship --------------------------------------
# Drop very small municipalities so the noise floor does not dominate.

data %>%
  filter(n_with_heating >= 50, !is.na(mean_taxable)) %>%
  ggplot(aes(mean_taxable, renewable_share)) +
  geom_point(aes(size = n_with_heating), alpha = 0.4, colour = "steelblue") +
  geom_smooth(method = "loess", se = FALSE, colour = "firebrick") +
  scale_x_continuous(labels = scales::label_number(big.mark = "'")) +
  scale_y_continuous(labels = scales::label_percent()) +
  labs(title = "Renewable heating share vs mean taxable income",
       subtitle = "One dot = one Swiss municipality",
       x = "Mean taxable income per taxpayer (CHF)",
       y = "Share of buildings with renewable heating",
       size = "Buildings") +
  theme_minimal()


## 9. Load the municipality polygons --------------------------------------
# This file is produced once by R/05_first_visuals.R, which downloads
# swissBOUNDARIES3D from swisstopo and caches the municipality layer here.
# st_zm() drops the 3D Z coordinate that ggplot's geom_sf does not like.

library(sf)

muni_polygons <- st_read(
  here("data-derived", "municipalities.gpkg"),
  quiet = TRUE
) %>%
  st_zm()

map_data <- muni_polygons %>%
  left_join(data, by = "bfs_id")


## 10. Choropleth maps ----------------------------------------------------

ggplot(map_data) +
  geom_sf(aes(fill = renewable_share), colour = NA) +
  scale_fill_viridis_c(labels = scales::label_percent(),
                       na.value = "grey90") +
  labs(title = "Renewable heating share by municipality",
       fill = NULL) +
  theme_void()

ggplot(map_data) +
  geom_sf(aes(fill = mean_taxable), colour = NA) +
  scale_fill_viridis_c(option = "magma",
                       labels = scales::label_number(big.mark = "'"),
                       na.value = "grey90") +
  labs(title = "Mean taxable income per taxpayer (2022)",
       fill = "CHF") +
  theme_void()
