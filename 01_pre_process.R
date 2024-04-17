start_year <- lubridate::year(lubridate::today())-1 #get rid of -1 once we have this years data
start_of_current_month <- lubridate::ym(tsibble::yearmonth(lubridate::today()))
#' NOTE: the files that are being compared need to be quite similar:
#' they need to have identical file names (between versions)
#' they need to have the same sheet names (between versions)
#' they need to have the same series names aka row identifiers (between versions)
#' the only thing that should differ is the numeric data.
#' there is a fuzzyjoin around line 130... this should be checked manually.

#libraries-----------------------
library(tidyverse)
library(here)
library(readxl)
library(janitor)
library(readxl)
library(conflicted)
conflicts_prefer(dplyr::filter)
conflicts_prefer(dplyr::lag)
#functions--------------------
get_cagrs <- function(tbbl, column){
  val_now <- tbbl[[column]][tbbl$year==start_year]
  val_fyfn <- tbbl[[column]][tbbl$year==start_year+5]
  val_tyfn <- tbbl[[column]][tbbl$year==start_year+10]
  cagr_ffy <- ((val_fyfn/val_now)^.2-1)
  cagr_sfy <- ((val_tyfn/val_fyfn)^.2-1)
  cagr_ty <- ((val_tyfn/val_now)^.1-1)
  tibble(cagr_ffy=cagr_ffy, cagr_sfy=cagr_sfy,cagr_ty=cagr_ty)
}
fix_noc <- function(tbbl){
  tbbl|>
    mutate(NOC=str_replace_all(NOC, "o","#"))
}

# the program---------------------------

joined <- tibble(which_file=list.files(here("data","occupation_new")))|>
  mutate(new_data=map(here("data","occupation_new", which_file), read_excel, skip=3, na="NA", col_types=c(rep("text",5), rep("numeric",13))),
         old_data=map(here("data","occupation_old", which_file), read_excel, skip=3, na="NA", col_types=c(rep("text",5), rep("numeric",13))),
         new_data=map(new_data, pivot_longer, cols=starts_with("2"), names_to="year", values_to="new_value"),
         old_data=map(old_data, pivot_longer, cols=starts_with("2"), names_to="year", values_to="original_value"),
         old_data=map(old_data, fix_noc),#for some reason the NOCs were f'd up ????
         new_data=map(new_data, fix_noc)#for some reason the NOCs were f'd up ????
         )|>
  mutate(joined=map2(new_data, old_data, inner_join))|>
  select(-new_data, -old_data)

write_rds(joined, here("out","joined.rds"))

############ internal_vs_stokes------------------------

internal <- read_excel(here("data",
                            "LMO 2024 edition LMIO industry employment forecast FINAL.xlsx"),
                       skip=2)|>
  select(-contains("CAGR"),-Note)|>
  pivot_longer(cols=-industry, names_to="year", values_to = "internal")|>
  separate(industry, into=c("lmo_ind_code", "industry"), sep=": ")|>
  select(-lmo_ind_code)

internal_total <- internal|>
  group_by(year)|>
  summarize(internal=sum(internal))|>
  mutate(industry="All industries")

internal <- full_join(internal, internal_total)|>
  group_by(industry)|>
  nest()|>
  rename(internal=data)

internal_vs_stokes <- read_excel(here("data","occupation_new", "employment_industry.xlsx"), skip = 3)|>
  filter(`Geographic Area`=="British Columbia")|>
  select(industry=Industry, starts_with("2"))|>
  pivot_longer(cols=-industry, names_to = "year", values_to = "stokes_cut")|>
  group_by(industry)|>
  nest()|>
  rename(stokes_cut=data)|>
  fuzzyjoin::stringdist_join(internal)|>
  rename(industry=industry.y)|>
  ungroup()|>
  select(-industry.x)|>
  mutate(data=map2(stokes_cut, internal, inner_join))|>
  select(-stokes_cut, -internal)

write_rds(internal_vs_stokes, here("out","internal_vs_stokes.rds"))

# CAGR stuff----------------------------------------

cagrs <- internal_vs_stokes|>
  mutate(internal=map(data, get_cagrs, "internal"),
         stokes=map(data, get_cagrs, "stokes_cut")
         )|>
  select(-data)|>
  unnest(internal, names_sep = "_")|>
  unnest(stokes, names_sep = "_")

write_rds(cagrs, here("out","cagrs.rds"))

# comparing to LFS data-----------------------------------

naics_to_lmo_mapping <- read_csv(here("data","tidy_2024_naics_to_lmo.csv"))

lfs_files <- list.files(here("data"), pattern = "lfsstat4digNAICS")

lfs_data <- vroom::vroom(here("data", lfs_files))|>
  na.omit()|>
  filter(LF_STAT=="Employed")|>
  inner_join(naics_to_lmo_mapping, by=c("NAICS_5"="naics"))|>
  group_by(lmo_ind_code, lmo_detailed_industry, SYEAR, SMTH)|>
  summarise(value=sum(`_COUNT_`, na.rm=TRUE))|>
  mutate(date=ym(paste(SYEAR, SMTH, sep="/")),
         series="LFS Data")|>
  ungroup()|>
  select(-SYEAR,-SMTH)|>
  filter(date<start_of_current_month)|>
    select(industry=lmo_detailed_industry, value, series, date)

lfs_data_totals <- lfs_data|>
  group_by(date, series)|>
  summarize(value=sum(value))|>
  mutate(industry="Total")

lfs_data <- full_join(lfs_data, lfs_data_totals)

write_csv(lfs_data, here("out","lfs_data.csv"))








