#' NOTE: the files that are being compared need to be quite similar:
#' they need to have identical file names (between versions)
#' they need to have the same sheet names (between versions)
#' they need to have the same series names aka row identifiers (between versions)
#' the only thing that should differ is the numeric data.
#' there is a fuzzyjoin around line 130... this should be checked manually.

#libraries-----------------------

library(vroom)
library(fpp3)
library(tidyverse)
library(here)
library(readxl)
library(janitor)
library(readxl)
library(conflicted)
conflicts_prefer(dplyr::filter)
conflicts_prefer(dplyr::lag)
#functions--------------------
get_rmse <- function(tbbl){
  sqrt(mean((tbbl$Stokes-tbbl$Rich)^2))
}

get_smape <- function(tbbl){
  mean(abs(tbbl$Stokes-tbbl$Rich)/(abs(tbbl$Stokes)+abs(tbbl$Rich)))
}

get_mean <- function(tbbl){
  (mean(tbbl$Stokes)+mean(tbbl$Rich))/2
}

fix_noc <- function(tbbl){
  tbbl|>
    mutate(NOC=str_replace_all(NOC, "o","#"))
}

# comparision between stokes cuts----------------------------

new_vs_old <- tibble(which_file=list.files(here("data","occupation_new")))|>
  mutate(new_data=map(here("data","occupation_new", which_file), read_excel, skip=3, na="NA", col_types=c(rep("text",5), rep("numeric",14))),
         old_data=map(here("data","occupation_old", which_file), read_excel, skip=3, na="NA", col_types=c(rep("text",5), rep("numeric",13))),
         new_data=map(new_data, pivot_longer, cols=starts_with("2"), names_to="year", values_to="new"),
         old_data=map(old_data, pivot_longer, cols=starts_with("2"), names_to="year", values_to="original"),
         old_data=map(old_data, fix_noc),#for some reason the NOCs were f'd up ????
         new_data=map(new_data, fix_noc)#for some reason the NOCs were f'd up ????
         )|>
  mutate(joined=map2(new_data, old_data, inner_join))|>
  select(-new_data, -old_data)

write_rds(new_vs_old, here("out","new_vs_old.rds"))

# comparison with an ensemble forecast---------------------

stokes_raw <- read_excel(here("data","occupation_new", "employment_occupation.xlsx"), skip=3)

stokes_top <- stokes_raw|>
  filter(NOC=="#T",
         `Geographic Area`=="British Columbia")|>
  pivot_longer(cols=starts_with("2"), names_to = "year", values_to = "stokes_top")|>
  select(year, stokes_top)|>
  mutate(year=as.numeric(year))|>
  filter(year>max(year)-10)

noc_names <- stokes_raw|>
  select(NOC, Description)|>
  distinct()|>
  mutate(noc_5=paste(NOC, Description, sep=": "))

stokes <- stokes_raw|>
  filter(`Geographic Area`=="British Columbia")|>
  pivot_longer(cols=starts_with("2"), names_to = "year", values_to = "stokes")|>
  clean_names()|>
  mutate(year=as.numeric(year))|>
  filter(noc!="#T",
         year>max(year)-10)|>
  group_by(year)|>
  mutate(noc_5=paste(noc, description, sep=": "))|>
  select(year, noc_5, Stokes=stokes)

lfs <- vroom(list.files(here("data", "LFS"), full.names = TRUE))|>
  clean_names()|>
  filter(lf_stat=="Employed",
         !is.na(syear),
         noc_5!="missi")|>
  group_by(noc_5)|>
  mutate(nobs=n())|>
  ungroup()|>
  filter(nobs==max(nobs),
         syear<max(syear))|>
  select(-nobs)|>
  mutate(noc_5=if_else(noc_5 %in% paste0("000",11:15), "00018", noc_5),
         noc=paste0("#",noc_5),
         series="LFS")|>
  group_by(syear, noc, series)|>
  summarize(value=sum(count)/12)|>
  left_join(noc_names, by=c("noc"="NOC"))|>
  ungroup()|>
  select(year=syear, noc_5, series, value)

lfs_props <- lfs|>
  select(-series)|>
  mutate(lfs_prop=value/sum(value))|>
  select(-value)

lfs_2021_props <- lfs_props|>
  filter(year %in% 2018:2024)|>
  group_by(noc_5)|>
  summarize(lfs_2021_prop=mean(lfs_prop))

lfs_props <- full_join(lfs_props, lfs_2021_props)

census_raw <- readxl::read_excel(here("data", "census.xlsx"))
colnames(census_raw)[1] <- "lmo_noc"

census <- census_raw|>
  separate(lmo_noc, into = c("lmo_noc", "description"), sep = " ", extra="merge")|>
  select(-description)|>
  mutate(lmo_noc=paste0("#",lmo_noc))|>
  inner_join(noc_names, by=c("lmo_noc"="NOC"))|>
  select(noc_5, value=`All industries`)|>
  distinct()|>
  mutate(year=2021,
         series="Census")|>
  mutate(census_prop=value/sum(value))

census|>
  select(year, noc_5, value, series, prop=census_prop)|>
  write_rds(here("out", "census.rds"))

lfs_props <- inner_join(lfs_props, census|>select(noc_5, census_prop))|>
  mutate(lfs_adjusted_prop=if_else(lfs_2021_prop>0, lfs_prop*census_prop/lfs_2021_prop, census_prop))|>
  group_by(year)|>
  mutate(lfs_final_prop=lfs_adjusted_prop/sum(lfs_adjusted_prop),
         series="LFS")|>
  select(noc_5, year, prop=lfs_final_prop, series)|>
  ungroup()|>
  as_tsibble(key=noc_5, index=year)

models <- lfs_props %>%
  model(
    ets = ETS(prop~trend("Ad")),
    tslm=TSLM(prop~trend())
  )

fcasts <- models %>%
  forecast(h = "10 years")|>
  tibble()|>
  mutate(fcast_prop=if_else(.mean<0, 0, .mean))|>
  group_by(noc_5, year)|>
  summarize(fcast_prop=mean(fcast_prop))|>
  group_by(year)|>
  mutate(prop=fcast_prop/sum(fcast_prop))|>
  select(year, noc_5, prop)|>
  mutate(series="Rich")

stokes_prop <- stokes|>
  group_by(year)|>
  mutate(prop=Stokes/sum(Stokes),
         series="Stokes")|>
  select(-Stokes)

bind_rows(lfs_props|>as_tibble(), fcasts, stokes_prop)|>
  write_rds(here("out","prop_data.rds"))

level_fcast <- inner_join(stokes_top, fcasts)|>
  mutate(Rich=stokes_top*prop)|>
  select(year,noc_5, Rich)

ensemble_vs_stokes <- inner_join(level_fcast, stokes)

errors <- ensemble_vs_stokes|>
  group_by(noc_5)|>
  nest()|>
  mutate(rmse=map_dbl(data, get_rmse),
         smape=map_dbl(data, get_smape),
         mean_value=map_dbl(data, get_mean)
         )|>
  arrange(desc(mean_value))|>
  select(-data)

new_vs_fcast <- ensemble_vs_stokes|>
  pivot_longer(cols=c(Stokes, Rich), names_to = "series", values_to = "value")|>
  bind_rows(lfs)

write_rds(new_vs_fcast, here("out","new_vs_fcast.rds"))
write_rds(errors, here("out","errors.rds"))






