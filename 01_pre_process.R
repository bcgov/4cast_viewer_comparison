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
  sqrt(mean((tbbl$stokes-tbbl$`ensemble forecast`)^2))
}

get_smape <- function(tbbl){
  mean(abs(tbbl$stokes-tbbl$`ensemble forecast`)/(abs(tbbl$stokes)+abs(tbbl$`ensemble forecast`)))
}

get_mean <- function(tbbl){
  (mean(tbbl$stokes)+mean(tbbl$`ensemble forecast`))/2
}

fix_noc <- function(tbbl){
  tbbl|>
    mutate(NOC=str_replace_all(NOC, "o","#"))
}

# comparision between stokes cuts----------------------------

new_vs_old <- tibble(which_file=list.files(here("data","occupation_new")))|>
  mutate(new_data=map(here("data","occupation_new", which_file), read_excel, skip=3, na="NA", col_types=c(rep("text",5), rep("numeric",14))),
         old_data=map(here("data","occupation_old", which_file), read_excel, skip=3, na="NA", col_types=c(rep("text",5), rep("numeric",13))),
         new_data=map(new_data, pivot_longer, cols=starts_with("2"), names_to="year", values_to="new_value"),
         old_data=map(old_data, pivot_longer, cols=starts_with("2"), names_to="year", values_to="original_value"),
         old_data=map(old_data, fix_noc),#for some reason the NOCs were f'd up ????
         new_data=map(new_data, fix_noc)#for some reason the NOCs were f'd up ????
         )|>
  mutate(joined=map2(new_data, old_data, inner_join))|>
  select(-new_data, -old_data)

write_rds(new_vs_old, here("out","new_vs_old.rds"))

# comparison with an ensemble forecast---------------------
noc_names <- read_excel(here("data","occupation_new", "employment_occupation.xlsx"), skip=3)|>
  select(NOC, Description)|>
  distinct()|>
  mutate(noc_5=paste(NOC, Description, sep=": "))

stokes <- read_excel(here("data","occupation_new", "employment_occupation.xlsx"), skip=3)|>
  filter(`Geographic Area`=="British Columbia")|>
  pivot_longer(cols=starts_with("2"), names_to = "syear")|>
  clean_names()|>
  mutate(syear=as.numeric(syear))|>
  filter(noc!="#T",
         syear>max(syear)-10)|>
  mutate(noc_5=paste(noc, description, sep=": "),
         .model="stokes")|>
  select(noc_5, syear, .model, .mean=value)

lfs <- vroom(list.files(here("data", "LFS"), full.names = TRUE))|>
  clean_names()|>
  filter(lf_stat=="Employed",
         !is.na(syear),
         noc_5!="missi")|>
  group_by(noc_5)|>
  mutate(nobs=n())|>
  ungroup()|>
  filter(nobs==max(nobs))|>
  select(-nobs)|>
  mutate(noc_5=if_else(noc_5 %in% paste0("000",11:15), "00018", noc_5),
         noc=paste0("#",noc_5))|>
  group_by(syear, noc)|>
  summarize(count=sum(count))|>
  left_join(noc_names, by=c("noc"="NOC"))|>
  select(-noc, -Description)

lfs_tsibble <- lfs|>
  ungroup()|>
  filter(syear<max(syear))|>
  mutate(count=count/12)|>
  tsibble(key = noc_5, index = syear)|>
  fill_gaps(count=0L)

models <- lfs_tsibble %>%
  model(
    ets = ETS(count),
    tslm=TSLM(count~trend())
  )

fcasts <- models %>%
  forecast(h = "10 years")|>
  tibble()|>
  mutate(.mean=if_else(.mean<0, 0, .mean))|>
  select(-count)

ensemble_fcasts <- fcasts|>
  group_by(noc_5, syear)|>
  summarize(.mean=mean(.mean),
            .model="ensemble forecast")

lfs <- lfs_tsibble|>
  rename(.mean=count)|>
  mutate(.model="LFS")

new_vs_fcast <- bind_rows(ensemble_fcasts, lfs, stokes)

errors <- new_vs_fcast|>
  pivot_wider(names_from = ".model", values_from = ".mean")|>
  select(-LFS)|>
  na.omit()|>
  group_by(noc_5)|>
  nest()|>
  mutate(rmse=map_dbl(data, get_rmse),
         smape=map_dbl(data, get_smape),
         mean_value=map_dbl(data, get_mean)
         )|>
  arrange(desc(mean_value))

write_rds(new_vs_fcast, here("out","new_vs_fcast.rds"))
write_rds(errors, here("out","errors.rds"))

#
#
#
# plt <- ggplot(errors, aes(rmse, smape, size=mean_value, colour=mean_value, alpha=mean_value, text=noc_5))+
#   geom_point()+
#   scale_x_continuous(trans="log10")+
#   scale_y_continuous(trans="log10")+
#   scale_colour_viridis_c()
#
# plotly::ggplotly(plt, tooltip = "text")
#
#
# new_vs_fcast|>
#   filter(str_detect(noc_5,"11100"))|>
#   ggplot(aes(syear, .mean, colour=.model))+
#   geom_line()+
#   labs(x=NULL, y=NULL, colour=NULL)+
#   scale_y_continuous(labels = scales::comma)
#
#
