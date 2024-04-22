library(tidyverse)
library(readxl)
library(here)

stokes_emp_occ <- read_excel(here("data",
                                  "occupation_new",
                                  "employment_occupation.xlsx"), skip=3)|>
  filter(`Geographic Area`=="British Columbia",
         NOC!="#T")|>
  mutate(Description=if_else(str_detect(Description, "Seniors"),
                             "Senior managers - public and private sector",
                             Description))|>
  select(Description, starts_with("2"))|>
  pivot_longer(cols=starts_with("2"), names_to = "year", values_to = "count")|>
  mutate(year=as.numeric(year),
         sample="stokes")|>
  filter(year >= year(today()))|>
  group_by(year)|>
  mutate(share=count/sum(count))|>
  select(-count)|>
  group_by(Description, sample, .add = FALSE)|>
  mutate(weight = .5^(year-max(year)+1)/sum(.5^(year-max(year)+1)))|>
  summarize(weighted_mean=sum(weight*share))

lfs_emp_occ <- read_excel(here("data",
                               "Labour force status for 5 digit NOC 2014-2023.xlsx"),
                          skip=3,
                          sheet = "Employed")|>
  filter(`Noc 5`!="Total")|>
  select(-`Noc 5`)|>
  rename(Description=`Class Title`)|>
  pivot_longer(cols=starts_with("2"), names_to = "year", values_to = "count")|>
  mutate(Description=if_else(str_detect(Description, "Senior managers|Senior government managers"),
                             "Senior managers - public and private sector",
                             Description))|>
  group_by(Description, year)|>
  summarize(count=sum(count, na.rm = TRUE))|>
  mutate(year=as.numeric(year))|>
  group_by(year)|>
  mutate(share=count/sum(count, na.rm=TRUE),
         sample="lfs")|>
  select(-count)|>
  group_by(Description, sample, .add = FALSE)|>
  mutate(weight = .5^(max(year)-year+1)/sum(.5^(max(year)-year+1)))|>
  summarize(weighted_mean=sum(weight*share))


occ_shares <- full_join(stokes_emp_occ, lfs_emp_occ)|>
  pivot_wider(names_from = sample, values_from = weighted_mean)

plt <- ggplot(occ_shares, aes(lfs,
                       stokes,
                       text=Description))+
  geom_abline(slope = 1, intercept = 0, colour="white", lwd=2)+
  geom_point(alpha=.2)+
  scale_x_continuous(trans="log10", labels = scales::percent)+
  scale_y_continuous(trans="log10", labels = scales::percent)+
  labs(x="Shares based on the Labour Market Survey",
       y="Shares based on Stokes forecast",
       title="Being far from diagonal line indicates large share difference between historic and forecast")

plotly::ggplotly(plt, tooltip = "text")






