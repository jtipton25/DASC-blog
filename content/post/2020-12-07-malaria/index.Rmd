---
title: Malaria over time
author: ~
date: '2020-12-07'
slug: malaria
categories: []
tags: []
---


```{r setup, echo = FALSE, include = FALSE, message = FALSE, tidy = TRUE}
library(here)
library(ggplot2)
library(tidyverse)
library(readxl)
library(mgcv)
library(knitr)

knitr::opts_chunk$set(echo = TRUE, tidy = FALSE)
dat1 <- read_csv(here::here("data", "tidytuesday", "data", "2018", "2018-11-13", "malaria_deaths.csv"))
dat2 <- read_csv(here::here("data", "tidytuesday", "data", "2018", "2018-11-13", "malaria_deaths_age.csv"))
dat3 <- read_csv(here::here("data", "tidytuesday", "data", "2018", "2018-11-13", "malaria_inc.csv"))
dat_codes <- read_csv(here::here("data", "tidytuesday", "data", "2018", "2018-11-13", "country-codes.csv"))

## rename deaths from malaria per 100K
dat1 <- rename(dat1, deaths_per_100K = `Deaths - Malaria - Sex: Both - Age: Age-standardized (Rate) (per 100,000 people)`)

# relabel row id
dat2 <- rename(dat2, row_id = X1)

## rename the incidence of malaria per 1000 population at risk
dat3 <- rename(dat3, incidence_per_1000_at_risk = `Incidence of malaria (per 1,000 population at risk) (per 1,000 population at risk)`)
```

# Malaria deaths

For this project, I am examining the malaria deaths by age dataset contained in the `malaria_deaths_age.csv` file on the Tidy Tuesday website. The data consists of `r nrow(dat2)` observations of `r ncol(dat2)-1` variables. The variable `entity` is a factor with 228 levels that represents the region for the deaths. The variable `code` is a factor with 196 levels which represents a three letter code for the country/region. The variable `year` is a discrete integer variable that records the years between `r min(dat2$year)` and `r max(dat2$year)`. The variable `age_group` is a discrete ordered factor with 5 levels that groups the population into age groups under 5, 5-14, 15-49, 50-69 and 70 or older. The variable `deaths` records the number of deaths from malaria for each region, year, and age group.  Initial exploration shows that the largest variability in the number of deaths occurs across age groups with the largest number of deaths occurring in children under 5. Other variability in deaths occurs when examining death rates by region.

The second dataset we use to understand malaria is the incidence rate of malaria given in the `malaria_inc.csv` file on the Tidy Tuesday website. The malaria incidence rate is the average number of people who contract malaria per 1000 people at risk. The malaria incidence dataset has 508 observations of 4 variables: the variable `Entity`, which is a factor with 127 levels, that represents the region for the malaria cases, the variable `Code`, which is a factor with 101 levels, that represents a three letter code for the country/region, the variable `year`, which is a discrete integer variable that records the years between `r min(dat3$Year)` and `r max(dat3$Year)` in an interval of 5 years, and the variable `Incidence` which records the number of malaria cases per 1000 people at risk.

To perform our analyses, we need to filter out the data that does not come from a country. These include the following:

```{r, include = FALSE}
non_countries <- c("Andean Latin America", "Australasia", "Caribbean", "Central Asia", "Central Europe", "Central Latin America", "Central Sub-Saharan Africa", "East Asia", "Eastern Europe", "Eastern Sub-Saharan Africa", "High SDI", "High-income Asia Pacific", "High-middle SDI", "Latin America and Caribbean", "Low SDI", "Low-middle SDI", "Middle SDI", "North Africa and Middle East", "North America", "Oceania", "South Asia", "Southeast Asia", "Southern Latin America", "Southern Sub-Saharan Africa", "Sub-Saharan Africa", "Tropical Latin America", "Western Europe", "Western Sub-Saharan Africa", "World") 
```

<!-- Using these two datasets, I will explore the following questions. First, is there a significant difference in malaria deaths by age group. If so, which groups are different. Second, is there a significant difference in deaths of malaria between regions with different socio-economic status? Third, how has the death rate from malaria changed over time? Fourth, has the incidence of malaria changed over time? -->

<!-- ```{r, echo = FALSE} -->
<!-- regions <- c("Australasia", "Central Asia", "Central Europe", "Central Latin America", "Central Sub-Saharan Africa", "East Asia", "Eastern Europe", "Eastern Sub-Saharan Africa", "Latin America and Carribean", "North Africa and Middle East", "North America", "Oceania", "South Asia", "Southeast Asia", "Southern Latin America", "Southern Sub-Saharan Africa", "Sub-Saharan Africa", "Tropical Latin America", "Western Europe", "Western Sub-Saharan Africa") -->
<!-- ``` -->

<!-- ```{r, echo - FALSE} -->
<!-- ses <- c("High SDI", "High-middle SDI", "High-income Asia Pacific", "Low SDI", "Low-middle SDI", "Middle SDI") -->
<!-- ``` -->

<!-- ```{r, echo = FALSE, include = FALSE} -->
<!-- by(data = dat2$deaths, dat2$age_group, mean) -->
<!-- ``` -->



<!-- ## Exploratory Data analysis -->

<!-- First, we explore the global patterns in malaria deaths and incidence using the `World` factor in the `entity` variable. -->

## Question 1: 

First, is there a significant difference in malaria deaths by age group. If so, which groups are different. First, we summarize the annual deaths by age group.

```{r, include = TRUE}
## factor in increasing ages
age_levels <- c("Under 5", "5-14", "15-49", "50-69", "70 or older")

dat2 %>%
  mutate(age_group = fct_relevel(age_group, age_levels)) %>%
  filter(!(entity %in% non_countries)) %>%
  group_by(age_group) %>%
  summarize(total_deaths = sum(deaths),
            sd_deaths = sd(deaths),
            count = n()) %>%
  mutate(prop_deaths = total_deaths / sum(total_deaths))
```


From the summary table, we see that the Under 5 age group is the largest proportion of deaths (76.7%) with the next highest death rate fo the 5-14 age group. The dataset shows that there is a very large amount of variability around total deaths. Next, we visualize this result to better highlight the differences. However, the data is really hard to visualize on the original data scale. 

```{r, include = TRUE}
dat2 %>%
  mutate(age_group = fct_relevel(age_group, age_levels)) %>%
  filter(!(entity %in% non_countries)) %>%
  mutate(age_group = fct_relevel(age_group, age_levels)) %>%
  filter(!(entity %in% non_countries)) %>%
  ggplot(aes(x = age_group, y = deaths)) +
  geom_boxplot() +
  geom_point(position = "jitter", alpha = 0.1)
```

There seems to be a pretty interesting grouping happening. Maybe this is related to country/region?




## Question 2:

Second, I want to test whether the death rate from malaria changed over time?


```{r, message = FALSE, fig.width = 4, fig.height = 2.25, out.width = "48%", fig.align = "center", fig.show = "hold"}
dat2 %>%
  filter(!(entity %in% non_countries)) %>%
  group_by(year) %>%
  summarize(total_deaths = sum(deaths)) %>%
  ggplot(aes(x = year, y = total_deaths)) +
  geom_point() +
  stat_smooth(method = "gam") +
  ylim(c(0, 1100000)) +
  ggtitle("Total Deaths")

## plot time varying response
dat2 %>%
  filter(!(entity %in% non_countries)) %>%
  group_by(year, age_group) %>%
  summarize(total_deaths = sum(deaths)) %>%
  mutate(age_group = fct_relevel(age_group, age_levels)) %>% 
  ggplot(aes(x = year, y = total_deaths, group = age_group, color = age_group)) +
  geom_point() +
  stat_smooth(method = "gam", se = FALSE) +
  # stat_smooth(method = "lm") +
  ylim(c(0, 1100000)) +
  ggtitle("Deaths by age group")
```

From these graphics, it appears that there is a change in total deaths over time, with an increase in young child mortality until the mid 2000s and then a relatively rapid decrease in child mortality in the late 2000s and 2010s.

To get a better understanding of the mortality patterns, I will plot the total mortality by country. To visualize this, I will group the country-level data by region. 


```{r, message = FALSE, fig.width = 4, fig.height = 2.25, out.width = "48%", fig.align = "center", fig.show = "hold"}
dat2 %>%
  filter(!(entity %in% non_countries)) %>%
  left_join(dat_codes, by = c("code" = "alpha-3")) %>%
  group_by(year, entity, region) %>%
  ## remove the NA values corresponding to the British Isles
  filter(!is.na(region)) %>%
  summarize(total_deaths = sum(deaths)) %>%
  ggplot(aes(x = year, y = total_deaths, group = entity, color = entity)) +
  geom_line() +
  facet_wrap(~ region) +
  ggtitle("Total Deaths") +
  theme(legend.position = "none")

```


Form this graphic, we can see that the African continent has been the primary driver of Malaria deaths with some deaths in Asia. We also see a large drop in Malaria deaths in the mid 2000s for many African countries. 

This leads to an interesting question: What are the countries that have had the highest Malaria deaths during the study period?

```{r, include = TRUE}
dat2 %>%
  mutate(age_group = fct_relevel(age_group, age_levels)) %>%
  filter(!(entity %in% non_countries)) %>%
  group_by(entity) %>%
  summarize(total_deaths = sum(deaths)) %>%
  arrange(desc(total_deaths))
```

The highest Malaria deaths occurred in Nigeria, Democratic Republic of Congo, India, and down the line. This is not surprising as these are the largest countries in the tropical region where the *Anopheles* mosquitoes that spread Malaria live.





## Question 3:

The third question is whether the incidence of malaria per 100K people at risk changed over time? This is a better indicator of risk as it is the same across countries with large and small populations.


```{r, message = FALSE, fig.width = 4, fig.height = 2.25, out.width = "48%", fig.align = "center", fig.show = "hold"}
dat3 %>%
  filter(!(Entity %in% non_countries)) %>%
  group_by(Year) %>%
  # summarize(mean_incidence = mean(incidence_per_1000_at_risk)) %>%
  ggplot(aes(x = Year, y = incidence_per_1000_at_risk)) +
  geom_point() +
  stat_summary(fun = "mean", geom = "line", color = "red")
  ggtitle("Malaria incidence per 100K")

```

From this graphics, it appears that there is a decrease in Malaria incidence over time.

To get a better understanding of the Malaria incidence patterns, I will plot the incidence over time by country To visualize this, I will group the country-level data by region. 


```{r, message = FALSE, fig.width = 4, fig.height = 2.25, out.width = "48%", fig.align = "center", fig.show = "hold"}
dat3 %>%
  filter(!(Entity %in% non_countries)) %>%
  left_join(dat_codes, by = c("Code" = "alpha-3")) %>%
  group_by(Year, Entity, region) %>%
  ## remove the NA values corresponding to the British Isles
  filter(!is.na(region)) %>%
  ggplot(aes(x = Year, y = incidence_per_1000_at_risk, group = Entity, color = Entity)) +
  geom_line() +
  facet_wrap(~ region) +
  ggtitle("Total Deaths") +
  theme(legend.position = "none")
```


Form this graphic, we can see that the African continent has the highest incidence of Malaria although in Asia, one country has had a very large decrease in Malaria incidence. The incidence rates in Africa have been slowly declining. 


This leads to an interesting question: What are the countries that have had the highest Malaria incidence during 2000? 

```{r, include = TRUE}
dat3 %>%
  filter(!(Entity %in% non_countries)) %>%
  filter(Year == "2000") %>%
  arrange(desc(incidence_per_1000_at_risk))
```

The highest Malaria incidence in 2000 was in Turkey, with many African countries also having high incidence. 

We can compare this to 2015 to see what has changed:

```{r, include = TRUE}
dat3 %>%
  filter(!(Entity %in% non_countries)) %>%
  filter(Year == "2015") %>%
  arrange(desc(incidence_per_1000_at_risk))
```

In 2015, the countries with the highest Malaria incidence have changed but are all in Africa. The overall Malaria incience in the top-10 most affected countries has also decreased. 

## Conclusion:

TBD
