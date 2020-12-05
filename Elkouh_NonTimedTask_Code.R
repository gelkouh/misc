##-----##
##-----##
# Code: Health insurance coverage task (non-timed)
##-----##
##-----##

# Download All_years.csv into same directory as this .R file
# https://www.dropbox.com/s/fpwv2ztxan7k8p3/All_years.csv?dl=0

# Load necessary packages
if (!require(tidyverse)) install.packages('tidyverse')
library(tidyverse)
if (!require(scales)) install.packages('scales')
library(scales)
if (!require(usmap)) install.packages('usmap')
library(usmap)
if (!require(plm)) install.packages('plm')
library(plm)
if (!require(stargazer)) install.packages('stargazer')
library(stargazer)

# Load data
df_full <- read_csv('All_years.csv')

##-----##
# Figure 1.
##-----##

# Add variable indicating "Other" insurance, i.e., any insurance but not Medicare/Medicaid 
# (e.g., private health insurance)
df_full <- df_full %>%
  mutate(healthcare_other = ifelse(hcovany==2 & hinscare==1 & hinscaid==1, 1, 0))

# Sample size, average age over time
df_by_year_age_count <- df_full %>% 
  group_by(year) %>%
  summarise(avg_age = mean(age), n = n())

figure_1 <- ggplot(df_by_year_age_count, aes(x=year,y=n)) +
  geom_line() +
  xlab('Year') +
  ylab('Sample size') +
  scale_x_continuous(breaks= pretty_breaks()) +
  ggtitle('Figure 1. Sample size per year')

##-----##
# Figure 2.
##-----##

# Count of people with any insurance by year
df_by_year_hcovany <- df_full %>% 
  group_by(year, hcovany) %>%
  summarise(n = n()) %>%
  # this will let plots stack in a more logical order with facet_wrap():
  mutate(hcovany = ifelse(hcovany == '1', '3', '2'))

df_total_count <- df_full %>%
  group_by(year) %>%
  summarise(total_n = n())

# Year-to-year changes in healthcare coverage
df_n_change_year <- df_by_year_hcovany %>%
  group_by(hcovany) %>%
  mutate(n_change = n - dplyr::lag(n), pct_n_change = ((n - dplyr::lag(n))/dplyr::lag(n)) *100) %>%
  mutate(hcovany = ifelse(hcovany == '3', 'no insurance', 'with insurance')) %>%
  merge(df_total_count,by='year') %>%
  mutate(pct_of_total = n/total_n) %>%
  group_by(hcovany) %>%
  mutate(pct_of_total_change = pct_of_total - dplyr::lag(pct_of_total))

figure_2 <- ggplot(filter(df_n_change_year,hcovany=='with insurance'), aes(x=year,y=pct_of_total)) +
  geom_line(col = 'blue') +
  scale_x_continuous(breaks= pretty_breaks()) +
  xlab('Year') +
  ylab('% of sample with any insurance') +
  ggtitle('Figure 2. Share insured')

##-----##
# Figure 3.
##-----##

figure_3 <- ggplot(data = df_by_year_hcovany, mapping = aes(year, n, color = factor(hcovany))) +
  geom_point() + 
  geom_line() +
  scale_color_manual(breaks = c('3','2'), values=c('red','blue')) +
  xlab('Year') +
  ylab('Number of people in sample') +
  scale_x_continuous(breaks= pretty_breaks()) + 
  facet_wrap(~hcovany, scales = "free", nrow=2, 
             labeller = as_labeller(c('2' = 'With coverage','3' = 'No coverage'))) +
  ggtitle('Figure 3. Health insurance coverage count') +
  theme(legend.position="none")

##-----##
# Figure 4.
##-----##

# Health insurance through Medicaid
df_pct_medicaid_hcovany <- df_full %>%
  group_by(year, hinscaid) %>%
  summarise(n_medicaid = n()) %>%
  merge(df_by_year_hcovany) %>%
  filter(hinscaid == 2 & hcovany == 2) %>%
  mutate(pct_medicaid_hcovany = n_medicaid/n) %>%
  select(c(year, n, n_medicaid, pct_medicaid_hcovany))

# Health insurance through Medicare
df_pct_medicare_hcovany <- df_full %>%
  group_by(year, hinscare) %>%
  summarise(n_medicaid = n()) %>%
  merge(df_by_year_hcovany) %>%
  filter(hinscare == 2 & hcovany == 2) %>%
  mutate(pct_medicare_hcovany = n_medicaid/n) %>%
  select(c(year, n, n_medicaid, pct_medicare_hcovany))

df_other_count_year <- df_full %>%
  group_by(year, healthcare_other) %>%
  summarise(n_other = n())

df_pct_other_hcovany <- df_other_count_year %>%
  merge(df_by_year_hcovany) %>%
  filter(healthcare_other == 1 & hcovany == 2) %>%
  mutate(pct_other_hcovany = n_other/n) %>%
  select(c(year, n, n_other, pct_other_hcovany))

df_pct_share_breakdown <- df_pct_medicaid_hcovany %>%
  merge(df_pct_medicare_hcovany,by='year') %>%
  merge(df_pct_other_hcovany,by='year') %>%
  select(year, pct_medicaid_hcovany, pct_medicare_hcovany, pct_other_hcovany)

figure_4 <- ggplot(data = df_pct_share_breakdown) + 
  geom_line(aes(x=year,y=pct_other_hcovany, color = 'red')) +
  geom_line(aes(x=year,y=pct_medicare_hcovany, color = 'light blue')) +
  geom_line(aes(x=year,y=pct_medicaid_hcovany, color = 'dark blue')) +
  scale_x_continuous(breaks= pretty_breaks()) +
  xlab('Year') +
  ylab('% of share of those with insurance') +
  scale_color_manual(name = "Insurance type:", labels = c('Medicaid','Medicare','Other'), 
                     values = c('dark blue','light blue','red')) +
  ggtitle('Figure 4. Coverage composition of those with any insurance')

##-----##
# Figure 5.
##-----##

df_above_65_pct <- df_full %>%
  mutate(over_65 = ifelse(age >= 65, 1, 0)) %>%
  group_by(year, over_65) %>%
  summarise(over_65_count = n()) %>%
  group_by(year) %>%
  mutate(total_n = sum(over_65_count)) %>%
  mutate(pct_over_65 = over_65_count/total_n * 100) %>%
  filter(over_65 == 1) %>%
  select(year, pct_over_65)

df_above_65_pct_insured <- df_full %>%
  mutate(over_65 = ifelse(age >= 65, 1, 0)) %>%
  group_by(year, over_65, hcovany) %>%
  summarise(over_65_count_insured = n()) %>%
  group_by(year) %>%
  filter(over_65 == 1) %>%
  mutate(total_n_over_65 = sum(over_65_count_insured)) %>%
  mutate(pct_over_65_insured = (over_65_count_insured/total_n_over_65) * 100) %>%
  filter(hcovany == 2) %>%
  select(year, pct_over_65_insured) %>%
  merge(df_above_65_pct, by = 'year')

figure_5 <- ggplot(df_above_65_pct_insured, aes(x=year,y=pct_over_65)) +
  geom_line() +
  xlab('Year') + 
  ylab('Percent of sample aged 65+') +
  scale_x_continuous(breaks= pretty_breaks()) +
  ggtitle('Figure 5. 65+ years old')

##-----##
# Figure 6.
##-----##

df_65_cutoff_breakdown <- df_full %>%
  mutate(flag_col = ifelse(age == 65, '65', ifelse(age == 64, '64', 0)),
         insurance_type = ifelse(hcovany == 2, ifelse(hinscaid == 2, 'Medicaid', ifelse(hinscare == 2, 'Medicare', 'Other')), 'Uninsured')) %>%
  filter(flag_col == '65' | flag_col == '64') %>%
  select(year, age, flag_col, insurance_type)

figure_6 <- ggplot(df_65_cutoff_breakdown, aes(x = flag_col,fill=factor(insurance_type))) +
  geom_bar() +
  scale_fill_discrete('Insurance type') +
  xlab('Age') +
  ylab('Total count in entire sample') +
  ggtitle('Figure 6. Changes at 65')

##-----##
# Figure 7.
##-----##

df_age_08 <- df_full %>%
  filter(year == 2008) %>%
  filter(hcovany == 1) %>%
  group_by(age) %>%
  mutate(sum_no_care_2008 = sum(hcovany)) %>%
  select(age, sum_no_care_2008) %>%
  distinct()

df_age <- df_full %>%
  filter(year == 2018) %>%
  filter(hcovany == 1) %>%
  group_by(age) %>%
  mutate(sum_no_care_2018 = sum(hcovany)) %>%
  select(age, sum_no_care_2018) %>%
  distinct() %>%
  merge(df_age_08, by = 'age')

figure_7 <- ggplot(df_age) +
  geom_col(aes(age, sum_no_care_2018), alpha = 0.5, fill = 'dark blue') +
  geom_col(aes(age, sum_no_care_2008), alpha = 0.5, fill = 'dark orange') +
  xlab('Age') +
  ylab('Total count without healthcare') +
  ggtitle('Figure 7. Total count without care: 2008 vs. 2018') +
  labs(caption = '2008 count is in orange and 2018 count is in blue (which appears brown on top of/behind the orange).')

##-----##
# Figure 8.
##-----##

df_race_white <- df_full %>%
  filter(race==1) %>%
  group_by(year, hcovany) %>%
  summarise(sub_n = n()) %>%
  group_by(year) %>%
  mutate(total_n = sum(sub_n)) %>%
  mutate(frac_covered = sub_n / total_n) %>%
  filter(hcovany == 2)

figure_8 <- ggplot(df_race_white, aes(year, frac_covered)) + 
  geom_line(col = 'blue') +
  xlab('Year') +
  ylab('% of white individuals insured') +
  ggtitle('Figure 8. White coverage')

##-----##
# Figure 9.
##-----##

df_race_nonwhite <- df_full %>%
  filter(race!=1) %>%
  group_by(year, hcovany) %>%
  summarise(sub_n = n()) %>%
  group_by(year) %>%
  mutate(total_n = sum(sub_n)) %>%
  mutate(frac_covered = sub_n / total_n) %>%
  filter(hcovany == 2)

figure_9 <- ggplot(df_race_nonwhite, aes(year, frac_covered)) + 
  geom_line(col = 'blue') +
  xlab('Year') +
  ylab('% of nonwhite individuals insured') +
  ggtitle('Figure 9. Nonwhite coverage')

##-----##
# Figure 10.
##-----##

# Change in healthcare coverage over time range of sample
df_state_change <- df_full %>%
  filter(year == 2008 | year == 2018) %>%
  group_by(statefip, year, hcovany) %>%
  summarise(sub_n = n()) %>%
  group_by(statefip, year) %>%
  mutate(tot_n = sum(sub_n)) %>%
  mutate(pct_tot = (sub_n / tot_n)*100) %>%
  group_by(statefip,hcovany) %>%
  mutate(pct_diff = pct_tot - dplyr::lag(pct_tot)) %>%
  filter(hcovany == 2) %>%
  ungroup() %>%
  # must have `fips` column for plot_usmap()
  select(c(fips = statefip,year,pct_diff, pct_tot))

df_state_change_merge_step <- df_state_change %>%
  filter(year == 2018) %>%
  select(fips, pct_diff)

df_state_change <- df_state_change %>%
  merge(df_state_change_merge_step, by = 'fips') %>%
  select(-pct_diff.x) %>%
  rename(pct_diff = pct_diff.y)

figure_10 <- plot_usmap(data = filter(df_state_change, year == 2008), values = 'pct_tot') +
  scale_fill_continuous(name = '% of sample',low = 'white',high='dark orange') +
  theme(legend.position = "right") +
  ggtitle('Figure 10. Share with any insurance: 2008')

##-----##
# Figure 11.
##-----##

figure_11 <- plot_usmap(data = filter(df_state_change, year == 2018), values = 'pct_tot') +
  scale_fill_continuous(name = '% of sample',low = 'white',high='dark orange') +
  theme(legend.position = "right") +
  ggtitle('Figure 11. Share with any insurance: 2018')

##-----##
# Figure 12.
##-----##

figure_12 <- plot_usmap(data = filter(df_state_change, year == 2018), values = 'pct_diff') +
  scale_fill_continuous(low = 'white', high = 'navy', name = '%-point change') +
  theme(legend.position = "right") +
  ggtitle('Figure 12. Change in share of sample with any insurance by state: 2008-2018')

##-----##
# Figure 13.
##-----##

figure_13 <- ggplot(filter(df_state_change, year == 2008), aes(x = pct_tot, y = pct_diff)) +
  geom_point() + 
  geom_smooth(method = lm) +
  xlab('Percentage of sample with any insurance in 2008') + 
  ylab('%-point change from 2008 to 2018') +
  scale_x_continuous(breaks= pretty_breaks()) +
  ggtitle('Figure 13. Change in share of sample with healthcare, by state')

##-----##
# Figure 14.
##-----##

figure_14 <- ggplot(filter(df_other_count_year, healthcare_other == 1), aes(x=year,y=n_other)) +
  geom_line() +
  xlab('Year') +
  ylab('Count of those insured by Other') +
  scale_x_continuous(breaks= pretty_breaks()) +
  ggtitle('Figure 14. Non-Medicare/-Medicade ("Other") coverage')

##-----##
# Table 1. (code for panel dataframe for fixed effects regressions and regression table)
##-----##

df_non_white <- df_full %>%
  group_by(statefip, year, race) %>%
  summarise(sub_n = n()) %>%
  group_by(statefip, year) %>%
  mutate(tot_n = sum(sub_n)) %>%
  filter(race == 1) %>%
  mutate(n_non_white = tot_n - sub_n, pct_non_white = (1 - (sub_n/tot_n))*100) %>%
  select(-c(race, sub_n, tot_n))

df_avg_wt <- df_full %>%
  group_by(statefip, year) %>%
  summarise(avg_wt = mean(perwt))

df_married <- df_full %>%
  mutate(married_simplified = ifelse(marst < 3, 1, 0)) %>%
  group_by(statefip, year, married_simplified) %>%
  summarise(sub_n = n()) %>%
  group_by(statefip, year) %>%
  mutate(tot_n = sum(sub_n)) %>%
  filter(married_simplified == 1) %>%
  mutate(pct_married_simplified = (sub_n/tot_n)*100) %>%
  select(-c(married_simplified, sub_n, tot_n))

df_19_26 <- df_full %>%
  mutate(is_19_26 = ifelse(age >= 19 & age < 26, 1, 0)) %>%
  group_by(statefip, year, is_19_26) %>%
  summarise(sub_n = n()) %>%
  group_by(statefip, year) %>%
  mutate(tot_n = sum(sub_n)) %>%
  filter(is_19_26 == 1) %>%
  mutate(pct_is_19_26 = (sub_n/tot_n)*100) %>%
  select(-c(is_19_26, sub_n, tot_n))

df_22_26 <- df_full %>%
  mutate(is_22_26 = ifelse(age >= 22 & age < 26, 1, 0)) %>%
  group_by(statefip, year, is_22_26) %>%
  summarise(sub_n = n()) %>%
  group_by(statefip, year) %>%
  mutate(tot_n = sum(sub_n)) %>%
  filter(is_22_26 == 1) %>%
  mutate(pct_is_22_26 = (sub_n/tot_n)*100) %>%
  select(-c(is_22_26, sub_n, tot_n))  

df_pct_male <- df_full %>%
  group_by(statefip, year, sex) %>%
  summarise(sub_n = n()) %>%
  group_by(statefip, year) %>%
  mutate(tot_n = sum(sub_n)) %>%
  filter(sex == 1) %>%
  mutate(pct_male = (sub_n/tot_n)*100) %>%
  select(-c(sex, sub_n, tot_n))  

df_educ <- df_full %>%
  mutate(is_college_4yrs = ifelse(educ == 10, 1, 0)) %>%
  group_by(statefip, year, is_college_4yrs) %>%
  summarise(sub_n = n()) %>%
  group_by(statefip, year) %>%
  mutate(tot_n = sum(sub_n)) %>%
  filter(is_college_4yrs == 1) %>%
  mutate(pct_college_4_yrs = (sub_n/tot_n)*100) %>%
  select(-c(is_college_4yrs, sub_n, tot_n))  

df_pct_65 <- df_full %>%
  group_by(statefip, year, age) %>%
  summarise(sub_n = n()) %>%
  group_by(statefip, year) %>%
  mutate(tot_n = sum(sub_n)) %>%
  filter(age == 65) %>%
  mutate(pct_65 = (sub_n/tot_n)*100) %>%
  select(-c(age, sub_n, tot_n))

df_pct_medicaid_of_any <- df_full %>%
  filter(hcovany == 2) %>%
  group_by(statefip, year, hinscaid) %>%
  summarise(sub_n = n()) %>%
  group_by(statefip, year) %>%
  mutate(tot_n = sum(sub_n)) %>%
  filter(hinscaid == 2) %>%
  mutate(pct_medicaid_of_any = (sub_n/tot_n)*100) %>%
  group_by(statefip) %>%
  mutate(lag_pct_medicaid_of_any = dplyr::lag(pct_medicaid_of_any)) %>%
  select(-c(hinscaid, sub_n, tot_n))

df_pct_medicare_of_any <- df_full %>%
  filter(hcovany == 2) %>%
  group_by(statefip, year, hinscare) %>%
  summarise(sub_n = n()) %>%
  group_by(statefip, year) %>%
  mutate(tot_n = sum(sub_n)) %>%
  filter(hinscare == 2) %>%
  mutate(pct_medicare_of_any = (sub_n/tot_n)*100) %>%
  group_by(statefip) %>%
  mutate(lag_pct_medicare_of_any = dplyr::lag(pct_medicare_of_any)) %>%
  select(-c(hinscare, sub_n, tot_n))

df_avg_inc <- df_full %>%
  group_by(statefip, year) %>%
  summarise(avg_inc = mean(inctot)) %>%
  mutate(lag_avg_inc = dplyr::lag(avg_inc)) %>%
  select(-avg_inc)

df_inc_below_20k_age_over_25_under_65 <- df_full %>%
  filter(age > 25 & age < 65) %>%
  mutate(inc_below_20k = ifelse(inctot < 20000, 1, 0)) %>%
  group_by(statefip, year, inc_below_20k) %>%
  summarise(sub_n = n()) %>%
  group_by(statefip, year) %>%
  mutate(tot_n = sum(sub_n)) %>%
  filter(inc_below_20k == 1) %>%
  mutate(pct_inc_below_20k = (sub_n/tot_n)*100) %>%
  group_by(statefip) %>%
  mutate(lag_pct_inc_below_20k = dplyr::lag(pct_inc_below_20k)) %>%
  select(-c(inc_below_20k, sub_n, tot_n))

df_state_change_by_year <- df_full %>%
  group_by(statefip, year, hcovany) %>%
  summarise(sub_n = n()) %>%
  group_by(statefip, year) %>%
  mutate(tot_n = sum(sub_n)) %>%
  mutate(pct_tot = (sub_n / tot_n)*100) %>%
  filter(hcovany == 2) %>%
  group_by(statefip) %>%
  mutate(pct_covered_any_pct_pt_change = pct_tot - dplyr::lag(pct_tot)) %>%
  rename(sample_size_n = tot_n, pct_covered_any = pct_tot) %>%
  mutate(lag_pct_covered_any = dplyr::lag(pct_covered_any), after_2010 = ifelse(year > 2010, 1, 0)) %>%
  select(statefip, year, pct_covered_any_pct_pt_change, pct_covered_any, sample_size_n, lag_pct_covered_any, after_2010) %>%
  left_join(df_non_white, by=c('statefip', 'year')) %>%
  left_join(df_avg_wt, by=c('statefip', 'year')) %>%
  left_join(df_married, by=c('statefip', 'year')) %>%
  left_join(df_19_26, by=c('statefip', 'year')) %>%
  left_join(df_22_26, by=c('statefip', 'year')) %>%
  left_join(df_pct_male, by=c('statefip', 'year')) %>%
  left_join(df_educ, by=c('statefip', 'year')) %>%
  mutate(interaction_educ_22_26 = pct_is_22_26 * pct_college_4_yrs) %>%
  left_join(df_pct_65, by=c('statefip', 'year')) %>%
  left_join(df_pct_medicaid_of_any, by=c('statefip', 'year')) %>%
  left_join(df_pct_medicare_of_any, by=c('statefip', 'year')) %>%
  left_join(df_avg_inc, by=c('statefip', 'year')) %>%
  left_join(df_inc_below_20k_age_over_25_under_65, by=c('statefip', 'year')) %>%
  mutate(interaction_lag_low_inc_medicaid = lag_pct_inc_below_20k * pct_medicaid_of_any)

fdata_lm_fe <- data.frame(df_state_change_by_year)
pdata_lm_fe <- pdata.frame(x = fdata_lm_fe, index = c("statefip", "year"))

lm_fe_1 <- plm(pct_covered_any_pct_pt_change ~ lag_pct_covered_any,
                           data = pdata_lm_fe, model = "within",effect = "twoways")
lm_fe_2 <- plm(pct_covered_any_pct_pt_change ~ lag_pct_covered_any + pct_medicaid_of_any + pct_medicare_of_any,
               data = pdata_lm_fe, model = "within",effect = "twoways")
lm_fe_3 <- plm(pct_covered_any_pct_pt_change ~ lag_pct_covered_any + pct_medicaid_of_any + 
                 pct_inc_below_20k + lag_pct_inc_below_20k + lag_avg_inc,
               data = pdata_lm_fe, model = "within",effect = "twoways")
lm_fe_4 <- plm(pct_covered_any_pct_pt_change ~ lag_pct_covered_any + pct_medicare_of_any + pct_65,
               data = pdata_lm_fe, model = "within",effect = "twoways")
lm_fe_5 <- plm(pct_covered_any_pct_pt_change ~ lag_pct_covered_any + pct_medicaid_of_any + pct_medicare_of_any + 
                 pct_inc_below_20k + lag_pct_inc_below_20k + lag_avg_inc,
               data = pdata_lm_fe, model = "within",effect = "twoways")
lm_fe_6 <- plm(pct_covered_any_pct_pt_change ~ lag_pct_covered_any + pct_medicaid_of_any + pct_medicare_of_any + 
                 pct_inc_below_20k + lag_pct_inc_below_20k + lag_avg_inc + pct_married_simplified + pct_non_white + avg_wt + pct_65,
               data = pdata_lm_fe, model = "within",effect = "twoways")

table_1 <- stargazer(lm_fe_1, lm_fe_2, lm_fe_3, lm_fe_4, lm_fe_5, lm_fe_6, 
                     digits = 3, header = FALSE, type = 'latex', model.numbers = FALSE,
                     column.labels = c("(1)", "(2)", "(3)", "(4)", "(5)", "(6)"))

# Display figures:
figure_1
figure_2
figure_3
figure_4
figure_5
figure_6
figure_7
figure_8
figure_9
figure_10
figure_11
figure_12
figure_13
figure_14

# Display regression table:
table_1

#pairs(fdata_lm_fe)
