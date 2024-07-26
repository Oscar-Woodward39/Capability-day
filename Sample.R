# Setup -------------------------------------------------------------------
rm(list = ls())
setwd('/Users/mattbrown/Documents/Teaching/EnvEconClass/misc/bookdown-Rguide')
library(dplyr)
library(fixest)
library(ggplot2)

# Load data ---------------------------------------------------------------
df_main = read.csv('datasets/mrc_table2.csv')
df_IPEDS_Scorecard = read.csv('datasets/mrc_table10.csv')

# Clean data --------------------------------------------------------------

### Select key variables
df_main = df_main %>% 
  select(super_opeid, name, tier, mr_kq5_pq1, par_median, par_q1)

df_IPEDS_Scorecard = df_IPEDS_Scorecard %>% 
  select(super_opeid, public, hbcu, flagship, scorecard_netprice_2013, state)

### Add supplemental variables to main data
df_main = df_main %>% left_join(df_IPEDS_Scorecard, by = 'super_opeid')

### Drop NAs 
df_main = na.omit(df_main)
  
### Sample restriction (public institutions only)
df_main = df_main %>% filter(public == 1)

### Create standardized price varaible
netprice_mn = mean(df_main$scorecard_netprice_2013)
netprice_sd = sd(df_main$scorecard_netprice_2013)
df_main = df_main %>% 
  mutate(netprice_std = (scorecard_netprice_2013 - netprice_mn )/netprice_sd)

# Plots --------------------------------------------------------------------

### Histograms for key continuous variables
hist_mr_kq5_pq1 = ggplot(data = df_main, aes(x = mr_kq5_pq1)) + 
  geom_histogram() 

hist_par_median = ggplot(data = df_main, aes(x = par_median)) + 
  geom_histogram() 

hist_price = ggplot(data = df_main, aes(x = scorecard_netprice_2013)) + 
  geom_histogram() 

### Scatter plots
uncond_scatter_mobility_price =
  ggplot(data = df_main, aes(y = mr_kq5_pq1, x = scorecard_netprice_2013)) + 
  geom_point()
# It's a good idea to view these plots as you work interactively.
# This can help you decide how to define variables, handle outliers, etc.

# Regressions -------------------------------------------------------------
# What variables are associated with mobility index?

dict = c('mr_kq5_pq1' = 'Mobility Rate',
         'netprice_std' = 'Standardized cost of attendance',
         'flagship' = 'Flagship',
         'hbcu' = 'HBCU',
         'par_q1' = 'Frac w/ parents in bottom income quintile')

reg_nocontrols = feols(
  data = df_main, 
  fml = mr_kq5_pq1 ~ netprice_std + flagship + hbcu,
  vcov = 'HC1'
  )

reg_statefe = feols(
  data = df_main, 
  fml = mr_kq5_pq1 ~ netprice_std + flagship + hbcu | state
  )

reg_statefe_parq1= feols(
  data = df_main, 
  fml = mr_kq5_pq1 ~ netprice_std + flagship + hbcu + par_q1| state
  )

result_list = 
  list(reg_nocontrols, reg_statefe, reg_statefe_parq1)

# Save output -------------------------------------------------------------
write.csv(df_main, file = 'output/analysis_data.csv')

ggsave(hist_mr_kq5_pq1, file = 'output/hist_mr_kq5_pq1.png')
ggsave(hist_par_median, file = 'output/hist_par_median.png')
ggsave(hist_price, file = 'output/hist_price.png')
ggsave(uncond_scatter_mobility_price, 
       file = 'output/uncond_scatter_mobility_price.png')

etable(result_list,    
       dict = dict,
       style.tex = style.tex('aer'),
       file = 'output/reg_table.tex')

etable(result_list,      
       dict = dict,
       style.tex = style.tex('aer'),
       export = 'output/reg_table.png')