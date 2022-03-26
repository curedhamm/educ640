# Lab 1 Key
## load needed packages
needs(tidyverse, rio, here, knitr, kableExtra, misty, janitor, rstatix, psych)

## Importing data
df <- misty::read.sav(here("assignments_data/Lab1_Vocab.sav"), 
                      use.value.labels = TRUE) %>% 
              clean_names() # cleaning up variable names

# Descriptive statistics
describe(df) %>% 
  select(n, mean, sd, skew, kurtosis, se)

# Histograms
ggplot(data = df, aes(x = vocab)) +
  geom_histogram(bins = 20)

# Boxplot
ggplot(data = df, aes(x = instruct, y = vocab)) +
  geom_boxplot()

# F-ratio
## calculate the between group variance (i.e., grand mean difference)
SSE.m <- sum( (df$vocab - mean(df$vocab))^2 )

## transform from long to wide data to calculate within group variance
df_wide <- spread(df, instruct, vocab) %>% 
  clean_names()

## calculate the within group variance, for each group
SSE.1 <- (df_wide$physical_science - mean(df_wide$physical_science, na.rm = T))^2
SSE.2 <- (df_wide$social_science - mean(df_wide$social_science, na.rm = T))^2

## sum within group variance
SSE <- sum(SSE.1, SSE.2, na.rm = T)

## calculate difference for between group variance (grand mean difference) and group variance
SSR <- SSE.m-SSE

## calculate mean square
MSR <- SSR/1   # (no. of groups - 1) or 1 degrees of freedom

## calculate mean square
MSE <- SSE/22  # (sample size - no. of groups) or 22 degrees of freedom

## produce F ratio statistic
Fratio <- MSR/MSE

## produce p-value for F-ratio
p.val  <- pf(Fratio, 1, 22, lower.tail=FALSE)
p.val
