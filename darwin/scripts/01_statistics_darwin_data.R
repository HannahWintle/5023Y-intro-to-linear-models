# PACKAGES ----
library(tidyverse)
library(here)
library(kableExtra)

#___________________________----
#IMPORT DATA ----
darwin <- read_csv(here("darwin", "data", "darwin.csv"))

#___________________________----
#CHECK DATA----
head(darwin)
glimpse(darwin)
colnames(darwin)

#___________________________----
#CLEAN DATA----
darwin <- janitor::clean_names(darwin)

#duplication
darwin%>%
  duplicated()%>%
  sum()

#typos
darwin%>%
  summarise(min=min(height, na.rm=TRUE),
            max=max(height, na.rm=TRUE))
darwin %>% 
  distinct(pair)

darwin %>% 
  distinct(type)

# missing values
darwin %>% 
  is.na() %>% 
  sum()

# quick summary
summary(darwin)

#________________________----
# VISUALISATION----
darwin %>% 
  ggplot(aes(x=type,
             y=height))+
  geom_point()

# you could also substitute (or combine) other geoms including
# geom_boxplot()
# geom_violin()
# geom_histogram()
# Why not have a go and see what you can make?

#________________________----
# FUNCTIONS----
darwin %>% 
  group_by(type) %>% 
  summarise(mean=mean(height),
            sd=sd(height))
# make a new object
darwin_summary <-darwin %>% 
  group_by(type) %>% 
  summarise(mean=mean(height),
            sd=sd(height))

# make a summary plot
darwin_summary %>% 
  ggplot(aes(x=type,
             y=mean))+
  geom_pointrange(aes(ymin=mean-sd, ymax=mean+sd))+
  theme_bw()

# use kable extra functions to make a nice table (could be replaced with kable() if needed)
darwin_summary %>% 
  kbl(caption="Summary statistics of crossed and selfed maize plants") %>% 
  kable_styling(bootstrap_options = "striped", full_width = T, position = "left")

#_______________________----
#DIFFERENCES----
# pivot data to wide format then subtract Selfed plant heights from Crossed plant heights
darwin_wide <- darwin %>% 
  pivot_wider(names_from = type, values_from = height) %>% 
  mutate(difference = Cross - Self)

difference_summary <- darwin_wide %>% 
  summarise(mean=mean(difference),
            sd=sd(difference),
            n=n())

difference_summary

#What we have just calculated is 
#the average difference in height between these groups of plants
#and the standard deviation of the difference

#standard error of the difference----
difference_summary %>% 
  mutate(se= sd/sqrt(n))

#put this into words and...
#...the average difference in height was 2.62 ± 1.22 inches (mean ± SE).

#______________________----
#UNCERTAINTY----

#normal distribution----
#Create a sequence of 100 equally spaced numbers between -4 and 4
x <- seq(-4, 4, length=100)

#create a vector of values that shows the height of the probability distribution
#for each value in x
y <- dnorm(x)

#plot x and y as a scatterplot with connected lines (type = "l") and add
#an x-axis with custom labels
plot(x,y, type = "l", lwd = 2, axes = FALSE, xlab = "", ylab = "")
axis(1, at = -3:3, labels = c("-3s", "-2s", "-1s", "mean", "1s", "2s", "3s"))

#confidence intervals----
lowerCI <- 2.62-(2*1.22)

upperCI <- 2.62+(2*1.22)

lowerCI
upperCI

#write up example----
#The maize plants that have been cross pollinated were taller on average than the self-pollinated plants, with a mean difference in height of 2.62 [0.18, 5.06] inches (mean [95% CI]).
#____________________----