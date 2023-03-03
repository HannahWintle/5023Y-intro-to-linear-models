# PACKAGES📦 ----
library(tidyverse)
library(here)
library(kableExtra)
library(broom.helpers) #click install
library(janitor)
#___________________________----
#IMPORT DATA📁 ----
darwin <- read_csv(here("darwin", "data", "darwin.csv"))

#___________________________----
#CHECK DATA🔎----
head(darwin)
glimpse(darwin)
colnames(darwin)

#___________________________----
#CLEAN DATA🧹----
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

#__________________________----
#ANOVA (analysis of variance)----

#as designs become more complicated, the number of comparisons can quickly become overwhelming, working with estimates and intervals alone can become harder
#As designs become more elaborate the number of pairwise t-tests rapidly increases, and therefore our risk of false positives (Type I errors).
#It is therefore useful to have a complementary approach that first asks if there is any support for a difference between the different means before diving into multiple comparisons
#This approach is called analysis of variance (ANOVA), and although it tends to be associated with categorical data
#ANOVA is just another type of linear model, so this approach can also be extended to included continuous variables

#our simple linear model for the maize data was:

lsmodel1 <- lm(height ~ type, data = darwin)

summary(lsmodel1)

#The general strategy of the ANOVA is to quantify the overall variability in the data set and then to divide it into the variability between and within the groups.

#The more of the variation explained by our fitted linear model, the more confident we can be that we have detected a real effect in our estimates of mean differences.
#This method of fitting a model is called ordinary least squares.

#SST = SSR + SSE

#REMEMBER

#SST = the sum of squared differences between the data points and the grand mean

#SSR = the sum of the squared differences between the grand mean and the predicted position on the linear model

#SSE = the sum of the squared differences between the predicted position and the observed position

##The ANOVA table----
anova(lsmodel1)

#________________________----
#TWO-WAY ANOVA----

#two way ANOVA includes two explanatory variables

lsmodel2 <- lm(height ~ type + as.factor(pair), data = darwin)

anova(lsmodel2)

#_______________________----
#WORKED EXAMPLE----