# PACKAGES📦 ----
library(tidyverse)
library(here)
library(kableExtra)
library(broom.helpers) #click install
library(janitor)
library(emmeans)
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

#two way ANOVA includes (as you might guess) two variables

lsmodel2 <- lm(height ~ type + as.factor(pair), data = darwin)

anova(lsmodel2)

#________________________----
#SUMMARY----
#ANOVA tables can be built for any linear model. 
#The tables partition the variance into signal(s) and noise, which can be compared using an F-test. 
#For complex analyses where many pairwise comparisons could be performed, an initial F-test can provide the initial evidence for whether there are any differences at all, reducing the risk of overtesting and the false positives that can be generated.

#We use regression for two continuous variables, ANOVA is used with one continuous variable and one categorical variable
#Usually use a boxplot or bargraph to present data
#Residuals tell you how close the data is to a normal distribution (the closer to 0, the better)

#_______________________----
#WORKED EXAMPLE----

##An analysis of the development time of frogspawn in response to water temperature

#import frog data
frogs <- read_csv(here("frogs", "data", "frogs_messy_data.csv"))

#___________________________----
#CHECK FROG DATA🔎----
head(frogs)
glimpse(frogs)
colnames(frog)

#___________________________----
#CLEAN FROG DATA🧹----
frogs <- janitor::clean_names(frogs)

#duplication
frogs %>%
  duplicated()%>%
  sum()

#typos
frogs %>%
  summarise(min=min(height, na.rm=TRUE),
            max=max(height, na.rm=TRUE))
frogs %>% 
  distinct(pair)

frogs %>% 
  distinct(type)

# missing values
frogs %>% 
  is.na() %>% 
  sum()

# quick summary
summary(frogs)

#___________________________----
#TIDY FROG DATA----

frogs <- frogs %>% 
  rename("13" = Temperature13,
         "18" = Temperature18,
         "25" = Temperature25,
         frogspawn_id = `Frogspawn sample id`) %>% 
  pivot_longer(`13`:`25`, names_to="temperature", values_to="days") %>% 
  drop_na(days)

#__________________________----
#ANALYSE FROG DATA----
#produce a linear model, check it's assumptions and interpret its finding

lsmodel_frogs1 <- lm(days ~ temperature, data = frogs)

# summary(lsmodel_frogs1)

# anova(lsmodel_frogs1)

broom::tidy(lsmodel_frogs1, conf.int = T)

#check assumptions
##base
plot(lsmodel_frogs1)

##tidyverse
performance::check_model(lsmodel_frogs1,
                         check = c("qq", "outliers", "homogeneity"))
#___________________________----
#PRODUCE A FIGURE OF FROG DATA----

ggplot(data = frogs, aes(x = temperature, y = days)) +
  geom_boxplot(aes(fill = temperature), # note fill is "inside" colour and colour is "edges" - try it for yourself
               alpha = 0.2, # fainter boxes so the points "pop"
               width = 0.5, # change width of boxplot
               outlier.shape=NA)+
  geom_jitter(aes(colour = temperature),
              width=0.2)+
  theme(legend.position = "none")
#Figure 1. Frogspawn hatching times at 13, 18 and 25 degrees Celsius. Boxplot displays median, hinges are first and third quartiles, whiskers extend from the hinge to 1.5X the interquartile range. Points represent individual frogspawns.

means <- 
  emmeans::emmeans(lsmodel_frogs1, 
                 specs = pairwise ~ temperature)$contrast %>%
  as.data.frame()

means


#___________________________----
#SUMMARY OF RESULTS----
#Increasing temperatures had a clear effect on reducing the time taken for frogspawn to hatch (one-way ANOVA: F2,57 = 385.9, P < 0.001). At 13∘C the mean time to hatching was 26.3 days [25.8-26.8 95% CI], this reduced by an average of 5.3 days [4.57 - 6.02] at 18∘C and by 10.1 days [9.37 - 10.82] at 25∘C.