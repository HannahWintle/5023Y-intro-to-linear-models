# PACKAGES📦 ----
library(tidyverse)
library(rstatix)
library(performance)

#___________________________----
#IMPORT DATA📁 ----
janka <- read_csv(here("janka", "data", "janka.csv"))

#___________________________----
#CHECK DATA🔎----
head(janka)
glimpse(janka)
colnames(janka)

#___________________________----
#CLEAN DATA🧹----
janka <- janitor::clean_names(janka)

#duplication
janka%>%
  duplicated()%>%
  sum()

# missing values
janka %>% 
  is.na() %>% 
  sum()

# quick summary
summary(janka)

#________________________----
# PEARSONS R CORRELATION----

janka %>% 
  ggplot(aes(x=dens, y=hardness))+
  geom_point()

#________________________----
#REGRESSION IN R----

# regression is similar to pearson's correlation except it looks for causation

janka_ls1 <- lm(hardness ~ dens, data = janka)

# specify linear model method for line of best fit (95% confidence interval)

janka %>% 
  ggplot(aes(x=dens, y=hardness))+
  geom_point()+
  geom_smooth(method="lm")

#________________________----
#MEAN CENTERED REGRESSION----

dens_mean <- janka %>% 
  summarise(mean_dens=mean(dens))
# 45.73333

janka %>% 
  mutate(centered_dens = dens-pull(dens_mean)) %>% 
  lm(hardness ~ centered_dens, data = .) %>% 
  broom::tidy()

#_______________________----
#CONFIDENCE INTERVALS----

#base
confint(janka_ls1)

#tidyverse
broom::tidy(janka_ls1, conf.int=T, conf.level=0.95)

#________________________----
#EFFECT SIZE----

#base
summary(janka_ls1)

#tidyverse
janka_ls1 %>% 
  broom::glance()

#________________________----
#ASSUMPTIONS----

#base
predict(janka_ls1)

resid(janka_ls1)

#tidyverse
janka_ls1 %>% 
  broom::augment() %>% 
  head()

##PLOT----
#black fitted regression line and red dashed lines representing the residuals
augmented_ls1 <- janka_ls1 %>% 
  broom::augment()

augmented_ls1 %>% 
  ggplot(aes(x=dens, 
             y=.fitted))+
  geom_line()+ 
  geom_point(aes(x=dens, 
                 y=hardness))+
  geom_segment(aes(x=dens, 
                   xend=dens, 
                   y=.fitted, 
                   yend=hardness), 
               linetype="dashed", colour="red")

# A line connecting all the data points in order 
p1 <- augmented_ls1 %>% 
  ggplot(aes(x=dens, y=hardness))+
  geom_line()+
  ggtitle("Full Data")

# Plotting the fitted values against the independent e.g. our regression line
p2 <- augmented_ls1 %>% 
  ggplot(aes(x=dens, y=.fitted))+
  geom_line()+
  ggtitle("Linear trend")

# Plotting the residuals against the fitted values e.g. remaining variance
p3 <- augmented_ls1 %>% 
  ggplot(aes(x=.fitted, y=.resid))+
  geom_hline(yintercept=0, colour="white", size=5)+
  geom_line()+
  ggtitle("Remaining \npattern")


library(patchwork)
p1+p2+p3

##The above is an example of functional, but repetitive code - could you make a function that reduces the amount of code needed?

model_plot <- function(data=augmented_ls1, 
                       x="dens", 
                       y="hardness", 
                       title="Full data"){
  ggplot(aes(x=.data[[x]], 
             y=.data[[y]]), 
         data=data)+
    geom_line()+
    theme_bw()+
    ggtitle(title)
}

p1 <- model_plot()
p2 <- model_plot(y=".fitted", title="Linear prediction")
p3 <- model_plot(y=".resid", title="Remaining pattern")

##NORMAL DISTRIBUTION----

#base
plot(janka_ls1, which=c(2,2))

#tidyverse
performance::check_model(janka_ls1, check=c("normality","qq"))

##EQUAL VARIANCE----

#base
plot(janka_ls1, which=c(1,3))

#tidyverse
performance::check_model(janka_ls1, check="homogeneity")

##OUTLIERS----

#base
plot(janka_ls1, which=c(4,5))

#tidyverse
performance::check_model(janka_ls1, check="outliers")

#____________________----
#PREDICTION----

#Using the coefficients of the intercept and the slope we can make predictions on new data
coef(janka_ls1)

#imagine we have a new wood samples with a density of 65, how can we use the equaation for a linear regression to predict what the timber hardness for the wood sample should be?
coef(janka_ls1)[1] + coef(janka_ls1)[2] * 65

#instead we can use functions like predict and broom::augment

#base
predict(janka_ls1, newdata=list(dens=c(22,35,65)))

#tidyverse
broom::augment(janka_ls1, 
               newdata=tibble(dens=c(22,35,65)))

##ADDING CONFIDENCE INTERVALS----

#standard error
broom::augment(janka_ls1, newdata = tibble(dens=c(22,35,65)), se=TRUE)

#95% confidence intervals
broom::augment(janka_ls1, newdata=tibble(dens=c(22,35,65)), interval="confidence")

#emmeans package
emmeans::emmeans(janka_ls1, 
                 specs = "dens", 
                 at = list(dens = c(22, 35, 65)))

#example
pred_newdata <- broom::augment(janka_ls1, 
                               newdata=tibble(dens=c(22,35,65)))

janka %>% 
  ggplot(aes(x=dens, y=hardness))+
  geom_point()+
  geom_smooth(method="lm")+
  geom_point(data=pred_newdata, aes(y=.fitted, x=dens), colour="red")+
  geom_label(data=pred_newdata, (aes(y=(.fitted+10), x=(dens+3), label=round(.fitted, digits=0))))+
  theme_bw()+
  labs(x="Density", y="Timber Hardness")+
  scale_x_continuous(limits=c(20,80), expand=expansion(add=c(0,5)))