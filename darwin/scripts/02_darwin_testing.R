# PACKAGES📦 ----
library(tidyverse)
library(here)
library(kableExtra)
library(broom.helpers) #click install

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

#________________________----
# FUNCTIONS📥----
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

#___________________----
# T-TEST ----

#The Student's t-test uses the t-distribution, a small-sample size version of the normal distribution, where tails are fatter if the degrees of freedom are small. 
#There are two basic types of t-test which we have encountered with our linear models.
##The one sample t-test: takes the mean of a sample and compares it with the null hypothesis of zero
##The two sample t-test which compares the difference between the means of two samples against a null hypothesis of no difference between the means of the two populations.

##base R ----
x <- seq(-4, 4, length=100)
hx <- dnorm(x)

degf <- c(1, 3, 8, 30)
colors <- c("red", "blue", "darkgreen", "gold", "black")
labels <- c("df=1", "df=3", "df=8", "df=30", "normal")

plot(x, hx, type="l", lty=2, xlab="x value",
     ylab="Density", main="Comparison of t Distributions")

for (i in 1:4){
  lines(x, dt(x,degf[i]), lwd=2, col=colors[i])
}

legend("topright", inset=.05, title="Distributions",
       labels, lwd=2, lty=c(1, 1, 1, 1, 2), col=colors)

##tidyverse ----
x <- seq(-4, 4, length=100)
z_dist <- dnorm(x)

values <- tibble(x,z_dist)

# map_dfc combines values returned into a dataframe
t <- map_dfc(degf, ~dt(x, .x))
colnames(t) <- degf

combined <- cbind(values,t)

combined %>% 
  pivot_longer(cols=!x, names_to="distribution") %>% 
  mutate(distribution=factor(distribution, levels=c("z_dist", "1", "3", "8", "30"))) %>%  
  mutate(distribution=fct_recode(distribution, "z distribution" = "z_dist", "df = 1" = "1", "df = 3" = "3", "df = 8" = "8", "df = 30" = "30")) %>% 
  ggplot(aes(x=x, y=value, colour=distribution))+
  geom_line(linetype="dashed")+
  theme_classic()

## degrees of freedom ----

#There is critical t the value which must be exceeded for the test to be significant (e.g. generates a P value that is less than our predefined α). 
#The critical value for t is defined by the df
#When observed t > critical t the result can be declared significantly different at that threshold for α 

df <- c(1:30)

# map_dbl forces returned values to be a single vector of numbers (rather than a list)
critical_t <- map_dbl(df, ~qt(p=0.05/2, df=.x, lower.tail=FALSE))

tibble(df,critical_t) %>% 
  ggplot(aes(x=df, y=critical_t))+
  geom_point()+
  geom_line()+
  geom_hline(aes(yintercept=1.96), linetype="dashed", colour="red")+
  labs(x= "Degrees of Freedom",
       y= expression(paste("Critical value of ", italic("t"))))

#Summary is the most common way to investigate a model result, but it is its own specific type of R object (e.g. not a dataframe or a tibble), which is why tidying the results into a dataframe like structure can be useful. 
#Using either method we can can see that they include t-tests for the coefficient, summary explicity calls them t, while tidy() refers to them generically as the 'statistic'

lsmodel1 <- lm(height ~ type, data = darwin)

summary(lsmodel1)

broom::tidy(lsmodel1)

tidy_model1 <- broom::tidy(lsmodel1)

tidy_model1[[2,2]] / tidy_model1[[2,3]]

#________________________----
# PAIRED T ----

#when we first calculated our estimates by hand - we started by making an average of the paired differences in height. 
#To generate the equivalent of a paired t-test, we simply have to add the factor for pairs to our linear model formula
darwin %>% 
  mutate(pair = as_factor(pair)) %>% 
  lm(height ~ type + pair, data = .) %>% 
  broom::tidy()

##confidence intervals----
lm(height ~ type + factor(pair), data = darwin) %>% 
  broom::tidy(., conf.int=T) %>% 
  slice(1:2) # just show first two rows

#We can see that estimate of the mean difference is identical but the 95% confidence intervals are now slightly different. 
#So in this particular version we have actually increased our level of uncertainty by including the pair parameter.
m1 <- lm(height ~ type, data = darwin) %>% 
  broom::tidy(., conf.int=T) %>% 
  slice(2:2) %>% 
  mutate(model="unpaired")

m2 <- lm(height ~ type + factor(pair), data = darwin) %>% 
  broom::tidy(., conf.int=T) %>% 
  slice(2:2) %>% 
  mutate(model="paired")

rbind(m1,m2) %>% 
  ggplot(aes(model, estimate))+
  geom_pointrange(aes(ymin=conf.high, ymax=conf.low))+
  geom_hline(aes(yintercept=0), linetype="dashed")+
  theme_minimal()+
  coord_flip()

#_____________________----
#REPEATABILITY----
set.seed(1234)

myList <- vector("list", 20)
y <- tibble()

for (i in 1:length(myList)) { 
  
  x <-  rnorm(n=12, mean=2.6, sd=2.83)
  data <- tibble(x)
  temp <- lm(x~1, data=data) %>% 
    broom::tidy(conf.int=T) 
  y <- rbind(y,temp)  
  
}

y$`experiment number` <- rep(1:20)

# the new dataframe y contains the results of 20 new experiments

#Using this newly generated data how may experiments found a significant difference and how many did not?
y %>% 
  mutate(`p value < 0.05` = if_else(p.value > 0.049, "non-significant", "significant")) %>% 
  group_by(`p value < 0.05`) %>% 
  summarise(`number of experiments`=n())

#Using this newly generated data compare the estimates and confidence intervals?
y %>% 
  ggplot(aes(x=`experiment number`, y=estimate))+
  geom_pointrange(aes(ymin = conf.low, ymax=conf.high))+
  labs(y = "Estimated mean effect of outcrossing")+
  geom_hline(linetype="dashed", yintercept=0.05)+
  theme_minimal()