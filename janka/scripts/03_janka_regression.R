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
# VISUALISATION📈----

janka %>% 
  ggplot(aes(x=dens, y=hardness))+
  geom_point()