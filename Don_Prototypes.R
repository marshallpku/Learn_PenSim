library("dplyr")
devtools::install_github("donboyd5/decrements")
library("decrements")
glimpse(mortality)
count(mortality, tablename, tid, series, memtype, collar, sex, year)
mortality %>% select(tablename, age, qx.m) %>% spread(tablename, qx.m) %>% data.frame
qplot(age, qx.m, data=mortality, colour=tablename, geom=c("point", "line"))

mortality %>% filter(age>=20) %>%
  group_by(tablename) %>%
  mutate(cqx=cumprod(1-qx.m)) %>%
  ungroup %>%
  qplot(age, cqx, data=., colour=tablename, geom=c("point", "line"))



devtools::install_github("donboyd5/decrements")
library(decrements)
library(dplyr)
library(tidyr)

data(package="decrements")
?mortality
?termination

glimpse(mortality)
count(mortality, tablename)

glimpse(termination)
count(termination, tablename)
termination
