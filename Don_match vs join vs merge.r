

library(data.table)
library(dplyr)

# Let's say we have two df's, a and b, that are both big
# they share grouping variables i, j, k
# a has unique variable x, b has unique variable y
# we want to get y onto the a df (left join)

# match can be particularly fast if we have a lot of variables
# define possible values for the grouping variables; I use numerics, but character variables are ok, too
i.idx <- 1:1e3
j.idx <- 1:1e2
k.idx <- 1:100
length(i.idx) * length(j.idx) * length(k.idx) / 1e6 # millions of combinations

set.seed(1234)

# let's make "a" have the complete set of possibilities
adf <- expand.grid(i=i.idx, j=j.idx, k=k.idx)
adf$x <- sample(1:500, nrow(adf), replace=TRUE)
ht(adf)

# let's make "b" be somewhat smaller
bdf <- expand.grid(i=sample(i.idx, round(max(i.idx)*.7)),
                   j=sample(j.idx, round(max(j.idx)*.4)),
                   k=sample(k.idx, round(max(k.idx)*.6)))
bdf$y <- sample(800:1200, nrow(bdf), replace=TRUE)


# first let's create ab, getting y from b ####
# the merge approach
system.time(abdf.merge <- merge(adf, bdf, all.x=TRUE))
ht(abdf.merge)
sum(abdf.merge$y, na.rm=TRUE) # 92 secs on my machine

# the match approach - we need to create an index into the other file
a <- proc.time()
abdf.match <- adf
# now create the index - it doesn't have to be a separate vector, it could be created on the fly within []
# the left side of match is the "left_join" df, and the right is the df we will look up into
idx <- match(with(adf, paste(i, j, k)),
             with(bdf, paste(i, j, k))) # get index of FIRST occurrence within bdf of an i, j, k combination, for each observation in adf
max(idx, na.rm=TRUE) # make sure the index doesn't exceed number of records in bdf - we'll use it to look up in bdf
abdf.match$y <- bdf$y[idx]
b <- proc.time()
b - a # 16 secs on my machine
sum(abdf.match$y, na.rm=TRUE) # should be same as the merge approach

# the left join approach
system.time(abdf.lj <- left_join(adf, bdf)) # only 4 secs on my machine
sum(abdf.lj$y, na.rm=TRUE)



# now create ba, getting x from a ####
# the merge approach
system.time(badf.merge <- merge(bdf, adf, all.x=TRUE))
ht(badf.merge)
sum(badf.merge$x, na.rm=TRUE) # 56 secs on my machine

# the match approach
a <- proc.time()
badf.match <- bdf
idx <- match(with(bdf, paste(i, j, k)),
             with(adf, paste(i, j, k))) # get index of FIRST occurrence within adf of an i, j, k combination, for each observation in bdf
max(idx, na.rm=TRUE) # make sure the index doesn't exceed number of records in adf - we'll use it to look up in adf
badf.match$x <- adf$x[idx]
b <- proc.time()
b - a # 15 secs on my machine
sum(badf.match$x, na.rm=TRUE) # should be same as the merge approach

# the left join approach
system.time(badf.lj <- left_join(bdf, adf)) # only 11 secs on my machine
sum(badf.lj$x, na.rm=TRUE)



## Experiment with data of vested terms. 

load("term_df.RData")

str(liab.term)
str(df_wf_term)


## merging by match()
a <- proc.time()
df_wf_term.match <- df_wf_term
# now create the index - it doesn't have to be a separate vector, it could be created on the fly within []
# the left side of match is the "left_join" df, and the right is the df we will look up into

i <- with(df_wf_term[1:1e7], paste(ea, age, year)) # paste is very slow with big dataset. 

idx <- match(with(df_wf_term, paste(ea, age, year, year.term)),
             with(liab.term, paste(ea, age, year, year.term))) # get index of FIRST occurrence within bdf of an i, j, k combination, for each observation in adf
max(idx, na.rm=TRUE) # make sure the index doesn't exceed number of records in bdf - we'll use it to look up in bdf
b <- proc.time()
b - a #
 # This takes extremely long, mainly due to the "paste" operation. 


## left_join
system.time(df2 <
              - left_join(df_wf_term, liab.term)) # 11 secs on office computer.
rm(df2)

## data.table 1
system.time( 
  {x1 <- merge( data.table(df_wf_term, key = "ea,age,year,year.term"),
          data.table(liab.term,  key = "ea,age,year,year.term"),
          by = c("ea", "age","year", "year.term"), all.x = TRUE)})

## data.table 2
system.time( 
  {df_wf_term <- data.table(df_wf_term, key = "ea,age,year,year.term")
   liab.term  <- data.table(liab.term,  key = "ea,age,year,year.term")
   merge(df_wf_term,
         liab.term,
         by = c("ea", "age","year", "year.term"), all.x = TRUE)})


## data.table 3
system.time( 
  {x2 <- merge( data.table(df_wf_term, key = "ea,age,year,year.term"),
                data.table(liab.term),
                by = c("ea", "age","year", "year.term"), all.x = TRUE)})
## data.table 1 and 2 have similar speed.
## It seems that the bigger the data, the greater advantage data.tabel has over dplyr. 



