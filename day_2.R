

# installing the dslabs packages
# packages are installed once in a hardware
install.packages ("dslabs")


#loading the dslab package in every session in RStudio memory
library(dslabs)

#load the murder dataset into active memory dssasdsasdsdaasddsa
data(murders)
View(murders)


# to view the first 6 rows
head(murders)


# name of the state with max population
murders $ state [which.max (murders $ population)]



# murder rate
murder_rate <- murders $ total/murders $ population * 100000


murder_rate



# Ordering the state by murder rate, in decreasing order
murders $ state [order(murder_rate, decreasing = TRUE)]


# Maximum population
max (murders $ population)


index <- murder_rate < 0.71

index



murders $ state [index]


sum(index)



west <- murders $ region == "West"
safe <- murder_rate <= 1
ind <- safe & west

ind


murders $ state [ind]



# which() gives us the entries 
# of logical vector that are True


# match() looks for entries in a vector
# and returns the index needed to 
# access them


# %in% if we want to know whether or 
# not each element of a first vector 
# is in a second vector



# to determine the murder rate in 
# Massachusetts, we may do the 
# following:
ind <- which (murders$state == "Massachusetts")


ind


murder_rate [ind]


# to obtain the indices and 
# subsequent murder rates of New York, 
# Florida, and Texas, we do:
ind <- match (c("New York", "Florida", 
                "Texas"),murders$state)

ind

murder_rate[ind]


# to see if Boston, Dakota, and 
# Washington are states:

c("Boston","Dakota","Washington") %in% murders$state




