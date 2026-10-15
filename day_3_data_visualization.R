install.packages("dplyr")

#load package
library(dslabs)
library(dplyr)

View("murders")

# adding a column with mutate
data("murders")
murders <- mutate (murders, rate=total/population * 100000)

View("murders")


#subsetting with 'filter' Keep in mind filter always will be in rows not columns
filter(murders, rate <= 0.71)



# selecting columns with 'select':
new_table <- select (murders, state, region, rate)
View (new_table)



filter (new_table, rate <= 0.71)

#Use of pipe operator (%>%) or >|
# Pipe operator short the long codes and make easy for us
# shortcut of pipe operator Ctrl+Shft+M
murders %>% select(state, region, rate) %>% filter (rate <= 0.71)



# Concept of Pipe Operator
object <- c(34, 45, 70)
object

#without pipe operator
bake (mix(add_eggs(add_flour(bowl))))

# with pipe operator
# |> shows add and pass through 
bowl |> add_flour() |> add_eggs |> mix() |> bake



# Matching operator %in%
my_state <- filter (murders, region %in% c("Northeast", "West") & rate < 1)
my_state

View(my_state)
