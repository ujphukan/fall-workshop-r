library(dplyr)
library(tidyr)
library(palmerpenguins)
penguins_data <- penguins

# class() can be used to know the type of data we have
class(penguins_data)
# head() to look at the first few lines
head(penguins_data)
# tibble and data frame looks similar, however tibble shows you the class of data in each variable or column
# str() tells about structure of the data
str(penguins_data)
# it shows some of the columns has "Factor" with levels, if you want to see how it is ordered, use unique() as below
unique(penguins_data$species)
# NA is used to encode missing data/value in R to treat it that way
# a column can be treated as a vector
mean(penguins_data$body_mass_g)
# it would give mean as "NA" since it has some "NA" values, it needs to be treated as follows
mean(penguins_data$body_mass_g, na.rm = T)
# na.rm =TRUE will remove the NA values
# pull out columns from dataframe using dplyr, use select(data, column1, column2)

island_year <-select(penguins_data, island, year)
# to pull out rows, use filter(data, column name == "query rows")
torgersen_penguins <- filter(penguins_data, island == "Torgersen")
# to pull out columns again
torgersen_penguins_only_sex_and_species <- select(torgersen_penguins, sex, species)
# to do this througha continuous pipe at once, as shown below
# "%>%" is pipe function for tidyverse, "|>" is symbol in basic R
torgereson_penguins_one_chunk <- filter(penguins_data, island=="Torgersen") |>
  select(sex, species)

# to create a new column in a data frame, use mutate
torgersen_penguins <- torgersen_penguins |>
  mutate(rounded_bill_length = round(bill_length_mm)) |>
  select(species, sex, rounded_bill_length)

# for summary statistics, "summary"
# group_by is used to take into account only the thing that we are interested in
 torgersen_penguins_summary <- torgersen_penguins |>
   group_by(species) |>
   summarise(mean_bill_length = mean(rounded_bill_length, na.rm = T))

 torgersen_penguins_summary <- torgersen_penguins |>
   group_by(species, sex) |>
   summarise(mean_bill_length = mean(rounded_bill_length, na.rm = T))

# to get the counts
 penguin_counts <- penguins_data |>
   group_by(species, sex, island) |>
   summarize(n = dplyr::n())

# pivot_wider() "widens" data, increasing the number of columns and decreasing the number of rows. 
# The inverse transformation is pivot_longer()
 
 penguins_wide <- penguin_counts |>
   pivot_wider(names_from = island, values_from = n, values_fill = 0)
 
 penguins_long_again <- penguins_wide |>
   pivot_longer(-c(species, sex), values_to = "count")
# lesson 7:
# "unite" is the function to combine multiple columns in a dataframe, remove = F will keep the original columns, if not it will replace the original columns with new merged column
# to delete a branch "git branch -d data-manip"
# new branch - "functions"
 # 