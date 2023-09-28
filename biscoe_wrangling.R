library(dplyr)
biscoe_dat <- read.csv("https://github.com/cct-datascience/repro-data-sci/raw/r-lessons/lessons/7-intermediate-r-1/lesson-data/Biscoe.csv")
head(biscoe_dat)
# to see if there is any NA data, it will say TRUE or FALSE
anyNA(biscoe_dat)
# to remove ther NA data
biscoe_dat <- biscoe_dat |> 
  na.omit()
# when we need mean for single columns based on some variables
biscoe_dat_means <- biscoe_dat |>
  group_by(species, sex) |>
  summarize(mean_bill_length = mean(bill_length_mm))

# when we need mean for several columns
biscoe_dat_means <- biscoe_dat |>
  group_by(species, sex) |>
  summarize(mean_bill_length = mean(bill_length_mm),
            mean_bill_depth= mean(bill_depth_mm),
            mean_flipper_length=mean(flipper_length_mm))

# instaead of writing this all, we can use across

biscoe_dat_means1 <- biscoe_dat |>
  group_by(species, sex) |>
  summarize(across(ends_with("mm"), mean))

# if ends_with is different for different columns, we have to use "|" as shown below

biscoe_dat_means2 <- biscoe_dat |>
  group_by(species, sex) |>
  summarize(across(ends_with("mm") | ends_with("g"), mean))
# if we want to convert mm to inches, use mutate as shown below
# *0,03937008 to convert it to inches
# "{.col}_in" is to keep the original column names and then add "_in" to the names
biscoe_dat_imperial <- biscoe_dat |>
  group_by(species, sex) |>
  mutate(across(ends_with("mm"), ~ . * 0.03937008, .names = "{.col}_in" ))
# We can change the names with stringr::str_replace
library(stringr)
biscoe_dat_imperial <- biscoe_dat_imperial |>
  rename_with(~stringr::str_replace(., "mm_in", "in"), .cols = ends_with("mm_in")) |>
  rename_with(~stringr::str_replace(., "g_lb", "lb"), .cols = ends_with("g_lb"))

# to pull out few columns where we want to get the character columns with where(is.character)
# and select columns that ends_with our desirable column ends
biscoe_dat_imperial1 <- biscoe_dat_imperial |>
  select(c(where(is.character) |
           ends_with("in") |
           ends_with("lb")))
# "filter" can be used for rows
# to create a function
# content within {}, and return as output
# paste0 removes spaces
my_fucntion <- function(){
  return("I need coffee!")
}
# function() need an argument
my_fucntion <- function(favorite_beverage ="coffee"){
  what_to_say <- paste0("I need", favorite_beverage, "!")
  return(what_to_say)
}

my_fucntion()

# function for read.csv
my_fucntion <- function(data_url ="https://github.com/cct-datascience/repro-data-sci/raw/r-lessons/lessons/7-intermediate-r-1/lesson-data/Biscoe.csv") {
 island_dat <- read.csv(data_url)
  return(island_dat)
}

my_fucntion()
# remove the NA
my_fucntion <- function(data_url ="https://github.com/cct-datascience/repro-data-sci/raw/r-lessons/lessons/7-intermediate-r-1/lesson-data/Biscoe.csv") {
  island_dat <- read.csv(data_url)
  
  island_dat <- island_dat |>
    na.omit()
  return(island_dat)
}
my_fucntion()
function_output <- my_fucntion()

# if we have to write the whole above codes into a single function
my_fucntion()
# remove the NA
my_fucntion <- function(data_url ="https://github.com/cct-datascience/repro-data-sci/raw/r-lessons/lessons/7-intermediate-r-1/lesson-data/Biscoe.csv") {
  island_dat <- read.csv(data_url)
  
  island_dat <- island_dat |>
    na.omit()
  
  # if ends_with is different for different columns, we have to use "|" as shown below
  
  biscoe_dat_means2 <- island_dat |>
    group_by(species, sex) |>
    summarize(across(ends_with("mm") | ends_with("g"), mean))
  # if we want to convert mm to inches, use mutate as shown below
  # *0,03937008 to convert it to inches
  # "{.col}_in" is to keep the original column names and then add "_in" to the names
  biscoe_dat_imperial <- biscoe_dat_means2 |>
    group_by(species, sex) |>
    mutate(across(ends_with("mm"), ~ . * 0.03937008, .names = "{.col}_in" ),
           across(ends_with("g"), ~ . * 0.002204623, .names = "{.col}_lb"))
  # We can change the names with stringr::str_replace
  library(stringr)
  biscoe_dat_imperial <- biscoe_dat_imperial |>
    rename_with(~stringr::str_replace(., "mm_in", "in"), .cols = ends_with("mm_in")) |>
    rename_with(~stringr::str_replace(., "g_lb", "lb"), .cols = ends_with("g_lb"))
  
  # to pull out few columns where we want to get the character columns with where(is.character)
  # and select columns that ends_with our desirable column ends
  biscoe_dat_imperial1 <- biscoe_dat_imperial |>
    select(c(where(is.character) |
               ends_with("in") |
               ends_with("lb")))
  return(biscoe_dat_imperial1)
}
my_fucntion()
function_output <- my_fucntion()
# then this function can be applied to other data set as well
torgersen_output <- my_fucntion("https://github.com/cct-datascience/repro-data-sci/raw/r-lessons/lessons/7-intermediate-r-1/lesson-data/Torgersen.csv")
dream_output <- my_fucntion("https://github.com/cct-datascience/repro-data-sci/raw/r-lessons/lessons/7-intermediate-r-1/lesson-data/Dream.csv")

# "|" is for "or" command, we can also use "," in place of |
# conditionals to filter data
# instead of data url , we can use conditionals "if" and "else if" and "else"
# 
my_fucntion <- function(island_to_use ="Biscoe") {
  
  if(island_to_use== "Biscoe"){
    data_url <- "https://github.com/cct-datascience/repro-data-sci/raw/r-lessons/lessons/7-intermediate-r-1/lesson-data/Biscoe.csv"
  } else if(island_to_use =="Dream"){
    data_url <- "https://github.com/cct-datascience/repro-data-sci/raw/r-lessons/lessons/7-intermediate-r-1/lesson-data/Dream.csv"
  } else if (island_to_use == "Torgersen"){
    data_url <- "https://github.com/cct-datascience/repro-data-sci/raw/r-lessons/lessons/7-intermediate-r-1/lesson-data/Torgersen.csv"
  } else {
    stop("The island to use doesn't exist")
  }
  
  island_dat <- read.csv(data_url)
  
  island_dat <- island_dat |>
    na.omit()
  
  # if ends_with is different for different columns, we have to use "|" as shown below
  
  biscoe_dat_means2 <- island_dat |>
    group_by(species, sex) |>
    summarize(across(ends_with("mm") | ends_with("g"), mean))
  # if we want to convert mm to inches, use mutate as shown below
  # *0,03937008 to convert it to inches
  # "{.col}_in" is to keep the original column names and then add "_in" to the names
  biscoe_dat_imperial <- biscoe_dat_means2 |>
    group_by(species, sex) |>
    mutate(across(ends_with("mm"), ~ . * 0.03937008, .names = "{.col}_in" ),
           across(ends_with("g"), ~ . * 0.002204623, .names = "{.col}_lb"))
  # We can change the names with stringr::str_replace
  library(stringr)
  biscoe_dat_imperial <- biscoe_dat_imperial |>
    rename_with(~stringr::str_replace(., "mm_in", "in"), .cols = ends_with("mm_in")) |>
    rename_with(~stringr::str_replace(., "g_lb", "lb"), .cols = ends_with("g_lb"))
  
  # to pull out few columns where we want to get the character columns with where(is.character)
  # and select columns that ends_with our desirable column ends
  biscoe_dat_imperial1 <- biscoe_dat_imperial |>
    select(c(where(is.character) |
               ends_with("in") |
               ends_with("lb")))
  return(biscoe_dat_imperial1)
}

biscoe_output <- my_fucntion
biscoe_output <- my_fucntion(island_to_use = "Biscoe")
torgersen_output<- my_fucntion(island_to_use = "Torgersen")
dream_output <- my_fucntion(island_to_use = "Dream")

# iteration with "for" loops


for (i in 1:3) {
  print(i)
}

# run my fucntion() on each island (Biscoe, Torgersen, Dream)
islands_we_want <- c("Biscoe", "Torgersen", "Dream")
for (i in 1:3) {
  print(islands_we_want[i])
}
# create a empty list first to store the data
cleaned_data <- list()
for (i in 1:3) {
  cleaned_data[[i]] <- my_fucntion(islands_we_want[i])
}
# to get the data as a dataframe
names(cleaned_data) <- islands_we_want
cleaned_data_df <- bind_rows(cleaned_data, .id = "island")



