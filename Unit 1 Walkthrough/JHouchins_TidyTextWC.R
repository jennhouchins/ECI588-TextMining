# ASSIGNMENT DESCRIPTION #####################################
# File:         JHouchins_TidyTextWC.R
# Project:      Unit 1 - Tidy Text and Word Counts 
# Author:       Jennifer Houchins
#
# Purpose:      Use text mining to complement prior qualitative analyses 
#               conducted as part of the RttT Evaluation by examining responses 
#               to open-ended questions on the RttT Online PD Survey 
#               administered to over 15,000 NC educators.
#
# Guiding 
# Questions:    1. What aspects of online professional development
#               offerings do teachers find most valuable?
#               2. How might resources differ in the value they afford 
#               teachers?
#
# Description:  Unit 1 Walkthrough for Tidy Text and Word Counts
#               exploring TT evaluation data. 

# 1 PROJECT SETUP  ################################
if (!require("pacman")) install.packages("pacman")
pacman::p_load(pacman, tidyverse, tidytext, wordcloud2, 
               forcats, remotes, ggplot2)

# these lines produce Pusheen on plots, if you want to try it
# uncomment these and install the ggcats package
# it will ask if you want to do updates and waits for response 
# which is the reason they are commented out for now
# the line of code in the bar plot in the data visualization section of
# the code will also need to be uncommented to add Pusheen to the bar plot
# remotes::install_github("R-CoderDotCom/ggcats@main")
# library(ggcats)

# 2 WRANGLE  ################################

# setting a variable with the file name
# this make it easier to run this script on a
# different file later if applicable

datafile <-"data/opd_survey.csv"
opd_survey <- read_csv(datafile)

# enter the name of your data frame and view directly in console 
opd_survey 

# view your data frame transposed so your can see every column and the first few entries
glimpse(opd_survey) 

# look at just the first six entries
head(opd_survey) 

# or the last six entries
tail(opd_survey) 

# view the names of your variables or columns
names(opd_survey) 

# or view in source pane
view(opd_survey) 

# 2A COMPREHENSION CHECK #######################################
# 1. What argument would you add to read_csv() if my file did not not have 
#    column names or headers? You can type ?read_csv to get help on this 
#    function or check this handy cheatsheet for the readr package from the 
#    readr website at https://readr.tidyverse.org/index.html
#
# R1. The argument I would add to read_csv() if my data file did not have 
#     column names or headers would be the col_names argument. Additionally,
#     I would probably supply a vector of column names to col_names depending
#     on the size of my data file (e.g., this may not be feasible for large data files)
# 
# 2. What is the first argument that read_csv() always expects and what 
#    happens if you don’t include in quotes?
#
# R2. The first argument that read_csv() always expects is a path to a file, 
#     a connection, or literal data in the form of a string or raw vector. If
#     you don't include the data file path in quotes, R checks to see if the argument
#     is a connection and returns an error saying the object is not found. Note that this 
#     is assuming that one hasn't saved the path to a variable as I have done in the code
#     above. (Setting up variables with file paths is a data management habit of mine for 
#     maintaining code that is easier to reuse.)
# 
# 3. What package would I need if I wanted to read a Microsoft Excel file 
#    into R? Hint: Check the cheatsheet.
# 
# R3. You can use readxl package if you want to read
#     Microsoft Excel files into R.
# 
# 4. What are some of the pros and cons of view() compared to other 
#    functions for viewing your data?
#
# R4. Some pros of using view() for viewing your data include being able to 
#     both sort and filter your data for some hands-on exploration. A con 
#     is not being able to see the data types of the columns.
#      
# 
# 5. What happens if you use write_csv(opd_survey, "opd_survey_copy.csv") 
#    and just specify the file name instead including the folder?
#
# R5. A file named opd_survey_copy.csv containing the opd_survey dataframe's 
#     contents will get written in the project's working directory instead 
#     of in the data folder/subdirectory.
# 
# 6. What immediate data wrangling issues do you anticipate we’ll have to 
#    deal with now that you’ve looked at our data?
# 
# R6. Removing the first two rows of Qualtrics extra useless metadata and 
#     omitting the NAs.
# 
  

# 2B REDUCE DATA 

# select only the data we'll use
opd_selected <- select(opd_survey, Role, Q21, Resource)
glimpse(opd_selected)

# rename the Q21 column to some more useful name
opd_renamed <- rename(opd_selected, text = Q21)
glimpse(opd_renamed)

# cleanup Qualtrics craziness
opd_sliced <- slice(opd_renamed, -1, -2)
glimpse(opd_sliced)

# get rid of those pesky NAs and filter by role of "Teacher"
opd_complete <- na.omit(opd_sliced)
opd_teacher <- filter(opd_complete, Role == "Teacher")
glimpse(opd_teacher)

# 2B CODE REDUCTION 

# code reduction is easier on the eyes
opd_teacher <- opd_survey %>%
  select(Role, Resource, Q21) %>%
  rename(text = Q21) %>%
  slice(-1, -2) %>%
  na.omit() %>%
  filter(Role == "Teacher")

glimpse(opd_teacher)

# 2B COMPREHENSION CHECK #######################################

# 1. How would you change the code in the Subset Columns section 
#    if you wanted to analyze how educators are using the online 
#    resources instead of the most beneficial aspects?
#
# R1. I would select the Q20 column for the survey question "How
#     are you using this resource?" so it would look like this:
#
#  opd_selected <- select(opd_survey, Role, Q20, Resource)
#  opd_renamed <- rename(opd_selected, text = Q20)
#
# 2. How would you rewrite the code in the Subset Rows section if 
#    you were interested in looking at School Executive responses 
#    instead of teachers?
#
# R2. I would filter on the Role of "School Executive (e.g. Principal, Assistant Principal)"
#     instead of "Teacher". It would look like this:
#
# opd_teacher <- filter(opd_complete, Role == "School Executive (e.g. Principal, Assistant Principal)")
#
# 3. Rewrite the piped code in the Code Reduction section to prepare 
#    a data frame for tidying that includes all Roles, not just teachers, 
#    but excludes the Resource column. Assign it to opd_benefits for 
#    later use.
#
# R3. see code below for opd_benefits

opd_benefits <- opd_survey %>%
  select(Role, Q21) %>%
  rename(text = Q21) %>%
  slice(-1, -2) %>%
  na.omit()

# 2C TIDY TEXT  ###############################################
opd_tidy <- unnest_tokens(opd_teacher, word, text)

glimpse(opd_tidy)

head(stop_words)
view(stop_words)
opd_clean <- anti_join(opd_tidy, stop_words)
head(opd_clean)

# 2C COMPREHENSION CHECK #######################################
#
# 1. How would you include the anti_join() function in our previous chain 
#    that uses the pipe operator? Give it a try and see what happens.
#
# R1. I would first pipe the unnest_tokens() function and then the anti_join()
#     function. This is shown below:

opd_teacher_testtidy <- opd_survey %>%
  select(Role, Resource, Q21) %>%
  rename(text = Q21) %>%
  slice(-1, -2) %>%
  na.omit() %>%
  filter(Role == "Teacher") %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

# compare opd_clean and my new dataframe/tibble opd_teacher_testtidy to  
# verify the piped tidy results are the same using the dplyr::all_equal() function

df_comparison <- dplyr::all_equal(opd_clean, opd_teacher_testtidy)
if (df_comparison) { print("The piped tidy results are the same.")}

# yay! They are the same results. :)
 
# 2. Why do you think the console provided the message “Joining, by = ‘word’”?
#
# R2. 'word' is the only column the two dataframes have in common to be used in 
#     comparison for the anti_join
#
# 3. How would you use anti_join() if we had named the output column from 
#    unnest_tokens() “tokens” instead? Hint: Check ?anti_join documentation.
# 
# R3. According to the anti_join() documentation:
#     "To join by different variables on x and y, use a named vector. 
#     For example, by = c("a" = "b") will match x$a to y$b."
#     Therefore, you could do the following:
#
#     opd_clean <- anti_join(opd_tidy, stop_words, by = c("tokens" = "word"))
#  
# 4. Tidy your opd_benefits data by tokenizing your text and removing stop words.

opd_benefits_clean <- opd_benefits %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

head(opd_benefits_clean)


# 3 EXPLORE  ###############################################################

# count those words...AH AH AH
opd_counts <- count(opd_clean, word, sort = TRUE)

# alternatively, we could have use the %>% operator to yield the same result.

opd_counts <- opd_clean %>% 
  count(word, sort = TRUE)

opd_counts

# count by resource
opd_resource_counts <- opd_clean %>%
  count(Resource, word, sort = TRUE)

glimpse(opd_resource_counts)

# word frequencies

opd_frequencies <- opd_clean %>%
  count(Resource, word, sort = TRUE) %>%
  group_by(Resource) %>%
  mutate(proportion = n / sum(n))

opd_frequencies

# tf-idf for opd_teacher with word counts by resource

opd_words <- opd_teacher %>%
  unnest_tokens(word, text) %>%
  count(Resource, word, sort = TRUE)

head(opd_words)

# total words by resource type
total_words <- opd_words %>%
  group_by(Resource) %>%
  summarise(total = sum(n))

total_words

opd_totals <- left_join(opd_words, total_words)
opd_totals

opd_tf_idf <- opd_totals %>%
  bind_tf_idf(word, Resource, n)
opd_tf_idf
view(opd_tf_idf)

# 3A COMPREHENSION CHECK  ###########################
#
# 1. Looking back at the Word Counts section, what other aspects of 
#    the online professional development resources to our word counts 
#    suggest teachers find valuable or beneficial?
#
# R1. According to opd_counts, the words most frequently used were information,
#     learning, videos, and resources. This suggests that teachers found what
#     they learned as well as the videos and other resources included in the 
#     online professional development valuable or beneficial.
#
# 2. Instead of using the view() function for opd_resource_counts and 
#    searching in the source how, how might you use the filter() 
#    function to find return the most common words for Recorded 
#    Webinars?
# 
# R2. See the following code using the filter() function:

opd_resource_counts_webinars <- opd_clean %>%
  filter(Resource == "Recorded Webinar or Presentation (e.g. Strategic Staffing, Standards and Assessment)") %>%
  count(Resource, word, sort = TRUE)
view(opd_resource_counts_webinars)

#
# 3. How many total resources were actually evaluated and which 
#    resource received the most feedback? How do you know?
# 
# R3. Using the total_words dataframe, there are 10 total resources evaluated
#     and the Online Learning Module resource received the most feedback
#     because it has the highest count of words.
#
# 4. What are some obvious limitations to tf-idf, at least for this 
#    dataset, based on the initial opd_tf_idf data frame we created?
#
# R4. According to the Text Mining with R text, tf-idf's function is to identify
#     words that are important to one text within a collection. In the case of 
#     this dataset, the words that we may consider important (for a resource)  
#     are used frequently across the collection which leads to a low tf-idf 
#     value much like the stop words.
#
# 5. Calculate word counts, frequencies, and tf-idf for your opd_benefits 
#    data frame. For frequencies and tf-idf, group by Role instead of Resource.
#
# R5. See the code below:


opd_benefits_counts <- opd_benefits_clean %>% 
  count(word, sort = TRUE)


# count by resource
opd_resource_benefits_counts <- opd_benefits_clean %>%
  count(Role, word, sort = TRUE)

glimpse(opd_resource_benefits_counts)

# word frequencies

opd_benefits_frequencies <- opd_benefits_clean %>%
  count(Role, word, sort = TRUE) %>%
  group_by(Role) %>%
  mutate(proportion = n / sum(n))

opd_benefits_frequencies

# tf-idf for opd_benefits with word counts by role

opd_benefits_words <- opd_benefits %>%
  unnest_tokens(word, text) %>%
  count(Role, word, sort = TRUE)

head(opd_benefits_words)

# total words by role
total_benefits_words <- opd_benefits_words %>%
  group_by(Role) %>%
  summarise(total = sum(n))

total_benefits_words

opd_benefits_totals <- left_join(opd_benefits_words, total_benefits_words)
opd_benefits_totals

opd_benefits_tf_idf <- opd_benefits_totals %>%
  bind_tf_idf(word, Role, n)
opd_benefits_tf_idf
view(opd_benefits_tf_idf)

#
# 6. What differences, if any, do you notice between teachers and other roles?
# 
# R6. Word count totals are higher for teachers than for other roles.

# 3b Word Search

opd_quotes <- opd_teacher %>%
  select(text) %>% 
  filter(grepl('online', text))

view(opd_quotes)

sample_n(opd_quotes, 20)

# word stem example
opd_quotes <- opd_teacher %>%
  select(text) %>% 
  filter(grepl('inform*', text))

view(opd_quotes)

sample_n(opd_quotes, 20)

# 3C Data Visualization

wordcloud2(opd_counts)

opd_counts %>%
  filter(n > 500) %>% # keep rows with word counts greater than 500
  mutate(word = reorder(word, n)) %>% #reorder the word variable by n and replace with new variable called word
  ggplot(aes(n, word)) + # create a plot with n on x axis and word on y axis
  geom_col() #+ uncomment the plus and the line below to add Pusheen to the bar plot # make it a bar plot
  #geom_cat(aes(n, word), cat="pusheen", size=3) 


opd_frequencies %>%
  filter(Resource != "Calendar") %>% # remove Calendar responses, too few. 
  group_by(Resource) %>%
  slice_max(proportion, n = 5) %>%
  ungroup() %>%
  ggplot(aes(proportion, fct_reorder(word, proportion), fill = Resource)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~Resource, ncol = 3, scales = "free")

# 3C COMPREHENSION CHECK ###############################################
#
# 1. Create a word cloud, bar chart, and/or small multiple using your 
#    opd_benefits data.
#
# R1. See code below for opd_benefits_counts word cloud

wordcloud2(opd_benefits_counts)
