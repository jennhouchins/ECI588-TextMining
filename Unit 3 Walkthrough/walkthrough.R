# ASSIGNMENT DESCRIPTION #####################################
# File:         walkthrough.R
# Project:      Unit 3 Walkthrough
# Author:       Jennifer Houchins
#
# Purpose:      Use R for topic modeling of MOOC-Ed discussion forums
#
# Guiding 
# Questions:    How do we to quantify what a document or collection of 
#               documents is about?
# 

# 1 PROJECT SETUP  ################################
if (!require("pacman")) install.packages("pacman")
pacman::p_load(pacman, tidyverse, tidytext, topicmodels, 
               stm, LDAvis, SnowballC, ldatuning,knitr)

# Import Data #########

# ts_forum_data <- read_csv("data/ts_forum_data.csv", 
#                           col_types = cols(course_id = col_character(),
#                                            forum_id = col_character(), 
#                                            discussion_id = col_character(), 
#                                            post_id = col_character()
#                           )
# )

# COMPREHENSION CHECK ################

# Try importing directly from the ECI 588 Github repository. 
# The data for Unit 3 is located in this folder: 
# https://github.com/sbkellogg/eci-588/tree/main/unit-3/data
# 
# Hint: Check the examples from the ?read_csv help file.

# Had to view the raw file and use that link to download directly from Github, it would not
# work any other way. And even though this worked, the data has less obs than the one
# I downloaded from Moodle.
ts_forum_data2 <- read_csv("https://raw.githubusercontent.com/sbkellogg/eci-588/main/unit-3/data/ts_forum_data.csv", 
                           col_types = cols(course_id = col_character(),
                                            forum_id = col_character(), 
                                            discussion_id = col_character(), 
                                            post_id = col_character()
                           ))

# write.csv(ts_forum_data2, "data/ts_forum_data_fromGH.csv")

# DOCUMENT TERM MATRIX ##############

forums_tidy <- ts_forum_data2 %>%
  unnest_tokens(output = word, input = post_content) %>%
  anti_join(stop_words, by = "word")

forums_tidy

# WORD COUNT
forums_counts <- forums_tidy %>%
  count(word, sort = TRUE)

# COMPREHENSION CHECK ################
# Use the filter() and grepl() functions introduced in Unit 1. Section 3b to 
# filter for rows in our ts_forum_data data frame that contain the terms 
# “agree” and “time” and another term or terms of your choosing. Select a 
# random sample of 10 posts using the sample_n() function for your terms and 
# answer the following questions:
#   

agree_quotes <- ts_forum_data2 %>%
  select(post_content) %>% 
  filter(grepl('agree', post_content))

time_quotes <- ts_forum_data2 %>%
  select(post_content) %>% 
  filter(grepl('time', post_content))

teaching_quotes <- ts_forum_data2 %>%
  select(post_content) %>% 
  filter(grepl('teaching', post_content))

# view(agree_quotes)
# view(time_quotes)
# view(teaching_quotes)

sample_n(agree_quotes, 20)
sample_n(time_quotes, 20)
sample_n(teaching_quotes, 20)

#   1. What, if anything, do these posts have in common?

#   A1. After a little inspection of the sample quotes, it looks like the posts
#       have a common theme of issues surrounding lessons/activities such as
#       student understanding, timing, or teaching practice.

#   2. What topics or themes might be apparent, or do you anticipate emerging, 
#   from our topic modeling?

#   A2. From my choice of the term teaching, I see a trend of resources and technology  
#   as potential topics that I anticipate emerging. However, looking at the time 
#   and agree quotes, it seems that potential emergent topics might be particular 
#   lessons or activities and the timing associated with them.


forums_dtm <- forums_tidy %>%
  count(post_id, word) %>%
  cast_dtm(post_id, word, n)

# COMPREHENSION CHECK ################

# Take a look at our forums_dtm object in the console and answer the 
# following questions:
#   
#   1. What “class” of object is forums_dtm?

#   A1. forums_dtm is an object of class Large DocumentTermMatrix

#   2. How many unique documents and terms are included our matrix?

#   A2. There are 5,761 documents and 13,666 terms included in our matrix.

#   3. Why might there be fewer documents/posts than were in our original data frame?

#   A3. Some posts may have been removed if they only contained stop words. A cursory
#       exploration of the origin data frame confirms that some posts consist only of a 
#       single word or a number and these may have been "tidied" up from the data.

#   4. What exactly is meant by “sparsity”?

#   A4. Sparsity is a term that means there is not a value for every term in 
#       every document. Since some terms may only occur in a few documents, 
#       the matrix ends up being sparsely populated with values.

# TO STEM OR NOT TO STEM ################

temp <- textProcessor(ts_forum_data2$post_content, 
                      metadata = ts_forum_data2,  
                      lowercase=TRUE, 
                      removestopwords=TRUE, 
                      removenumbers=TRUE,  
                      removepunctuation=TRUE, 
                      wordLengths=c(3,Inf),
                      stem=TRUE,
                      onlycharacter= FALSE, 
                      striphtml=TRUE, 
                      customstopwords=NULL)

meta <- temp$meta
vocab <- temp$vocab
docs <- temp$documents

stemmed_forums <- ts_forum_data2 %>%
  unnest_tokens(output = word, input = post_content) %>%
  anti_join(stop_words, by = "word") %>%
  mutate(stem = wordStem(word))

stemmed_forums

# COMPREHENSION CHECK ################
# 
# Complete the following code using what we learned in the section on 
# Creating a Document Term Matrix and answer the following questions:
#   
#   1. How many fewer terms are in our stemmed document term matrix?
# 
#   A1. It has 3,606 fewer terms.
# 
#   2. Did stemming words significantly reduce the sparsity of the network?
# 
#   A2. No. The new stemmed dtm is just as sparse as the non-stemmed dtm.
#   
# Hint: Make sure your code includes stem counts rather than word counts.

stemmed_dtm <- ts_forum_data2 %>%
  unnest_tokens(output = word, input = post_content) %>%
  anti_join(stop_words, by = "word") %>%
  mutate(stem = wordStem(word)) %>%
  count(post_id, stem) %>%
  cast_dtm(post_id, stem, n)

  stemmed_dtm

# MODEL ###############################

n_distinct(ts_forum_data2$forum_name)

forums_lda <- LDA(forums_dtm, 
                  k = 20, 
                  control = list(seed = 588) # <- I see what you did there! :)
)

forums_lda

# stm Package

forums_stm <- stm(documents=docs, 
                  data=meta,
                  vocab=vocab, 
                  prevalence =~ course_id + forum_id,
                  K=20,
                  max.em.its=25,
                  verbose = FALSE)

forums_stm

plot.STM(forums_stm, n = 5)

# COMPREHENSION CHECK ################

# Fit a model for both LDA and STM using different values 
# for K and answer the following questions:
#   
#   1. What topics appear to be similar to those using 20 topics for K?

#   A1. Topics that consist of terms like Statist, student, think, learn, 
#       question, answer, understand. Please refer to topicplot-k20.png and
#       topicplot-k10.png or topicplot-k35.png in the Unit 3 Walkthrough plots directory.

#   2. Knowing that you don’t have as much context as I do, how might 
#   you interpret one of these latent topics or themes using the key 
#   terms assigned?

#   A2. Referring to my k=10 topic plot, I might interpret the topic consisting of terms
#       question, can, differ, answer, mean to be a topic addressing student misconceptions 
#       or one consisting of terms resourc, technolog, share, mani, classroom to be a
#       topic addressing the types of technology resources or manipulatives that could be
#       used in the classroom for teaching statistics.

#   3. What topic emerged that seem dramatically different and how might you 
#      interpret this topic?

#   A3. Topic 6 on the k=20 topic plot consists of terms coaster, roller, steel, speed, and present.
#       I might interpret this topic as something to do with a particular statistics activity
#       with an application or theme of roller coasters.

new_lda <- LDA(forums_dtm, 
                  k = 10, 
                  control = list(seed = 42) 
)


forums_stm <- stm(documents=docs, 
                  data=meta,
                  vocab=vocab, 
                  prevalence =~ course_id + forum_id,
                  K=10,
                  max.em.its=25,
                  verbose = FALSE)

forums_stm

plot.STM(forums_stm, n = 5)

# FINDING K #############################

k_metrics <- FindTopicsNumber(
  forums_dtm,
  topics = seq(10, 75, by = 5),
  metrics = "Griffiths2004",
  method = "Gibbs",
  control = list(),
  mc.cores = NA,
  return_models = FALSE,
  verbose = FALSE,
  libpath = NULL
)

FindTopicsNumber_plot(k_metrics)

findingk <- searchK(docs, 
                    vocab, 
                    K = c(5:15),
                    data = meta, 
                    verbose=TRUE)

plot(findingk)

# THE LDAVIS EXPLORER ############################

toLDAvis(mod = forums_stm, docs = docs)

# EXPLORE ########################################

terms(forums_lda, 5)

tidy_lda <- tidy(forums_lda)

tidy_lda

top_terms <- tidy_lda %>%
  group_by(topic) %>%
  slice_max(beta, n = 5, with_ties = FALSE) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  group_by(topic, term) %>%    
  arrange(desc(beta)) %>%  
  ungroup() %>%
  ggplot(aes(beta, term, fill = as.factor(topic))) +
  geom_col(show.legend = FALSE) +
  scale_y_reordered() +
  labs(title = "Top 5 terms in each LDA topic",
       x = expression(beta), y = NULL) +
  facet_wrap(~ topic, ncol = 4, scales = "free")

# Exploring Gamma Values

td_beta <- tidy(forums_lda)

td_gamma <- tidy(forums_lda, matrix = "gamma")

td_beta

td_gamma

top_terms <- td_beta %>%
  arrange(beta) %>%
  group_by(topic) %>%
  top_n(7, beta) %>%
  arrange(-beta) %>%
  select(topic, term) %>%
  summarise(terms = list(term)) %>%
  mutate(terms = map(terms, paste, collapse = ", ")) %>% 
  unnest()

gamma_terms <- td_gamma %>%
  group_by(topic) %>%
  summarise(gamma = mean(gamma)) %>%
  arrange(desc(gamma)) %>%
  left_join(top_terms, by = "topic") %>%
  mutate(topic = paste0("Topic ", topic),
         topic = reorder(topic, gamma))

gamma_terms %>%
  select(topic, gamma, terms) %>%
  kable(digits = 3, 
        col.names = c("Topic", "Expected topic proportion", "Top 7 terms"))

plot(forums_stm, n = 7)

# READING TEA LEAVES #########################

ts_forum_data_reduced <-ts_forum_data2$post_content[-temp$docs.removed]

findThoughts(forums_stm,
             texts = ts_forum_data_reduced,
             topics = 2, 
             n = 10,
             thresh = 0.5)

findThoughts(forums_stm,
             texts = ts_forum_data_reduced,
             topics = 10, 
             n = 10,
             thresh = 0.5)

ts_forum_data_reduced <-ts_forum_data2$post_content[-temp$docs.removed]

findThoughts(forums_stm,
             texts = ts_forum_data_reduced,
             topics = 3, 
             n = 10,
             thresh = 0.5)

# COMPREHENSION CHECK ################
# Using the STM model you fit from the Section 3 [Comprehension Check] 
# with a different value for K, use the approaches demonstrated in Section 4 
# to explore and interpret your topics and terms and revisit the following 
# question:
#   
#   Now that you have a little more context, how might you revise your 
#   initial interpretation of some of the latent topics or latent themes 
#   from your model?

#   I'm not sure I would revise my initial interpretation of some of the latent
#   topics or themes. Perhaps that's because I ran through the entire analysis before
#   answering the comprehension checks and my responses align accordingly...

#  I also provided screenshots in my plots directory of the LDAvis explorer for both the 
#  k = 20 and k = 10. It would seem that while k = 20 is an optimal value for k, using 
#  a smaller k (k=10) results in topics with much less overlap. Though, I'm not sure
#  I would consider overlap such a bad thing in this case as we may wish to see
#  participants in the MOOC-Ed making connections across discussion posts.

