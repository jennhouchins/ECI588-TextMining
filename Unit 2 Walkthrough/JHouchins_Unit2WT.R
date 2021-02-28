# ASSIGNMENT DESCRIPTION #####################################
# File:         JHouchins_Unit2WT.R
# Project:      Unit 2 Walkthrough
# Author:       Jennifer Houchins
#
# Purpose:      
#
# Guiding 
# Questions:    1. What is the public sentiment expressed toward the NGSS?
#               2. How does sentiment for NGSS compare to sentiment for CCSS?
#
# Description:  Simple "replication study" comparing the sentiment 
#               analysis of tweets about NGSS and Common Core SS
#               to better understand public reaction to these two curriculum 
#               reform efforts.

# 1 PROJECT SETUP  ################################
if (!require("pacman")) install.packages("pacman")
pacman::p_load(pacman, dplyr, readr, tidyr, rtweet, writexl,
               readxl, tidytext, textdata, ggplot2, textdata, scales,
               ggcats)

## check to see if the Twitter token is loaded
get_token()

# 2 WRANGLE  ################################

ngss_all_tweets <- search_tweets(q = "#NGSSchat", n=5000)

# 2a: 1st COMPREHENSION CHECK ######################

# 1. How many tweets did our query using the Twitter API actually return? How many
#    variables?
#   A1. Our query returned 210 tweets with 90 variables.
#
# 2. Why do you think our query pulled in far less than 5000 tweets requested?
#   A2. The query only looked for tweets that included the hashtag #NGSSchat. It is 
#       possible that some tweets deal with the topic of interest but include 
#       other hashtags (i.e., #NGSS) or no hashtags at all.
#
# 3. Does our query also include retweets? How do you know?

?search_tweets

#   A3. Yes because the include_rts option of the search_tweets function is TRUE by
#       default. (See the results of the help functionality for search_tweets) 

ngss_non_retweets <- search_tweets("#NGSSchat", 
                                   n=5000, 
                                   include_rts = FALSE)

ngss_or_tweets <- search_tweets(q = "#NGSSchat OR ngss", 
                                n=5000,
                                include_rts = FALSE)

# 2a: 2nd COMPREHENSION CHECK ######################
# 
# 1. Does excluding the OR operator return more tweets, the same number of 
#    tweets, or fewer tweets? Why?
#      A1. Excluding the OR operator returns fewer tweets because it narrows 
#          the search to tweets that only include the #NGSSchat hashtag.
# 2. What other useful arguments does the search_tweet() function contain? 
#    Try adding one and see what happens.
#     A2. The geocode arguments allows you to specify a geographic limiter for your
#         search. That is, you could limit your search to tweets posted within a 
#         fifty mile radius of a particular school for example. See below:

ngss_ncsu_tweets <- search_tweets(q = "#NGSSchat OR ngss", 
                                  n=5000,
                                  include_rts = FALSE,
                                  geocode = "35.7596,79.0193,5000mi") 

# Had to use a 5000 mi radius from NC State get 5 tweets...Really? No one is 
# tweeting about NGSS in NC? wow

# Use Multiple Queries

ngss_tweets <- search_tweets2(c("#NGSSchat OR ngss",
                                '"next generation science standard"',
                                '"next generation science standards"',
                                '"next gen science standard"',
                                '"next gen science standards"'
                                ), 
                              n=5000,
                              include_rts = FALSE)

# Our First Dictionary

ngss_dictionary <- c("#NGSSchat OR ngss",
                     '"next generation science standard"',
                     '"next generation science standards"',
                     '"next gen science standard"',
                     '"next gen science standards"')

ngss_tweets <- search_tweets2(ngss_dictionary,
                              n=5000,
                              include_rts = FALSE)

# Common core dictionary and tweets

ccss_dictionary <- c("#commoncore", '"common core"')

ccss_tweets <- ccss_dictionary %>% 
  search_tweets2(n=5000, include_rts = FALSE)
     
# 2a: 3rd COMPREHENSION CHECK ######################

# 1. Use the search_tweets function to create you own custom query for a 
#    twitter hashtag or topic(s) of interest.

csedu_dictionary <- c("#CSforAll", 
                      "#csforall", 
                      "#CSed",
                      "#inclusiveCS",
                      "#digitalliteracy")

csedu_tweets <- csedu_dictionary %>%
  search_tweets2(n=5000, include_rts = FALSE)

# Write to Excel (Excel?! sad face...)

write_xlsx(ngss_tweets, "data/ngss_tweets.xlsx")
write_xlsx(ccss_tweets, "data/csss_tweets.xlsx")

# Running other useful queries

fi <- c("sbkellogg", "mjsamberg", "haspires", "tarheel93", "drcallie_tweets", "AlexDreier")

fi_tweets <- fi %>%
  get_timelines(include_rts=FALSE)

# Hey! I'm in this ^ data. :D

sample_n(fi_tweets, 10) %>%
  select(screen_name, text)

vignette("intro", package="rtweet")

# 2a: 4th COMPREHENSION CHECK ######################

# get_timelines() Get the most recent 3,200 tweets from users.
# stream_tweets() Randomly sample (approximately 1%) from the live stream of all tweets.
# get_friends() Retrieve a list of all the accounts a user follows.
# get_followers() Retrieve a list of the accounts following a user.
# get_favorites() Get the most recently favorited statuses by a user.
# get_trends() Discover what’s currently trending in a city.
# search_users() Search for 1,000 users with the specific hashtag in their profile bios.

myfollowers <- get_followers("TooSweetGeek", n=5000) # haha, jk...I don't have that many followers XD
myflw_data <- lookup_users(myfollowers$user_id) # well that's fun

# Tidy Text

# Don't really need to read these because I got them earlier and still loaded
# ngss_tweets <- read_xlsx("data/ngss_tweets.xlsx")
# ccss_tweets <- read_xlsx("data/csss_tweets.xlsx")

ngss_text <- filter(ngss_tweets, lang == "en")
ngss_text <- select(ngss_text,screen_name, created_at, text)
ngss_text <- mutate(ngss_text, standards = "ngss")
ngss_text <- relocate(ngss_text, standards)
ngss_text <- select(ngss_text, standards, screen_name, created_at, text)

ngss_text <-
  ngss_tweets %>%
  filter(lang == "en") %>%
  select(screen_name, created_at, text) %>%
  mutate(standards = "ngss") %>%
  relocate(standards)

# 2b: 1st COMPREHENSION CHECK ######################

# 1. Create an new ccss_text data frame for our ccss_tweets Common Core tweets 
#    by modifying code above.

ccss_text <-
  ccss_tweets %>%
  filter(lang == "en") %>%
  select(screen_name, created_at, text) %>%
  mutate(standards = "ccss") %>%
  relocate(standards)

# Combine Data Frames

tweets <- bind_rows(ngss_text, ccss_text)
head(tweets)
tail(tweets)

# Tokenize text

tweet_tokens <- tweets %>%
  unnest_tokens(output = word, 
                input = text, 
                token = "tweets")

# Remove stopwords

tidy_tweets <- tweet_tokens %>%
  anti_join(stop_words, by = "word")

# Custom stopwords
count(tidy_tweets, word, sort = T)
filter(tweets, grepl('amp', text))

tidy_tweets <- tweet_tokens %>%
  anti_join(stop_words, by = "word") %>%
  filter(!word == "amp")

# 2b: 2nd COMPREHENSION CHECK ######################

# Rewrite the tokenization and removal of stop words processes into a more
# compact series of commands and save your data frame as tidy_tweets.

# tidy-er code for tidy_tweets

tidy_tweets <- bind_rows(ngss_text, ccss_text) %>% 
  unnest_tokens(output = word, 
                input = text, 
                token = "tweets") %>% 
  anti_join(stop_words, by = "word") %>%
  filter(!word == "amp")
  
# Get Sentiments (grab those lexicons)

afinn <- get_sentiments("afinn")
afinn

bing <- get_sentiments("bing")
bing

nrc <- get_sentiments("nrc")
nrc

loughran <- get_sentiments("loughran")
loughran

# 2c: 1st COMPREHENSION CHECK ######################
# 1. How were these sentiment lexicons put together and validated? 
#    Hint: take a look at Chapter 2 from Text Mining with R.
#   A1. The lexicons were put together either by author efforts or by crowdsourcing
#       through avenues like Amazon's Mechanical Turk and validated via more
#       crowdsourcing or comparison with other data such as reviews or tweets.
#   
# 2. Why should we be cautious when using and interpreting them?
#   A2. The manner in which the lexicons were validated could differ 
#       dramatically from the text that we are analyzing. Moreover, these lexicons
#       are based on unigrams alone and don't account for qualifiers which may
#       alter the meaning of the text's sentiment if taken as a whole.

# Join sentiments

sentiment_afinn <- inner_join(tidy_tweets, afinn, by = "word")
sentiment_afinn

sentiment_bing <- inner_join(tidy_tweets, bing, by = "word")
sentiment_bing

# 2c: 2nd COMPREHENSION CHECK ######################
# 1. Create a sentiment_nrc data frame using the code above.
# 

sentiment_nrc <- inner_join(tidy_tweets, nrc, by = "word")
sentiment_nrc

# 2. What do you notice about the change in the number of observations 
#   (i.e. words) between the tidy_tweets and data frames with sentiment 
#   values attached? Why did this happen?
#   A2. There are fewer rows in the sentiment data frames because the 
#       inner join returns the rows of tidy_tweets where there are matching 
#       "words" in the nrc dataframe. The nrc dataframe may not contain a 
#       sentiment value for every word that appears in the tidy_tweets
#       dataframe, therefore fewer rows of tidy_tweets are returned and we are
#       presented with only the data for which a sentiment value can be obtained.

# 3 EXPLORE ######################

ts_plot(tweets, by = "days")

ts_plot(tweets, by = "hours")

# 3: 1st COMPREHENSION CHECK ######################
# 1. Use ts_plot with the group_by function to compare the number of tweets 
#    over time by Next Gen and Common Core standards

?ts_plot

tweets %>% 
  dplyr::group_by(standards) %>% 
  ts_plot(by = "days")

# 2. Which set of standards is Twitter users talking about the most?
#   A2. Twitter users are talking about the common core standards the most.

# Sentiment Summaries

summary_bing <- count(sentiment_bing, sentiment, sort = TRUE)
summary_bing

summary_bing <- sentiment_bing %>% 
  group_by(standards) %>% 
  count(sentiment) 

summary_bing

# Compute Sentiment Value

summary_bing <- sentiment_bing %>% 
  group_by(standards) %>% 
  count(sentiment, sort = TRUE) %>% 
  spread(sentiment, n) 

summary_bing

summary_bing <- sentiment_bing %>% 
  group_by(standards) %>% 
  count(sentiment, sort = TRUE) %>% 
  spread(sentiment, n) %>%
  mutate(sentiment = positive - negative) %>%
  mutate(lexicon = "bing") %>%
  relocate(lexicon)

summary_bing

head(sentiment_afinn)

summary_afinn <- sentiment_afinn %>% 
  group_by(standards) %>% 
  summarise(sentiment = sum(value)) %>% 
  mutate(lexicon = "AFINN") %>%
  relocate(lexicon)

summary_afinn

# 3: 2nd COMPREHENSION CHECK ######################
# For your final task for this walkthough, calculate a single sentiment 
# score for NGSS and CCSS using the remaining nrc and loughan lexicons 
# and answer the following questions. 
#   
# Hint: The nrc lexicon contains “positive” and “negative” values just like 
# bing and loughan, but also includes values like “trust” and “sadness” as 
# shown below. You will need to use the filter() function to select rows that 
# only contain “positive” and “negative.”

# for nrc
nrc

summary_nrc <- sentiment_nrc %>% 
  filter(sentiment %in% c("positive","negative")) %>% 
  group_by(standards) %>% 
  count(sentiment, sort = TRUE) %>% 
  spread(sentiment, n) %>%
  mutate(lexicon = "nrc") %>%
  relocate(lexicon)

# for loughran
loughran 
sentiment_loughran <- inner_join(tidy_tweets, loughran, by = "word")
sentiment_loughran


summary_loughran <- sentiment_loughran %>% 
  filter(sentiment %in% c("positive","negative")) %>% 
  group_by(standards) %>% 
  count(sentiment, sort = TRUE) %>% 
  spread(sentiment, n) %>%
  mutate(lexicon = "loughran") %>%
  relocate(lexicon)


summary_nrc
summary_loughran

# Are these findings above still consistent?
# A. The finding are not entirely consistent. Using the nrc lexicon, the ngss 
#    tweets still score overall positive but the ccss tweets also score positive 
#    unlike the results using the other lexicons where the score negative.
#    Likewise, the loughran lexicon scores both the ngss and ccss tweets as overall
#    negative whereas the ngss tweets score positive in all the other lexicons.
#    These results could be explained by the ratio of negative to positive words
#    in the lexicons. For example, the text suggests that the nrc lexicon has a 
#    lower ratio of negative to positive words than the bing lexicon leading it 
#    to produce higher positive scores.
