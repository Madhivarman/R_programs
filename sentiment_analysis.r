#loading harrypotter package
if (packageVersion("devtools") < 1.6) {
  install.packages("devtools")
}

devtools::install_github("bradleyboehmke/harrypotter")

library(tidyverse)
library(stringr)
library(tidytext)
library(harrypotter)
library(dplyr)
library(spread)

#philosophers_stone[1:2]

#print sentiments
sentiments

#titles
titles <- c("Philosopher's Stone","chamber_of_secrets","Prisoner of Azkaban",
            "Goblet of Fire", "Order of the Phoenix", "Half-Blood Prince",
            "Deathly Hallows")
books <- list(philosophers_stone,chamber_of_secrets,prisoner_of_azkaban,goblet_of_fire,
              order_of_the_phoenix,half_blood_prince,deathly_hallows)
series <- tibble()

for(i in seq_along(titles)){
  clean <- tibble(chapter = seq_along(books[[i]]),text = books[[i]] )%>%
                    unnest_tokens(word,text) %>%
                    mutate(book = titles[i]) %>%
                    select(book,everything())
  series <- rbind(series,clean)
}

series$book <- factor(series$book,levels = rev(titles))

#nrc sentiment data set to access the different sentiments
series %>% right_join(get_sentiments("nrc")) %>% 
  filter(!is.na(sentiment)) %>% 
  count(sentiment,sort = TRUE)
# algorithm we  perform in this
"create an index that breaks up each book by 500 words; this is the approximate number of words on every two pages so this will allow us to assess changes in sentiment even within chapters
join the bing lexicon with inner_join to assess the positive vs. negative sentiment of each word
count up how many positive and negative words there are for every two pages"
spread our data and.
calculate a net sentiment (positive - negative)
plot our data
"

series %>% 
       group_by(book) %>%
       mutate(word_count = 1:n(),index = word_count %/% 500 + 1) %>%
       inner_join(get_sentiments("bing")) %>%
       count(book,index = index,sentiment) %>%
       ungroup() %>%
       spread(sentiment,n,fill = 0) %>%
       mutate(sentiment = positive - negative,book = factor(book,levels=titles)) %>%
  ggplot(aes(index, sentiment, fill = book)) +
  geom_bar(alpha = 0.5, stat = "identity", show.legend = FALSE) +
  facet_wrap(~ book, ncol = 2, scales = "free_x")
  



