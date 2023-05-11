#23-0511 thu 11:05

#
library(tidytext)
library(afinn)
#install.packages("textdata")
library(textdata)

# 용어집
sentiments
get_sentiments("nrc") #error
get_sentiments("bing")
get_sentiments("afinn")

#
austen_books() |> group_by(book) |> 
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]",
                                                 ignore_case = T)))) |> 
  ungroup() |> 
  unnest_tokens(word, text) -> tidy_books

#nrcjoy
get_sentiments("nrc") |> filter(sentiment %in% c("joy")) -> nrcjoy

tidy_books |> 
  filter(book %in% c("Emma")) |> 
  inner_join(nrcjoy, by = "word") |> 
  count(word, sort = T)

#
tidy_books |> 
  inner_join(get_sentiments("bing")) |> 
  count(book, index = linenumber %/% 80)

#


