#23-0511 thu 09:40

#
library(tidyverse)
library(tidytext)
library(janeaustenr)
library(gutenbergr)

#
c("Because I could not stop for Death -",
  "He kindly stopped for me -",
  "the Carriage held but just Ourselves -",
  "and Immortality") -> text

text
tibble(
  line = c(1:4),
  text = text
) -> text_df

text_df |> 
  unnest_tokens(word, text)

#jane ostine
austen_books() |> 
  group_by(book) |> 
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, 
                                     regex("^chapter [\\divxlc]",
                                           ingore_case = T)))) |> 
  ungroup() -> original_books

original_books |> 
  unnest_tokens(word, text) -> tidy_books

#불용어
tidy_books |> anti_join(stop_words) -> tidy_books

#가장 많이 쓰인 단어
tidy_books |> count(word, sort = T) |> 
  filter(n > 600) |> 
  ggplot(aes(word |> fct_reorder(n), 
             n)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme(axis.title.y = element_blank())

#구텐베르크 
gutenberg_download(c(35,36,5230,159)) -> hgwells
table(hgwells$gutenberg_id, useNA = "always")

  #웰스
hgwells |> 
  unnest_tokens(word, text) |> 
  anti_join(stop_words) -> tidy_hgwells

tidy_hgwells |> count(word, sort = T)

  #브론테
gutenberg_download(c(1260,768,969,9182,767)) -> bronte

bronte |> unnest_tokens(word, text) |> 
  anti_join(stop_words) -> tidy_bronte

tidy_bronte |> count(word, sort = T)

semi_join(tidy_hgwells |> count(word, sort = T), 
          tidy_bronte |> count(word, sort = T), 
          by = "word")

#bind_rows() #이거 좋다
tidy_books |> dim()
tidy_bronte |> dim()
bind_rows(tidy_bronte, tidy_books)

bind_rows(
tidy_bronte |> mutate(author = "Bronte Sisters"),
tidy_hgwells |> mutate(author = "H.G. Wells"),
tidy_books |> mutate(author = "Jane Austin")) |> 
  mutate(
    word = str_extract(word, "[a-z']+")) |> 
  count(author, word) |> 
  group_by(author) |> 
  mutate(proportion = n / sum(n)) |> 
  select(-n) |> 
  pivot_wider(
    names_from = author,
    values_from = proportion) |> 
    gather(author, proportion, 'Bronte Sisters':'H.G. Wells') -> frequency

frequency |> ggplot(aes(x = proportion, y = "Jane Austen")) +
  geom_abline() +
  geom_jitter() +
  geom_text(aes(label = word)) +
  scale_x_log10(labels = percent_format())

#상관계수

































