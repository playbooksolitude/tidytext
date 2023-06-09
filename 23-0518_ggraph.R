#23-0518 thu 10:11

#
library(tidyverse)
install.packages("tidygraph")
library(tidygraph)
library(showtext)
showtext_auto()

#
?tidygraph
library(tidygraph)

play_erdos_renyi(10, 0.5) %>% 
  activate(nodes) %>% 
  mutate(degree = centrality_degree()) %>% 
  activate(edges) %>% 
  mutate(centrality = centrality_edge_betweenness()) %>% 
  arrange(centrality)

devtools::install_github('thomasp85/ggraph')

######
library(ggraph)
#> Loading required package: ggplot2
library(tidygraph)
#> 
#> Attaching package: 'tidygraph'
#> The following object is masked from 'package:stats':
#> 
#>     filter

# Create graph of highschool friendships
load("./files/highschool.rda")
highschool

#if
highschool |> mutate(
  year = ifelse(year == 1957, "영화", "방송")
) -> highschool
highschool |> view()

#
graph <- as_tbl_graph(highschool) %>% 
  mutate(Popularity = centrality_degree(mode = 'in'))

graph |> print(n = 50)

# plot using ggraph
ggraph(graph, layout = 'kk') + 
  geom_edge_fan(aes(alpha = after_stat(index)), show.legend = FALSE) + 
  geom_node_point(aes(size = Popularity)) + 
  facet_edges(~year) + 
  theme_graph(foreground = 'steelblue', fg_text_colour = 'white')

graph |> view()
#
ggraph(graph, layout = 'kk') + 
  geom_edge_fan(aes
                (alpha = after_stat(index),
                    color = ifelse(highschool$from %in% 
                                     c(1,2,3,4,5,66,67,68,69,70,71,72), 
                                   "red", "black")), 
                show.legend = FALSE) + 
  geom_node_point(aes(size = Popularity)) + 
  facet_edges(~year) + 
  theme_graph(foreground = 'steelblue', fg_text_colour = 'white')


#