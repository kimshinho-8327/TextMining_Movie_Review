rm(list = ls())
library(dplyr)
library(tidytext)
library(ggplot2)
library(KoNLP)
library(remotes)
library(showtext)
library(ggwordcloud)
library(multilinguer)
library(textclean)
library(stringr)
library(tidyr)
library(readr)
library(widyr)
library(tidygraph)
library(ggraph)
typeof(sentiment_analysis)

# mbti 데이터 불러오기
sentiment_analysis <- read_csv("C:/csv/csv_mbti.csv")

# mbti 데이터 전처리
sentiment_analysis <- sentiment_analysis %>% select(!X1)
row_names <- colnames(sentiment_analysis)
sentiment_analysis <- t(sentiment_analysis)
sentiment_analysis <- sentiment_analysis %>% as_tibble()
row.names(sentiment_analysis) <- row_names
names(sentiment_analysis)[1:2] = "explain"
sentiment_analysis <- sentiment_analysis %>% mutate(explain = str_squish(replace_html(explain)),
                                                    id = row_names)


glimpse(sentiment_analysis)
str(sentiment_analysis)
View(sentiment_analysis)

nrow(sentiment_analysis)
ncol(sentiment_analysis)

# 명사 단위로 추출
sentiment_analysis <- sentiment_analysis %>% unnest_tokens(input = explain, output = word, token = extractNoun, drop = F)
sentiment_analysis <- sentiment_analysis %>% filter(str_count(word) >= 2)

# 사람, 성격등 성격을 표현할때 사용하기 애매한 단어는 제거함
frequency <- sentiment_analysis %>% filter(word != "사람" & word != "성격" & word!= "자신" & word!= "유형" & word!= "성향") %>% group_by(id) %>% count(word, id, sort = T)

top3 <- frequency %>% slice_max(n, n = 3, with_ties = F)

font_add_google(name = "Black Han Sans", family = "BHS")
showtext_auto()

top12 <- top3[1:12,c(1, 2, 3)]
top12

top12 %>% ggplot(aes(reorder(word, n), n, fill = id)) + geom_col(show.legend = F) + facet_wrap(~id, scales = "free") + coord_flip() + geom_text(aes(label = n), hjust = -0.3) + scale_y_continuous(expand = expansion(mult = c(0.05, 0.25))) + labs(x = NULL, y = NULL, title = "EN-TYPE") + theme(plot.title = element_text(hjust = 0.5), text = element_text(family = "BHS"))
  

top24 <- top3[13:24, c(1, 2, 3)]
top24

top24 %>% ggplot(aes(reorder(word, n), n, fill = id)) + geom_col(show.legend = F) + facet_wrap(~id, scales = "free") + coord_flip() + geom_text(aes(label = n), hjust = -0.3) + scale_y_continuous(expand = expansion(mult = c(0.05, 0.25))) + labs(x = NULL, y = NULL, title = "ES-TYPE") + theme(plot.title = element_text(hjust = 0.5), text = element_text(family = "BHS"))


top36 <- top3[25:36, c(1, 2, 3)]
top36

top36 %>% ggplot(aes(reorder(word, n), n, fill = id)) + geom_col(show.legend = F) + facet_wrap(~id, scales = "free") + coord_flip() + geom_text(aes(label = n), hjust = -0.3) + scale_y_continuous(expand = expansion(mult = c(0.05, 0.25))) + labs(x = NULL, y = NULL, title = "IN-TYPE") + theme(plot.title = element_text(hjust = 0.5), text = element_text(family = "BHS"))

top48 <- top3[37:48, c(1, 2, 3)]
top48

top48 %>% ggplot(aes(reorder(word, n), n, fill = id)) + geom_col(show.legend = F) + facet_wrap(~id, scales = "free") + coord_flip() + geom_text(aes(label = n), hjust = -0.3) + scale_y_continuous(expand = expansion(mult = c(0.05, 0.25))) + labs(x = NULL, y = NULL, title = "IS-TYPE") + theme(plot.title = element_text(hjust = 0.5), text = element_text(family = "BHS"))







sentiment_analysis_df <- frequency %>% pivot_wider(names_from = id,
                                                            values_from = n,
                                                            values_fill = list(n = 0))
View(sentiment_analysis_df)

# 글자 수가 2개 이상인 단어만 추출
sentiment_analysis_df <- sentiment_analysis_df %>% filter(str_count(word) >= 2)

