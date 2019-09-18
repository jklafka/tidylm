# Build model
# These functions serve to create your ngram model
# Takes as input a dataframe, trains on a column of text

library(tidyverse)
library(tidytext)

pad_sentences <- function(x, var) {
  x %>%
    mutate(var = paste0("<s> ", {{ var }}, " </s>"))
}

compute_grams <- function(x, var, n) {
  x %>%
    mutate(var_id = 1:n()) %>%
    pad_sentences({{ var }}) %>%
    group_by(var_id) %>%
    unnest_tokens(gram, {{ var }}, token = "ngrams", n = n) %>%
    ungroup() %>%
    count(gram) %>%
    filter(!is.na(gram))
}

