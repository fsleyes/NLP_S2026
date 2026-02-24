shawn_genie <- function(x, wordcol) {
  
  
  library(tidyverse)
  library(tm)
  library(stringr)
  library(stringi)
  library(textstem)
  load(url("https://github.com/Reilly-ConceptsCognitionLab/reillylab_publicdata/blob/main/replacements_25.rda?raw=true"))
  load(url("https://github.com/Reilly-ConceptsCognitionLab/reillylab_publicdata/blob/main/Temple_stops25.rda?raw=true"))
  
  
  #what are the column names that are not the wordcol?
  x_metadata <- setdiff(colnames(x), wordcol)
  
  #some basic standardiziation
  data_prep_standard <- x %>%
    dplyr::mutate(text_initialSplit = stringi::stri_enc_toutf8(.[[wordcol]])) %>% #convert to UTF
    dplyr::mutate(text_initialSplit = ifelse( #standardize apostrophes
      is.na(text_initialSplit), NA_character_, 
      stringi::stri_replace_all_regex(
        text_initialSplit,
        "[\u2018\u2019\u02BC\u201B\uFF07\u0092\u0091\u0060\u00B4\u2032\u2035]",
        "'")))
  
  
  #split the text first then delete the original column
  data_prep_split <- data_prep_standard %>%
    tidyr::separate_rows(text_initialSplit, sep = "[[:space:]]+") %>%
    dplyr::filter(!is.na(text_initialSplit))
  
  
  
  #do cleaning on individual text rows
  #RETAIN apostrophes
  data_prep_convert <- data_prep_split %>%
    dplyr::mutate(text_prep = tolower(as.character(text_initialSplit)),
                  text_prep = tm::stripWhitespace(text_prep),
                  text_prep = stringr::str_replace_all(text_prep, "^'+|'+$", ""),
                  text_prep = stringr::str_replace_all(text_prep, "[/-]", " "))
  
  #load in the list of contractions that we want to replace
  replacements_25 <- replacements_25 %>%
    dplyr::mutate(word = stringi::stri_replace_all_regex(word, 
                                                         "[\u2018\u2019\u02BC\u201B\uFF07\u0092\u0091\u0060\u00B4\u2032\u2035]",
                                                         "'", 
                                                         vectorize_all = FALSE
    ))
  
  
  contractions_list <- replacements_25$word
  
  
  #replace contractions
  data_prep_contractReplace <- data_prep_convert %>%
    dplyr::left_join(replacements_25, by = c("text_prep" = "word")) %>%
    dplyr::mutate(text_prep = dplyr::case_when(
      is.na(text_prep) | text_prep == "" ~ NA_character_,
      !is.na(replacement) ~ replacement,
      TRUE ~ text_prep
    )) %>%
    dplyr::select(-c(wordcol, replacement)) %>%
    tidyr::separate_rows(text_prep, sep = "[[:space:]]+")
  
  
  data_prep_clean <- data_prep_contractReplace %>%
    dplyr::mutate(
      word_clean = text_prep,
      word_clean = gsub(pattern = "[^a-zA-Z]", replacement = "", word_clean), #uses regex to remove any numbers from a string
      word_clean = tm::stripWhitespace(word_clean)
    ) %>%
    dplyr::filter(!is.na(word_clean) & word_clean != "") %>%
    dplyr::filter(!(word_clean %in% c("i","ii","iii","iv","v","vi","vii","viii","ix","x")))
  
  
  
  data_prep_lemma <- data_prep_clean %>%
    dplyr::mutate(word_lemma = textstem::lemmatize_strings(word_clean)) %>%
    dplyr::mutate(
      word_lemma = word_lemma,
      word_lemma = gsub(pattern = "[^a-zA-Z]", replacement = "", word_lemma), #uses regex to remove any numbers from a string
      word_lemma = tm::stripWhitespace(word_lemma)
    ) %>%
    dplyr::filter(!is.na(word_lemma) & word_lemma != "")
  
  
  stopwords <- Temple_stops25$word
  data_prep_stopRemove <- data_prep_lemma %>%
    dplyr::mutate(
      is_stopword = ifelse( #create new variable identifying which words are stopwords
        is.na(word_lemma), NA, #if empty do nothing
        word_lemma %in% stopwords) #if not empty then TRUE if stopword, FALSE if not stopword
    ) %>%
    dplyr::filter(is_stopword == FALSE) #keep only the words that are NOT stopwords
  
  
  data_final <- data_prep_stopRemove %>%
    dplyr::select(dplyr::all_of(x_metadata), !!wordcol := word_lemma)
  
  
  
  
  return(data_final)
  
}
