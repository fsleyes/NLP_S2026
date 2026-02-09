add_func <- function(x, y) {
  
  #add them together
  sum <- x + y
  return(sum)
}




mean_func <- function(x, y) {
  
  #take the sum
  sum <- x + y
  
  #divide by 2
  mean <- sum / 2
  return(mean)
}



replace_func <- function(string, old_Word, replacement_Word) {
  
  x <- string
  x_clean <- x %>%
    gsub(pattern = old_Word, replacement = replacement_Word) 
  
  return(x_clean)
  
  
}



#takes a string returns a string
remove_special_func <- function(myString) {
  
  
  #replaces punctuation and special characters with blanks
  x_clean <- gsub("[[:punct:]]", "", myString)
  
  
  return(x_clean)
  
  
}




#takes a string returns a data frame
split_string <- function(x) {
  
  #convert to a data frame first
  cleaned_x <- data.frame(string = x) %>%
    separate_longer_delim(string, delim = " ") %>% #pivots to long format based on delimeter of space
    filter(string != "") #filters so that empty rows are deleted
  return(cleaned_x)
  
}




remove_the <- function(x) {
  
  #replaces "the" with blank
  x_clean <- x %>%
    gsub(pattern = "the", replacement = "", ignore.case = TRUE)
  return(x_clean)
}




#takes a string returns a string
remove_numbers <- function(x) {
  
  #uses regex to remove any numbers from a string
  x_clean <- x %>%
    gsub(pattern = "\\d+", replacement = "")
  return(x_clean)
  
}





#takes a string, returns a tidy data frame with one word per row
#x is the data frame
#words is a list of words that we want to remove as a vector
clean_string <- function(x, words) {
  
  #remove special characters
  x <- remove_special_func(x)
  
  
  #remove numbers 
  x <- remove_numbers(x)
  
  #remove words that we don't want by sequencing along a list of words that we don't want in a loop
  for (i in seq_along(words)) {
    
    x <- replace_func(string = x, old_Word = words[i], replacement_Word = "")
    x <- tolower(x)
  }
  
  return(x)
  
}