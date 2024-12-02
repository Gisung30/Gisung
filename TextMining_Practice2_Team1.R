### Course: Text Mining                #####
### Subject: 2024-2                    #####
### Title: Practice 02                 #####
### TEAM: 01                           #####    
### Member: 22000028 Juha Gwak         #####
###         22000283 Yejun Park        #####
###         22000383 GiSung Shin       #####    
############################################

#1-1

# Load the necessary libraries
library(officer)
library(dplyr)

# Load the Trump speech .docx file into a variable
trump.speech <- read_docx("Trump_2021_final_speech.docx")

# Summarize the Trump speech content into a data frame using docx_summary
trump.speech.sum <- trump.speech %>% docx_summary

# Display the first few rows of the summarized data frame
trump.speech.sum %>% head(2)


#1-2

# Count the number of unique document indexes in the speech summary
num_doc_index <- length(unique(trump.speech.sum$doc_index))

# Display the number of document indexes
num_doc_index
#There are 29 doc_indices, it represents the number of paragraphs in a document.


#1-3

# Load necessary libraries
library(stringr)

# Extract alphabetic strings from the Trump speech text
extracted_texts <- trump.speech.sum$text %>% str_extract('\\[[:alpha:]{1,}')

# Get the indices of non-NA values
valid_indices <- which(!is.na(extracted_texts))

# Extract the corresponding text based on valid indices
result_texts <- trump.speech.sum$text[valid_indices]

# Display the extracted texts
result_texts

#This code is trying to extract alphabetic sequences preceded by a square bracket 
#from a set of text strings and then filter out the ones that failed the extraction


#1-4

# Filter out non-relevant content, leaving only Trump's speech

# Extract text based on valid indices
result_texts <- trump.speech.sum$text[valid_indices]

# Retain only texts starting with "Donald Trump"
filtered_texts <- ifelse(str_detect(result_texts, "^\\[Donald Trump"), result_texts, NA)

# Apply filtering to the original data frame
trump.speech.sum$filtered_text <- trump.speech.sum$text
trump.speech.sum$filtered_text[valid_indices] <- filtered_texts

# Display the filtered texts
trump.speech.sum$filtered_text

# Further clean the text: remove spaces, NA, and non-English text

# Retain only texts starting with an English letter or "[" (for brackets)
trump.speech.sum$filtered_text <- ifelse(str_detect(trump.speech.sum$filtered_text, "^[A-Za-z\\[]"), trump.speech.sum$filtered_text, NA)

# Remove texts that are only spaces
trump.speech.sum$filtered_text <- ifelse(str_trim(trump.speech.sum$filtered_text) == "", NA, trump.speech.sum$filtered_text)

# Filter out rows where filtered_text is NA
trump.speech.sum <- trump.speech.sum %>% filter(!is.na(filtered_text))

# Display the final cleaned texts
print(trump.speech.sum$filtered_text)


#1-5

# Count the occurrences of the word "love" (case-insensitive) in the filtered text
love_count <- str_count(trump.speech.sum$filtered_text, regex("love", ignore_case = TRUE))

# Sum up the total occurrences of "love"
total_love_count <- sum(love_count, na.rm = TRUE)

# Display the total count of the word "love"
total_love_count


# 2-1
load('airbnb.review.RData')

# Count the number of missing values (NA) in the dataset
airbnb.review %>%
  is.na()%>%
  sum()

# Count the number of duplicated rows in the dataset
airbnb.review %>%
  duplicated()%>%
  sum()

summary(airbnb.review)
print(airbnb.review)

#no missing values or duplicates found.
#but looking into the text, we could find that some words are corrupted, since the text is written not only in Enlgish
#but in Spansih, German etcs... We need to handle these afterwards.


# 2-2

# Group the data by listing_id and concatenate all comments for each listing
df <-airbnb.review %>%
  group_by(listing_id) %>%
  summarise(all_words = paste(comments, collapse = " "))

airbnb.review.ls <- df$all_words%>%
  strsplit(' ')

lapply(airbnb.review.ls, head, 15)
#we first create dataframe called df by grouping the data according to listing_id, and creating a column of all comments pasted together.
#using created 'df''s all_words column, by spliting the sentences into words based on the spaces,  we created the list that we aimed.


#2-3

#Create a logical vector that identifies rows with the phrase 'automated posting' in the comments

comments_logical <- airbnb.review$comments %>%
  str_detect('automated posting')

#Filter out rows where 'automated posting' appears in the comments
airbnb.review.updated <- airbnb.review %>%
  filter(!comments_logical)

airbnb.review.updated

#Group the updated data by listing_id and concatenate the comments for each listing
df.updated <-airbnb.review.updated %>%
  group_by(listing_id) %>%
  summarise(all_words = paste(comments, collapse = " "))

airbnb.review.ls.updated <- df.updated$all_words%>%
  strsplit(' ')

#Display the first 15 words for each listing in the updated list

lapply(airbnb.review.ls.updated, head, 15)
#we searched for automated postings and again created a logical vector
#using this logical vector we filtered the airbnb.review dataframe, and rest are the same process as 2-2.


# 2-4

# Count how many times the word "love" appears in each list element
love_count <- sapply(airbnb.review.ls, function(x) sum(grepl("love", x, ignore.case = TRUE)))

love_count

# The third list has the most mentions of "love" with 32 occurrences.


# 2-5

# Extract words before and after "love" for the listing_id with the most mentions
comments <- airbnb.review.ls[[3]]

# Find the indices where the word "love" appears
love_indices <- grep("love", comments, ignore.case = TRUE)

# Extract the words before and after "love"
love.neighbor <- data.frame(
  love.before = comments[love_indices - 1],  # Word before "love"
  love.before = comments[love_indices],  # The word "love"
  love.after = comments[love_indices + 1]    # Word after "love"
)

# Output the first 6 rows of the result
head(love.neighbor)


# 2-6

# Clean up meaningless characters and spaces from love.before and love.after
love.neighbor_clean <- love.neighbor %>%
  mutate(love.before = str_squish(str_replace_all(love.before, "[^\\w]", "")),  # Remove all non-word characters
         love.after = str_squish(str_replace_all(love.after, "[^\\w]", "")))    # Remove all non-word characters

# Calculate the frequency of words in love.before and love.after
# Count word frequency for love.before
love_before_freq <- love.neighbor_clean %>%
  count(love.before, sort = TRUE)

# Count word frequency for love.after
love_after_freq <- love.neighbor_clean %>%
  count(love.after, sort = TRUE)

# Output the frequency tables
love_before_freq  # Many determiners and conjunctions, with 'everything' appearing frequently.

love_after_freq  # "Many determiners, conjunctions, and the word 'stay' appear frequently."

#Excluding grammatical terms, "everything" and "stay" are the most frequent.
