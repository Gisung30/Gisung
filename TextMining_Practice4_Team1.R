### Course: Text Mining                #####
### Subject: 2024-2                    #####
### Title: Practice 04                 #####
### TEAM: 01                           #####    
### Member: 22000028 Juha Gwak         #####
###         22000383 GiSung Shin       #####    
############################################


# Package installation and loading
if(!require(officer)) install.packages("officer")
if(!require(dplyr)) install.packages("dplyr")
if(!require(tidyr)) install.packages("tidyr")
if(!require(stringr)) install.packages("stringr")

# Load the libraries for data manipulation and text processing
library(officer)
library(dplyr)
library(tidyr)
library(stringr)

# 1-1: Load "David.pptx" file and summarize slide contents
# Load the PowerPoint file into an R object
david <- read_pptx("David.pptx")
david
# Extract a summary of the slides in the PowerPoint file
david_summary <- pptx_summary(david)
# Display the summary information to understand the structure of the data
david_summary

# Create a data frame with selected columns from the summary
# 'id' is mapped as 'slide_id' and 'type' as 'content_type'
david.content <- david_summary %>%
  select(text, id, content_type, slide_id, media_file)
# Display the first row of the data frame for verification
david.content %>% head(1)

# 1-2: Filter rows with non-empty text in slides with id 3
# Select rows where 'id' is 3 and 'text' is not empty or NA
david.content.all <- david.content %>%
  filter(id == 3, !is.na(text), text != "")
# Display filtered data frame and its dimensions
david.content.all
print(dim(david.content.all))

# 1-3: Concatenate all text content into a single string
# Extract the 'text' column and collapse all values into a single string separated by spaces
david.all <- david.content.all %>%
  pull(text) %>%
  paste(collapse = " ")
# Print the first 20 characters to check the concatenated string
print(substr(david.all, 1, 20))

# 1-4: Generate frequency tables for unigrams, bigrams, and trigrams
# Create a data frame from the combined text content
david_all_df <- data.frame(text = david.all, stringsAsFactors = FALSE)

# Unigram frequency table: Tokenize the text into individual words and count occurrences
unigrams <- david_all_df %>%
  unnest_tokens(unigram, text, token = "words", to_lower = FALSE) %>%
  count(unigram, sort = TRUE) %>%
  rename(Freq = n) %>%
  slice_max(order_by = Freq, n = 10)

# Bigram frequency table: Tokenize the text into pairs of words and count occurrences
bigrams <- david_all_df %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2, to_lower = FALSE) %>%
  count(bigram, sort = TRUE) %>%
  rename(Freq = n) %>%
  slice_max(order_by = Freq, n = 10)

# Trigram frequency table: Tokenize the text into triplets of words and count occurrences
trigrams <- david_all_df %>%
  unnest_tokens(trigram, text, token = "ngrams", n = 3, to_lower = FALSE) %>%
  count(trigram, sort = TRUE) %>%
  rename(Freq = n) %>%
  slice_max(order_by = Freq, n = 10)

# Print frequency tables for unigrams, bigrams, and trigrams
print(unigrams)
print(bigrams)
print(trigrams)

#If you want quick keywords, Unigrams work well. 
#For basic word relationships, Bigrams are useful. 
#If you need a deeper understanding of the text, Trigrams  are ideal.

# 1-5: Plot combined trigram and quadgram frequency data
if(!require(tidytext)) install.packages("tidytext")
library(tidytext)

if(!require(ggplot2)) install.packages("ggplot2")
library(ggplot2)

# Generate quadgram frequency table for plotting
quadgrams <- david_all_df %>%
  unnest_tokens(quadgram, text, token = "ngrams", n = 4, to_lower = FALSE) %>%
  count(quadgram, sort = TRUE) %>%
  rename(token = quadgram, Freq = n)

# Step 1: Ensure column names are consistent for both trigrams and quadgrams data frames
# Rename columns in trigrams and quadgrams to "ngram" and "frequency" for consistency
colnames(trigrams) <- c("ngram", "frequency")
colnames(quadgrams) <- c("ngram", "frequency")

# Combine trigrams and quadgrams, then sort by frequency in descending order and select top 20
combined_grams <- bind_rows(trigrams, quadgrams) %>%
  arrange(desc(frequency)) %>%
  head(20)

# Visualize 
library(ggplot2)

# Create the bar plot with updated column names
ggplot(combined_grams, aes(x = reorder(ngram, frequency), y = frequency)) +
  geom_bar(stat = "identity", fill = "gray", show.legend = FALSE) +  # Use 'gray' fill color
  coord_flip() +  # Flip the coordinates for a horizontal bar plot
  labs(title = "Top 20 Trigrams and Quadgrams", x = "n-gram", y = "Frequency") +  # Set labels
  theme_minimal() +  # Use a minimal theme
  theme(axis.text.y = element_text(size = 8))  # Adjust y-axis text size

#To improve analysis, remove stop words like "is," "to," and "the" to focus on meaningful terms.
#Use stemming or lemmatization to combine similar words (e.g., "describe" and "described"), 
#and filter out low-frequency n-grams that don’t contribute to key themes.
#Eliminate punctuation and special characters, and convert text to lowercase for consistency. 
#Finally, filter out context-specific terms like "BCE" and "centuries" if they don’t add value, 
#allowing more relevant patterns to stand out.


#2-1

# Import the three CSV files into R as variables 
# and rearrange "simpson" by "episode_id" and "character_id".

getwd()

setwd("C:/work/Simpsons")

# Load each file
simpson<-read.csv("simpsons_text.csv")

head(simpson)

simpson.loc<-read.csv("simpsons_locations.csv")

simpson.char<-read.csv("simpsons_characters.csv")

# Use arrange to sort by episode_id
# Note that "character_id" is not available in simpson

# Check the result
simpson<-simpson %>% arrange(episode_id ,number)

simpson %>% head

simpson.loc %>% head

simpson.char %>% head
# The data is now properly prepared

#2-2

library(stringr)

# We need to find the location that appears the most in each episode.
# First, check the data 
head(simpson)

head(simpson.loc)

# To merge the two datasets, unify the column names
colnames(simpson.loc)<-c("location_id","name")

# Merge the two datasets
simpson_merge<-merge(simpson,simpson.loc)

# Calculate the frequency for each episode and location.
# Use group_by to group by episode and location, and calculate the frequency
location_counts <- simpson_merge %>%
  group_by(episode_id, name) %>%
  summarise(count = n()) %>%
  ungroup()

# The frequency of locations for each episode is shown
location_counts

# Then, group by episode_id again and select the top 1 for each group.
simpson.top.loc <- location_counts %>%
  group_by(episode_id) %>%
  slice_max(order_by = count, n = 1) %>%
  ungroup()

# Check the result
simpson.top.loc %>%
  select(episode_id, name, count) %>%
  head()

# The results show the location that appeared most in each episode.
# For example, in the first episode, Simpson Home appeared the most.

#2-3

# We need to analyze the character that appeared the most in each episode.

# Apply a regular expression to extract the characters.
# Extract names that start with uppercase letters and end with a colon ":".
# The remaining part is assumed to be the script.
simpson.ed <- simpson %>%
  mutate(character = raw_text %>% str_extract("^[[:upper:]]{1}[[:alnum:]]{1,}[[:space:]]?[[:upper:]]*[[:alnum:]]*\\:"),
         script = str_replace(raw_text, "^[[:upper:]]{1}[[:alnum:]]{1,}[[:space:]]?[[:upper:]]*[[:alnum:]]*\\:", ""))

# Check if the characters and scripts were properly extracted
head(simpson.ed)

# Calculate the frequency of characters
character_counts <- simpson.ed %>%
  filter(!is.na(character)) %>%  # Select rows with character
  group_by(episode_id, character) %>%
  summarise(count = n()) %>%
  ungroup()

# Select the most frequently appearing character in each episode
simpson.top.char <- character_counts %>%
  group_by(episode_id) %>%
  slice_max(order_by = count, n = 1) %>%
  ungroup()

# Output the result ,select episode_id, character, and count columns
simpson.top.char %>%
  select(episode_id, character, count) %>%
  head()

# The results show that Homer Simpson appears the most in general.

#2-4

# We want to remove actions and keep only the dialogues.

# First, by checking the scripts, 
#it can be seen that actions are enclosed in parentheses ().
simpson.ed$script %>% head(10)

# Remove the text inside the parentheses and create a new column "script_ed".
simpson.ed <- simpson.ed %>%
  mutate(script_ed = gsub("\\([^)]*\\)", "", script))

simpson.ed$script %>% head(10)

simpson.ed$script_ed %>% head(10)

# The result shows that only dialogues remain.

#2-5
# We need to find the keywords in each episode.
# To do this, we will concatenate the scripts in each episode.

# Load the purrr package
library(purrr)

# Group the text by episode_id
simpson.ep1 <- simpson.ed %>%
  filter(script_ed!="") %>%  # Remove missing values in episode_id
  group_by(episode_id) %>%
  summarise(text = paste(script_ed, collapse = " "))

# Convert the text for each episode into a list by episode_id
simpson.ep <- simpson.ep1 %>%
  split(.$episode_id) %>%
  map(~ .x$text)

# Check the text of the first episode
simpson.ep[[1]] %>% substr(1, 300)  # Output the first 300 characters

# Check the text of the second episode
simpson.ep[[2]] %>% substr(1, 300)  # Output the first 300 characters

# Through this process, we can identify the keywords and themes of each episode.
# For instance, the keywords in the first episode, such as "There's no time"Santas" and "Lands", 
## The keywords suggest an urgent situation, while at the same time hinting at a story tied to Christmas.

#2-6
# We need to preprocess the data using VCorpus.

library(tm)
library(SnowballC)

# 1. Create a corpus from simpson.ep
simpson.corpus <- VCorpus(VectorSource(simpson.ep))

# 2. Apply the text preprocessing functions
simpson.cleaned <- simpson.corpus %>%
  tm_map(content_transformer(tolower)) %>%   # Convert to lowercase
  tm_map(removePunctuation) %>%              # Remove punctuation
  tm_map(removeNumbers) %>%                  # Remove numbers
  tm_map(stripWhitespace) %>%                # Remove unnecessary whitespaces
  tm_map(removeWords, stopwords("en")) %>%   # Remove stopwords
  tm_map(stemDocument)                       # Apply stemming (Lemmatization is also possible)


#We applied general preprocessing steps, including converting text to 
#lowercase, removing punctuation, eliminating numbers, and stripping 
#extra whitespace. Additionally, we removed stopwords, following standard 
#text analysis procedures.

# Check the result 
simpson.cleaned[[1]] %>% substr(1, 300) 

# Check the second document
simpson.cleaned[[2]] %>% substr(1, 300) 

# The result shows that the text was cleaned successfully.
