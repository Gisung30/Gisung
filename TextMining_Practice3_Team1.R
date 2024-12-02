### Course: Text Mining                #####
### Subject: 2024-2                    #####
### Title: Practice 03                 #####
### TEAM: 01                           #####    
### Member: 22000028 Juha Gwak         #####
###         22000383 GiSung Shin       #####
############################################


# 1-1

# Load the required package
library(rvest)

# Specify the link
url <- "https://www.biblegateway.com/passage/?search=Acts%2019&version=NIV"

# Save the HTML content to a variable
html <- read_html(url)

html

# 1-2

# Extract all <div> tags
div_tags <- html %>% html_nodes("div")

# Check the number of extracted <div> tags
length(div_tags)  # There are 155 <div> tags in total


# Extract <div> tags containing 'text-html'
div_lists <- html %>% html_nodes("div.text-html")

# Check the result
div_lists



# 1-3

# Extract <p> tags
ptag <- html %>% html_nodes("p")

# Check the result
ptag


# 1-4

# Extract text from <p> tags and save it to act.19 variable
act.19 <- ptag %>% html_text()

# Check the first two lines of the text
head(act.19, 2)

# Check the number of elements in act.19
length(act.19)

# There are 17 elements
# The number of elements in act.19 indicates how many paragraphs 
# <p> tags were extracted from the HTML.
# Each element represents a block of text inside a <p> tag.


# 1-5

# Remove unnecessary text 
act.19_cleaned <- gsub("Holy Bible.*|NIV Reverse.*|By submitting.*", "", act.19)

# Combine already separated text into a single string
act.19_combined <- paste(act.19_cleaned, collapse = " ")

# Split based on verse numbers ignoring those in brackets
act.19_split <- unlist(strsplit(act.19_combined, "(?<!\\[)\\d+(?!\\])", perl = TRUE))

# Remove parentheses and letters inside them
act.19_no_brackets <- gsub("\\([A-Za-z]+\\)|\\[[A-Za-z]+\\]", "", act.19_split)

# Remove empty entries including spaces
act.19_no_empty <- act.19_no_brackets[act.19_no_brackets != ""]

# Trim whitespace from both ends
act.19 <- trimws(act.19_no_empty)

# Check the result 
head(act.19, 6)


# 1-6

# Generate verse numbers starting from 1
verse_number <- 1:length(act.19)

# Create a data frame
act.19_df <- data.frame(verse_number = verse_number, verse = act.19, stringsAsFactors = FALSE)

# Check the result 
head(act.19_df, 2)


# 1-7

# Convert all verses to lowercase and remove punctuation
act.19_processed <- tolower(gsub("[[:punct:]]", "", act.19_df$verse))

# Split each verse into individual words
act.19_word <- unlist(strsplit(act.19_processed, "\\s+"))

# Remove whitespace around each word
# Use gsub to replace those not starting with an alphabet character with ""
act.19_word <- gsub("^[^a-zA-Z]+|[^a-zA-Z]+$", "", act.19_word)

# Remove empty strings
act.19_word <- act.19_word[act.19_word != ""]

# Check the result 
head(act.19_word)


# 1-8

# Calculate word frequency
word_freq <- table(act.19_word)

# Create a data frame
act.19.word.df <- as.data.frame(word_freq)

# Rename the columns
colnames(act.19.word.df) <- c("word", "Freq")

# Check the result (print the first 6 words and frequencies)
head(act.19.word.df)


# 1-9

# Load required package
library(ggplot2)

# Select top 30 words
top_30_words <- act.19.word.df[order(-act.19.word.df$Freq), ][1:30, ]

# Create a bar graph
ggplot(top_30_words, aes(x = reorder(word, Freq), y = Freq)) + 
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Top 30 Words in Acts 19", x = "Words", y = "Frequency") +
  theme_minimal() +
  coord_flip()  # Flip the axes

# Most of the words are pronouns or articles, 
# so the result is not particularly meaningful.

# 2-1

# Load required package
library(rvest)

# Specify the link
url <- "https://www.handong.edu/kor/major/college/ACE/faculty"

# Save the HTML content to a variable
saai_html <- read_html(url)

# Check the result 
print(saai_html)


# 2-2

# Extract faculty information
saai <- saai_html %>%
  html_nodes("table") %>% # Select the table
  html_table(fill = TRUE) # Convert the table to a data frame

# Check the result
saai

# 2-3

# Load required package
library(dplyr)

# Create a data frame based on faculty information
saai.df <- do.call(rbind, lapply(saai, function(member) {
  # Extract information corresponding to each keyword and convert to data frame
  data.frame(
    Name = member[which(member[[1]] == "이름"), 2],  
    Major = member[which(member[[1]] == "전공"), 2],
    Office = member[which(member[[1]] == "연구실"), 2],
    Phone = member[which(member[[1]] == "전화"), 2],
    Email = member[which(member[[1]] == "E-mail"), 2],
    stringsAsFactors = FALSE
  )
}))

# Rename the columns
colnames(saai.df) <- c("Name", "Major", "Office", "Phone", "Email")

# Check the result
head(saai.df)


# 2-4

# Add new columns, split KorName and EngName
saai.df <- saai.df %>%
  mutate(
    KorName = gsub("\\(.*\\)", "", Name),  # Extract Korean name
    EngName = gsub(".*\\((.*)\\)", "\\1", Name)  # Extract English name
  )


# Check the result
print(saai.df)

# Remove the existing Name column and rearrange the column order
saai.df <- saai.df %>%
  select(KorName, EngName, Major, Office, Phone, Email)

# Check the result
print(saai.df)


# 3-1

library(pdftools)

# Import PDF file to a variable
mat10 <- pdf_text("Matthew10.pdf")  # Change to the actual path of the PDF file.

# Combine the character vector into a single element
mat10 <- paste(mat10, collapse = " ")  # Combine all text into one string.

# Check the result (optional)
print(mat10)


# 3-2

# Split the text from the PDF into verses and clean each verse
mat10.verse <- unlist(strsplit(mat10, "\\d+\\s"))  # Split based on the verse numbers

# Remove whitespace and empty strings
mat10.verse <- trimws(mat10.verse)
mat10.verse <- mat10.verse[mat10.verse != ""]  # Remove empty strings

# Check the result
head(mat10.verse)



# 3-3

# Create a list of words for each verse
mat10.verse.word <- lapply(mat10.verse, function(x) unlist(strsplit(x, "\\s+")))

# Check the result 
mat10.verse.word[[1]]


# 3-4

# Combine all words into a single vector
all_words <- unlist(mat10.verse.word)

# Calculate word frequency
word_freq <- table(all_words)

# Sort in descending order and extract the top 10 words
top_10_words <- sort(word_freq, decreasing = TRUE)[1:10]

# Convert to a data frame
top_10_df <- as.data.frame(top_10_words)

# Rename the columns
colnames(top_10_df) <- c("Word", "Frequency")

# Check the result
print(top_10_df)

# Create a bar graph
ggplot(top_10_df, aes(x = reorder(Word, Frequency), y = Frequency)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Top 10 Most Frequently Used Words in Matthew 10",
       x = "Words", y = "Frequency") +
  theme_minimal() +
  coord_flip()  # Flip the axes

#The finding makes sense, as the most frequently used words are  
#common function words such as articles, prepositions, and conjunctions 

