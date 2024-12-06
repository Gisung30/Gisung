install.packages("tidyverse")     # Data processing and visualization
install.packages("lubridate")     # Date handling
install.packages("tidytext")      # Text data processing
install.packages("httr")          # API calls (stock data)
install.packages("jsonlite")      # JSON data handling
install.packages("text2vec")      # Text vectorization
install.packages("rJava")

# Load necessary libraries
library(tidyverse)
library(lubridate)
library(tidytext)
library(httr)
library(jsonlite)
library(dplyr)

library(stringr)

# 1. Data Loading ------------------------------------------------------
# Load news data
news_data <- readxl::read_excel("Hanmi.xlsx")  # Path to the news data

head(news_data)

# Keep specific columns
news_data <- news_data %>%
  select(`뉴스 식별자`, 일자, 언론사, 기고자, 제목, 본문)

# Check dates
news_data["일자"]

news_data <- news_data %>%
  mutate(일자 = trimws(일자))  # Remove white spaces

news_data <- news_data %>%
  mutate(일자 = as.Date(일자, format = "%Y%m%d"))  # Assign correct date format

# Keep specific columns and rename
news_data <- news_data %>%
  select(`뉴스 식별자`, 일자, 언론사, 기고자, 제목, 본문) %>%
  rename(
    뉴스_ID = `뉴스 식별자`,
    날짜 = 일자,
    언론 = 언론사,
    기자 = 기고자,
    제목 = 제목,
    본문 = 본문
  )

# Check results
head(news_data)

# Use stock data downloaded from Yahoo Finance
stock_data <- read.csv("042700.KS.csv")

head(stock_data)

# Remove the first row
stock_data <- stock_data[-1, ]  # Remove the first row using row index

head(stock_data)

# Convert dates in stock_data
stock_data <- stock_data %>%
  mutate(Date = as.Date(Date))  # Remove timezone and convert to date format

# Check both datasets
print(head(news_data))
print(head(stock_data))

# Check results
print(head(stock_data))
head(news_data)

# Calculate news frequency
news_frequency <- merged_data %>%
  group_by(날짜) %>%
  summarise(뉴스_빈도 = n())

# Merge news frequency with stock data
freq_price_data <- stock_data %>%
  left_join(news_frequency, by = c("Date" = "날짜"))

# Check variable types
str(freq_price_data$뉴스_빈도)
str(freq_price_data$Close)

# Convert to numeric
freq_price_data <- freq_price_data %>%
  mutate(
    뉴스_빈도 = as.numeric(뉴스_빈도),
    Close = as.numeric(Close)
  )

# Check variable types
str(freq_price_data$뉴스_빈도)
str(freq_price_data$Close)

# Handle missing values: Set frequency to 0 for dates with no news data
freq_price_data <- freq_price_data %>%
  mutate(뉴스_빈도 = ifelse(is.na(뉴스_빈도), 0, 뉴스_빈도))

# Calculate correlation
correlation_freq_close <- cor(freq_price_data$뉴스_빈도, freq_price_data$Close, use = "complete.obs")
print(paste("Correlation between news frequency and stock prices:", correlation_freq_close))


# Check main text data
head(news_data$본문)

clean_text <- function(text) {
  text %>%
    # Remove line breaks and other control characters
    str_replace_all("[\\r\\n]+", " ") %>%
    # Remove all content inside parentheses
    str_replace_all("\\([^)]*\\)", "") %>%
    str_replace_all("\\[[^]]*\\]", "") %>%
    str_replace_all("\\{[^}]*\\}", "") %>%
    # Remove all characters except Korean, English, and spaces
    str_replace_all("[^가-힣a-zA-Z\\s]", "") %>%
    # Remove consecutive spaces
    str_squish()
}

# Clean data
news_data <- news_data %>%
  mutate(본문_정제 = map_chr(본문, clean_text))

# map_chr() always returns a character vector and ensures type stability.
# It is highly compatible with dplyr and provides clearer debugging information in case of errors.
# The return type is explicit, making the code more readable and robust against exceptions than sapply().

# Check results
head(news_data$본문_정제)

# Tokenization using the `tidytext` package
library(tidytext)

# Tokenize main text data
tokenized_data <- news_data %>%
  unnest_tokens(단어, 본문_정제)

# Check results
head(tokenized_data$단어, 30)

# Stopword list
stop_words <- c("의", "가", "이", "은", "들", "는", "좀", "잘", "로",
                "걍", "과", "도", "를", "으로", "자", "에", "와", "한", 
                "에서", "을", "돼", "면", "다") # End stopwords

remove_words <- c("뉴스", "일보")  # Remove words containing specific terms

# Remove stopwords and filter specific words
cleaned_data <- tokenized_data %>%
  # Remove stopwords at the end
  mutate(
    단어 = str_remove(단어, paste0("(", paste(stop_words, collapse = "|"), ")$", collapse = ""))
  ) %>%
  # Remove blank words
  filter(단어 != "") %>%
  # Filter out specific words
  filter(!str_detect(단어, paste(remove_words, collapse = "|")))

# Check results
head(cleaned_data$단어, 30)

# Word data
word_list <- cleaned_data$단어

# Calculate word frequency (exclude single-character words)
word_frequency <- as.data.frame(table(word_list)) %>%
  filter(nchar(as.character(word_list)) > 1) %>%  # Exclude single-character words
  arrange(desc(Freq))  # Sort by frequency in descending order

library(dplyr)

length(word_frequency)

# Extract top 1000 words
top_10_words <- head(word_frequency, 1000)

# Calculate daily frequency of each word
word_date_counts <- cleaned_data %>%
  filter(단어 %in% top_10_words$word_list) %>%
  group_by(단어, 뉴스_ID) %>%
  summarise(단어_빈도 = n()) %>%
  left_join(news_data %>% select(뉴스_ID, 날짜), by = "뉴스_ID") %>%
  group_by(단어, 날짜) %>%
  summarise(단어_빈도_날짜 = sum(단어_빈도, na.rm = TRUE))

# Merge with stock data to create a dataset of words and stock prices
merged_word_stock <- word_date_counts %>%
  left_join(stock_data, by = c("날짜" = "Date"))

# Handle missing values: Exclude days with no stock data
merged_word_stock <- merged_word_stock %>%
  filter(!is.na(Close)) %>%
  mutate(Close = as.numeric(Close))

# Calculate correlation between stock prices and daily word frequency
correlation_results <- merged_word_stock %>%
  group_by(단어) %>%
  summarise(상관계수 = cor(단어_빈도_날짜, Close, use = "complete.obs"))

# Print results
print(correlation_results)

# Sort correlation results in descending order
correlation_results <- correlation_results %>%
  arrange(desc(상관계수))

# Print results
View(head(correlation_results, 50))

# Sort correlation results in ascending order
correlation_results_as <- correlation_results %>%
  arrange(상관계수)

# Print results
View(head(correlation_results_as, 50))

# Filter for specific words
target_word <- "과정"

# Extract 뉴스_ID containing the target word
target_news_ids <- cleaned_data %>%
  filter(단어 == target_word) %>%
  select(뉴스_ID) %>%
  distinct()

# Extract main text of 뉴스_ID from the original news data
target_news_context <- news_data %>%
  filter(뉴스_ID %in% target_news_ids$뉴스_ID) %>%
  select(뉴스_ID, 본문)

# Check results
View(target_news_context)





