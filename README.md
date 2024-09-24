library(data.table)
file_path <- "~/Desktop/tagged_texts(female author).txt"
text_data <- readLines(file_path)
text_data <- gsub("[[:punct:]&&[^/]]", "", text_data)
word_tag_pairs <- unlist(strsplit(text_data, "\\s+"))
valid_word_tag_pairs <- word_tag_pairs[grepl("/", word_tag_pairs)]
valid_word_tag_pairs <- valid_word_tag_pairs[!grepl("[^a-zA-Z/]", valid_word_tag_pairs)]
print(head(valid_word_tag_pairs, 20))
word_tag_dt <- rbindlist(lapply(valid_word_tag_pairs, function(x) {
  word_tag <- unlist(strsplit(x, "/"))
  if(length(word_tag) >= 2 && word_tag[1] != "" && !is.na(word_tag[1])) {
    list(Word = word_tag[1], POS = word_tag[2])
  } else {
    list(Word = NA, POS = NA)
  }
}))
word_tag_dt <- na.omit(word_tag_dt)
print(word_tag_dt) 
word_count_dt <- word_tag_dt[, .(Count = .N), by = .(Word, POS)]
print(word_count_dt)  
total_words <- sum(word_count_dt$Count)
word_count_dt[, Complement Count := total_words - Count]
print(word_count_dt) 
word_count_dt[, Relative_Frequency := Count / total_words]
print(word_count_dt)  
word_count_dt[, Source := "female"]
print(word_count_dt)  
setnames(word_count_dt, c("Word", "POS"), c("Item", "Part_of_Speech"))
print(word_count_dt)  
final_table_dt <- word_count_dt[, .(Source, Item, Count, Complement_Count, Relative_Frequency, Part_of_Speech)]
print(final_table_dt) 
final_table_dt <- final_table_dt[!(is.na(Item) | Item == "")]
print(final_table_dt) 
fwrite(final_table_dt, "~/Desktop/female_author_word_analysis.csv")

library(data.table)
file_path <- "~/Desktop/tagged_texts(male author).txt"
text_data <- readLines(file_path)
text_data <- gsub("[[:punct:]&&[^/]]", "", text_data)
word_tag_pairs <- unlist(strsplit(text_data, "\\s+"))
valid_word_tag_pairs <- word_tag_pairs[grepl("/", word_tag_pairs)]
valid_word_tag_pairs <- valid_word_tag_pairs[!grepl("[^a-zA-Z/]", valid_word_tag_pairs)]
print(head(valid_word_tag_pairs, 20))  
word_tag_dt <- rbindlist(lapply(valid_word_tag_pairs, function(x) {
  word_tag <- unlist(strsplit(x, "/"))
  if(length(word_tag) >= 2 && word_tag[1] != "" && !is.na(word_tag[1])) {
    list(Word = word_tag[1], POS = word_tag[2])
  } else {
    list(Word = NA, POS = NA)
  }
}))
word_tag_dt <- na.omit(word_tag_dt)
print(word_tag_dt) 
word_count_dt <- word_tag_dt[, .(Count = .N), by = .(Word, POS)]
print(word_count_dt)
total_words <- sum(word_count_dt$Count)
word_count_dt[, Complement_Count := total_words - Count]
print(word_count_dt)  
word_count_dt[, Relative_Frequency := Count / total_words]
print(word_count_dt)  
word_count_dt[, Source := "male"]
print(word_count_dt) 
setnames(word_count_dt, c("Word", "POS"), c("Item", "Part_of_Speech"))
print(word_count_dt) 
final_table_dt <- word_count_dt[, .(Source, Item, Count, Complement_Count, Relative_Frequency, Part_of_Speech)]
print(final_table_dt) 
final_table_dt <- final_table_dt[!(is.na(Item) | Item == "")]
print(final_table_dt) 
write(final_table_dt, "~/Desktop/male_author_word_analysis.csv")

library(data.table)
female_data <- fread("~/Desktop/female_author_word_analysis2.csv")
male_data <- fread("~/Desktop/male_author_word_analysis.csv")
combined_data <- rbind(female_data, male_data)
print(combined_data)
fwrite(combined_data, "~/Desktop/combined_author_word_analysis.csv")

library(data.table)
female_data <- fread("~/Desktop/female_author_word_analysis2.csv")
male_data <- fread("~/Desktop/male_author_word_analysis.csv")
female_data[, Item := tolower(Item)]
male_data[, Item := tolower(Item)]
female_data <- female_data[, .(Count = sum(Count), 
                               Complement_Count = sum(Complement_Count),
                               Relative_Frequency = sum(Relative_Frequency)), 
                           by = .(Source, Item, Part_of_Speech)]

male_data <- male_data[, .(Count = sum(Count), 
                           Complement_Count = sum(Complement_Count),
                           Relative_Frequency = sum(Relative_Frequency)), 
                       by = .(Source, Item, Part_of_Speech)]
combined_data <- rbind(female_data, male_data)
print(combined_data)
fwrite(combined_data, "~/Desktop/combined_author_word_analysis.csv")

> setwd("/Users/mac/Desktop")
> data <- read.csv("combined_author_word_analysis(New).csv")
> low_freq_data <- data[data$Count >= 1 & data$Count <= 5, ]
> high_freq_data <- data[data$Count > 5, ]
> write.csv(high_freq_data, "high_freq_words.csv", row.names = FALSE)
> write.csv(low_freq_data, "low_freq_words.csv", row.names = FALSE)
>
> #Adjective
#High Frequency
setwd("/Users/mac/Desktop")
data <- read.csv("combined_author_word_analysis(New).csv")
adj_data <- data[data$Part_of_Speech == "JJ", ]
library(dplyr)
top10_adj <- adj_data %>%
  group_by(Source) %>%
  arrange(desc(Count)) %>%
  slice_head(n = 10) %>%
  select(Source, Item, Count, Complement_Count)
write.csv(top10_adj, "top10_adjectives_analysis.csv", row.names = FALSE)

library(ggplot2)
ggplot(top10_adj, aes(x = reorder(Item, -Relative_Frequency), y = Relative_Frequency, fill = Source)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("male" = "#0be4ff", "female" = "#4fff76")) +
  labs(title = "Top 10 Adjectives Frequency Comparison",
       x = "Adjectives",
       y = "Relative Frequency",
       fill = "Source") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5)) 
ggsave("top10_adjectives_comparison.png", width = 10, height = 6)

> setwd("/Users/mac/Desktop")
> data <- read.csv("combined_author_word_analysis(New).csv")
> little_data <- data[data$Item == "little" & data$Part_of_Speech == "JJ", ]
> female_count <- sum(little_data$Count[little_data$Source == "female"])
> male_count <- sum(little_data$Count[little_data$Source == "male"])
> female_non_adj_count <- sum(data$Count[data$Source == "female"]) - female_count
> male_non_adj_count <- sum(data$Count[data$Source == "male"]) - male_count
> observed <- matrix(c(female_count, male_count, female_non_adj_count, male_non_adj_count),
+                    nrow = 2,
+                    byrow = TRUE,
+                    dimnames = list(c("Adjective", "Non-Adjective"),
+                                    c("Female", "Male")))
> chisq_test <- chisq.test(observed)
> std_residuals <- chisq_test$stdres
> print(chisq_test)

	Pearson's Chi-squared test with Yates' continuity correction

data:  observed
X-squared = 2032.6, df = 1, p-value < 2.2e-16

> print(std_residuals)
                 Female      Male
Adjective      45.09058 -45.09058
Non-Adjective -45.09058  45.09058
> write.csv(std_residuals, "little_std_residuals.csv", row.names = TRUE)
>
> > other_data <- data[data$Item == "other" & data$Part_of_Speech == "JJ", ]
> female_count <- sum(other_data$Count[other_data$Source == "female"])
> male_count <- sum(other_data$Count[other_data$Source == "male"])
> female_non_adj_count <- sum(data$Count[data$Source == "female"]) - female_count
> male_non_adj_count <- sum(data$Count[data$Source == "male"]) - male_count
> observed <- matrix(c(female_count, male_count, female_non_adj_count, male_non_adj_count),
+                    nrow = 2,
+                    byrow = TRUE,
+                    dimnames = list(c("Adjective", "Non-Adjective"),
+                                    c("Female", "Male")))
> chisq_test <- chisq.test(observed)
> std_residuals <- round(chisq_test$stdres, 2)
> print(chisq_test)

	Pearson's Chi-squared test with Yates' continuity correction

data:  observed
X-squared = 375.84, df = 1, p-value < 2.2e-16

> print(std_residuals)
              Female   Male
Adjective     -19.39  19.39
Non-Adjective  19.39 -19.39
> write.csv(std_residuals, "other_std_residuals.csv", row.names = TRUE)
>
> > good_data <- data[data$Item == "good" & data$Part_of_Speech == "JJ", ]
> female_count <- sum(good_data$Count[good_data$Source == "female"])
> male_count <- sum(good_data$Count[good_data$Source == "male"])
> female_non_adj_count <- sum(data$Count[data$Source == "female"]) - female_count
> male_non_adj_count <- sum(data$Count[data$Source == "male"]) - male_count
> observed <- matrix(c(female_count, male_count, female_non_adj_count, male_non_adj_count), nrow = 2, byrow = TRUE, dimnames = list(c("Adjective", "Non-Adjective"), c("Female", "Male")))
> chisq_test <- chisq.test(observed)
> std_residuals <- round(chisq_test$stdres, 2)
> print(chisq_test)

	Pearson's Chi-squared test with Yates' continuity correction

data:  observed
X-squared = 29.169, df = 1, p-value = 6.632e-08

> print(std_residuals)
              Female  Male
Adjective       5.41 -5.41
Non-Adjective  -5.41  5.41
> write.csv(std_residuals, "good_std_residuals.csv", row.names = TRUE)
>
> > old_data <- data[data$Item == "old" & data$Part_of_Speech == "JJ", ]
> female_count <- sum(old_data$Count[old_data$Source == "female"])
> male_count <- sum(old_data$Count[old_data$Source == "male"])
> female_non_adj_count <- sum(data$Count[data$Source == "female"]) - female_count
> male_non_adj_count <- sum(data$Count[data$Source == "male"]) - male_count
> observed <- matrix(c(female_count, male_count, female_non_adj_count, male_non_adj_count), nrow = 2, byrow = TRUE, dimnames = list(c("Adjective", "Non-Adjective"), c("Female", "Male")))
> chisq_test <- chisq.test(observed)
> std_residuals <- round(chisq_test$stdres, 2)
> print(chisq_test)

	Pearson's Chi-squared test with Yates' continuity correction

data:  observed
X-squared = 382.1, df = 1, p-value < 2.2e-16

> print(std_residuals)
              Female   Male
Adjective      19.55 -19.55
Non-Adjective -19.55  19.55
> write.csv(std_residuals, "old_std_residuals.csv", row.names = TRUE)
>
> own_data <- data[data$Item == "own" & data$Part_of_Speech == "JJ", ]
> female_count <- sum(own_data$Count[own_data$Source == "female"])
> male_count <- sum(own_data$Count[own_data$Source == "male"])
> female_non_adj_count <- sum(data$Count[data$Source == "female"]) - female_count
> male_non_adj_count <- sum(data$Count[data$Source == "male"]) - male_count
> observed <- matrix(c(female_count, male_count, female_non_adj_count, male_non_adj_count), nrow = 2, byrow = TRUE, dimnames = list(c("Adjective", "Non-Adjective"), c("Female", "Male")))
> chisq_test <- chisq.test(observed)
> std_residuals <- round(chisq_test$stdres, 2)
> print(chisq_test)
> print(std_residuals)
> write.csv(std_residuals, "own_std_residuals.csv", row.names = TRUE)
>
> 
