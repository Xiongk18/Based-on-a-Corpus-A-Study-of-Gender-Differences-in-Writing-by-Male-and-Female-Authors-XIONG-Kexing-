> library(data.table)
> file_path <- "~/Desktop/tagged_texts(female author).txt"
> text_data <- readLines(file_path)
> text_data <- gsub("[[:punct:]&&[^/]]", "", text_data)
> word_tag_pairs <- unlist(strsplit(text_data, "\\s+"))
> valid_word_tag_pairs <- word_tag_pairs[grepl("/", word_tag_pairs)]
> valid_word_tag_pairs <- valid_word_tag_pairs[!grepl("[^a-zA-Z/]", valid_word_tag_pairs)]
> print(head(valid_word_tag_pairs, 20))
> word_tag_dt <- rbindlist(lapply(valid_word_tag_pairs, function(x) {
  word_tag <- unlist(strsplit(x, "/"))
  if(length(word_tag) >= 2 && word_tag[1] != "" && !is.na(word_tag[1])) {
    list(Word = word_tag[1], POS = word_tag[2])
  } else {
    list(Word = NA, POS = NA)
  }
}))
> word_tag_dt <- na.omit(word_tag_dt)
> print(word_tag_dt)
> word_count_dt <- word_tag_dt[, .(Count = .N), by = .(Word, POS)]
> print(word_count_dt)
> total_words <- sum(word_count_dt$Count)
> word_count_dt[, Complement_Count := total_words - Count]
> print(word_count_dt)
> word_count_dt[, Relative_Frequency := Count / total_words]
> print(word_count_dt)
> word_count_dt[, Source := "female"]
> print(word_count_dt)
> setnames(word_count_dt, c("Word", "POS"), c("Item", "Part_of_Speech"))
> print(word_count_dt)
> final_table_dt <- word_count_dt[, .(Source, Item, Count, Complement_Count, Relative_Frequency, Part_of_Speech)]
> print(final_table_dt)
> final_table_dt <- final_table_dt[!(is.na(Item) | Item == "")]
> print(final_table_dt)
> fwrite(final_table_dt, "~/Desktop/female_author_word_analysis.csv")
>
> library(data.table)
> file_path <- "~/Desktop/tagged_texts(male author).txt"
> text_data <- readLines(file_path)
> text_data <- gsub("[[:punct:]&&[^/]]", "", text_data)
> word_tag_pairs <- unlist(strsplit(text_data, "\\s+"))
> valid_word_tag_pairs <- word_tag_pairs[grepl("/", word_tag_pairs)]
> valid_word_tag_pairs <- valid_word_tag_pairs[!grepl("[^a-zA-Z/]", valid_word_tag_pairs)]
> print(head(valid_word_tag_pairs, 20))
> word_tag_dt <- rbindlist(lapply(valid_word_tag_pairs, function(x) {
  word_tag <- unlist(strsplit(x, "/"))
  if(length(word_tag) >= 2 && word_tag[1] != "" && !is.na(word_tag[1])) {
    list(Word = word_tag[1], POS = word_tag[2])
  } else {
    list(Word = NA, POS = NA)
  }
}))
> word_tag_dt <- na.omit(word_tag_dt)
> print(word_tag_dt)
> word_count_dt <- word_tag_dt[, .(Count = .N), by = .(Word, POS)]
> print(word_count_dt)
> total_words <- sum(word_count_dt$Count)
> word_count_dt[, Complement_Count := total_words - Count]
> print(word_count_dt)
> word_count_dt[, Relative_Frequency := Count / total_words]
> print(word_count_dt)
> word_count_dt[, Source := "male"]
> print(word_count_dt)
> setnames(word_count_dt, c("Word", "POS"), c("Item", "Part_of_Speech"))
> print(word_count_dt)
> final_table_dt <- word_count_dt[, .(Source, Item, Count, Complement_Count, Relative_Frequency, Part_of_Speech)]
> print(final_table_dt)
> final_table_dt <- final_table_dt[!(is.na(Item) | Item == "")]
> print(final_table_dt)
> write(final_table_dt, "~/Desktop/male_author_word_analysis.csv")
>
> library(data.table)
> female_data <- fread("~/Desktop/female_author_word_analysis2.csv")
> male_data <- fread("~/Desktop/male_author_word_analysis.csv")
> combined_data <- rbind(female_data, male_data)
> print(combined_data)
> fwrite(combined_data, "~/Desktop/combined_author_word_analysis.csv")
>
> library(data.table)
> female_data <- fread("~/Desktop/female_author_word_analysis2.csv")
> male_data <- fread("~/Desktop/male_author_word_analysis.csv")
> female_data[, Item := tolower(Item)]
> male_data[, Item := tolower(Item)]
> female_data <- female_data[, .(Count = sum(Count), 
                               Complement_Count = sum(Complement_Count),
                               Relative_Frequency = sum(Relative_Frequency)), 
                           by = .(Source, Item, Part_of_Speech)]
> male_data <- male_data[, .(Count = sum(Count), 
                           Complement_Count = sum(Complement_Count),
                           Relative_Frequency = sum(Relative_Frequency)), 
                       by = .(Source, Item, Part_of_Speech)]
> combined_data <- rbind(female_data, male_data)
> print(combined_data)
> fwrite(combined_data, "~/Desktop/combined_author_word_analysis.csv")
>
> setwd("/Users/mac/Desktop")
> data <- read.csv("combined_author_word_analysis(New).csv")
> low_freq_data <- data[data$Count >= 1 & data$Count <= 5, ]
> high_freq_data <- data[data$Count > 5, ]
> write.csv(high_freq_data, "high_freq_words.csv", row.names = FALSE)
> write.csv(low_freq_data, "low_freq_words.csv", row.names = FALSE)
>
> setwd("/Users/mac/Desktop")
> data <- read.csv("combined_author_word_analysis(New).csv")
> adj_data <- data[data$Part_of_Speech == "JJ", ]
> library(dplyr)
> top10_adj <- adj_data %>%
  group_by(Source) %>%
  arrange(desc(Count)) %>%
  slice_head(n = 10) %>%
  select(Source, Item, Count, Complement_Count)
> write.csv(top10_adj, "top10_adjectives_analysis.csv", row.names = FALSE)
> library(ggplot2)
> ggplot(top10_adj, aes(x = reorder(Item, -Relative_Frequency), y = Relative_Frequency, fill = Source)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("male" = "#0be4ff", "female" = "#4fff76")) +
  labs(title = "Top 10 Adjectives Frequency Comparison",
       x = "Adjectives",
       y = "Relative Frequency",
       fill = "Source") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5))
> ggsave("top10_adjectives_comparison.png", width = 10, height = 6)

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
> other_data <- data[data$Item == "other" & data$Part_of_Speech == "JJ", ]
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
> good_data <- data[data$Item == "good" & data$Part_of_Speech == "JJ", ]
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
> old_data <- data[data$Item == "old" & data$Part_of_Speech == "JJ", ]
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

	Pearson's Chi-squared test with Yates' continuity correction

data:  observed
X-squared = 38.809, df = 1, p-value = 4.675e-10

> print(std_residuals)
              Female  Male
Adjective       6.24 -6.24
Non-Adjective  -6.24  6.24
> write.csv(std_residuals, "own_std_residuals.csv", row.names = TRUE)
>
> great_data <- data[data$Item == "great" & data$Part_of_Speech == "JJ", ]
> female_count <- sum(great_data$Count[great_data$Source == "female"])
> male_count <- sum(great_data$Count[great_data$Source == "male"])
> female_non_adj_count <- sum(data$Count[data$Source == "female"]) - female_count
> male_non_adj_count <- sum(data$Count[data$Source == "male"]) - male_count
> observed <- matrix(c(female_count, male_count, female_non_adj_count, male_non_adj_count), nrow = 2, byrow = TRUE, dimnames = list(c("Adjective", "Non-Adjective"), c("Female", "Male")))
> chisq_test <- chisq.test(observed)
> std_residuals <- round(chisq_test$stdres, 2)
> print(chisq_test)

	Pearson's Chi-squared test with Yates' continuity correction

data:  observed
X-squared = 30.183, df = 1, p-value = 3.931e-08

> print(std_residuals)
              Female Male
Adjective       -5.5  5.5
Non-Adjective    5.5 -5.5
> write.csv(std_residuals, "great_std_residuals.csv", row.names = TRUE)
>
> such_data <- data[data$Item == "such" & data$Part_of_Speech == "JJ", ]
> female_count <- sum(such_data$Count[such_data$Source == "female"])
> male_count <- sum(such_data$Count[such_data$Source == "male"])
> female_non_adj_count <- sum(data$Count[data$Source == "female"]) - female_count
> male_non_adj_count <- sum(data$Count[data$Source == "male"]) - male_count
> observed <- matrix(c(female_count, male_count, female_non_adj_count, male_non_adj_count), nrow = 2, byrow = TRUE, dimnames = list(c("Adjective", "Non-Adjective"), c("Female", "Male")))
> chisq_test <- chisq.test(observed)
> std_residuals <- round(chisq_test$stdres, 2)
> print(chisq_test)

	Pearson's Chi-squared test with Yates' continuity correction

data:  observed
X-squared = 245.62, df = 1, p-value < 2.2e-16

> print(std_residuals)
              Female   Male
Adjective     -15.68  15.68
Non-Adjective  15.68 -15.68
> write.csv(std_residuals, "such_std_residuals.csv", row.names = TRUE)
>
> same_data <- data[data$Item == "same" & data$Part_of_Speech == "JJ", ]
> female_count <- sum(same_data$Count[same_data$Source == "female"])
> male_count <- sum(same_data$Count[same_data$Source == "male"])
> female_non_adj_count <- sum(data$Count[data$Source == "female"]) - female_count
> male_non_adj_count <- sum(data$Count[data$Source == "male"]) - male_count
> observed <- matrix(c(female_count, male_count, female_non_adj_count, male_non_adj_count), nrow = 2, byrow = TRUE, dimnames = list(c("Adjective", "Non-Adjective"), c("Female", "Male")))
> chisq_test <- chisq.test(observed)
> std_residuals <- round(chisq_test$stdres, 2)
> print(chisq_test)

	Pearson's Chi-squared test with Yates' continuity correction

data:  observed
X-squared = 694.59, df = 1, p-value < 2.2e-16

> print(std_residuals)
              Female   Male
Adjective     -26.36  26.36
Non-Adjective  26.36 -26.36
> write.csv(std_residuals, "same_std_residuals.csv", row.names = TRUE)
>
> much_data <- data[data$Item == "much" & data$Part_of_Speech == "JJ", ]
> female_count <- sum(much_data$Count[much_data$Source == "female"])
> male_count <- sum(much_data$Count[much_data$Source == "male"])
> female_non_adj_count <- sum(data$Count[data$Source == "female"]) - female_count
> male_non_adj_count <- sum(data$Count[data$Source == "male"]) - male_count
> observed <- matrix(c(female_count, male_count, female_non_adj_count, male_non_adj_count), nrow = 2, byrow = TRUE, dimnames = list(c("Adjective", "Non-Adjective"), c("Female", "Male")))
> chisq_test <- chisq.test(observed)
> std_residuals <- round(chisq_test$stdres, 2)
> print(chisq_test)

	Pearson's Chi-squared test with Yates' continuity correction

data:  observed
X-squared = 207.63, df = 1, p-value < 2.2e-16

> print(std_residuals)
              Female   Male
Adjective      14.42 -14.42
Non-Adjective -14.42  14.42
> write.csv(std_residuals, "much_std_residuals.csv", row.names = TRUE)
>
> last_data <- data[data$Item == "last" & data$Part_of_Speech == "JJ", ]
> female_count <- sum(last_data$Count[last_data$Source == "female"])
> male_count <- sum(last_data$Count[last_data$Source == "male"])
> female_non_adj_count <- sum(data$Count[data$Source == "female"]) - female_count
> male_non_adj_count <- sum(data$Count[data$Source == "male"]) - male_count
> observed <- matrix(c(female_count, male_count, female_non_adj_count, male_non_adj_count), nrow = 2, byrow = TRUE, dimnames = list(c("Adjective", "Non-Adjective"), c("Female", "Male")))
> chisq_test <- chisq.test(observed)
> std_residuals <- round(chisq_test$stdres, 2)
> print(chisq_test)

	Pearson's Chi-squared test with Yates' continuity correction

data:  observed
X-squared = 43.604, df = 1, p-value = 4.021e-11

> print(std_residuals)
              Female  Male
Adjective       6.61 -6.61
Non-Adjective  -6.61  6.61
> write.csv(std_residuals, "last_std_residuals.csv", row.names = TRUE)
>
> first_data <- data[data$Item == "first" & data$Part_of_Speech == "JJ", ]
> female_count <- sum(first_data$Count[first_data$Source == "female"])
> male_count <- sum(first_data$Count[first_data$Source == "male"])
> female_non_adj_count <- sum(data$Count[data$Source == "female"]) - female_count
> male_non_adj_count <- sum(data$Count[data$Source == "male"]) - male_count
> observed <- matrix(c(female_count, male_count, female_non_adj_count, male_non_adj_count), nrow = 2, byrow = TRUE, dimnames = list(c("Adjective", "Non-Adjective"), c("Female", "Male")))
> chisq_test <- chisq.test(observed)
> std_residuals <- round(chisq_test$stdres, 2)
> print(chisq_test)

	Pearson's Chi-squared test with Yates' continuity correction

data:  observed
X-squared = 0.047195, df = 1, p-value = 0.828

> print(std_residuals)
              Female  Male
Adjective      -0.23  0.23
Non-Adjective   0.23 -0.23
> write.csv(std_residuals, "first_std_residuals.csv", row.names = TRUE)
>
> library(dplyr)  
> setwd("/Users/mac/Desktop")
> data <- read.csv("high and medium_freq_words.csv")
> adj_data <- data[data$Part_of_Speech == "JJ", ]
> quantiles <- quantile(adj_data$Relative_Frequency, probs = c(0.25, 0.75))
> mid_freq_adj_data <- adj_data[adj_data$Relative_Frequency >= quantiles[1] & adj_data$Relative_Frequency <= quantiles[2], ]
> random_adj_female <- mid_freq_adj_data %>%
+   filter(Source == "female") %>%
+   sample_n(5) %>%
+   select(Source, Item, Count, Complement_Count)
> random_adj_male <- mid_freq_adj_data %>%
+   filter(Source == "male") %>%
+   sample_n(5) %>%
+   select(Source, Item, Count, Complement_Count)
> print(random_adj_female)
  Source       Item Count Complement_Count
1 female  deceptive     9          7866020
2 female     benign    11          7866018
3 female  inscrutable    37          7865992
4 female noteworthy     9          7866020
5 female       fun    10          7866019
> print(random_adj_male)
  Source       Item Count Complement_Count
1   male phlegmatic    13          9600082
2   male  forceful    11          9600084
3   male vivacious    20          9600075
4   male associate    11          9600084
5   male     domed    10          9600085
> write.csv(random_adj_female, "random_mid_freq_adj_female_25_75.csv", row.names = FALSE)
> write.csv(random_adj_male, "random_mid_freq_adj_male_25_75.csv", row.names = FALSE)
>
> setwd("/Users/mac/Desktop")
> data <- read.csv("combined_author_word_analysis(New).csv")
> deceptive_data <- data[data$Item == "deceptive" & data$Part_of_Speech == "JJ", ]
> female_count <- sum(deceptive_data$Count[deceptive_data$Source == "female"])
> male_count <- sum(deceptive_data$Count[deceptive_data$Source == "male"])
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
X-squared = 0.28826, df = 1, p-value = 0.5913

> print(std_residuals)
              Female  Male
Adjective      -0.74  0.74
Non-Adjective   0.74 -0.74
> write.csv(std_residuals, "deceptive_std_residuals.csv", row.names = TRUE)
>
> benign_data <- data[data$Item == "benign" & data$Part_of_Speech == "JJ", ]
> female_count <- sum(benign_data$Count[benign_data$Source == "female"])
> male_count <- sum(benign_data$Count[benign_data$Source == "male"])
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
X-squared = 2.0972, df = 1, p-value = 0.1476

> print(std_residuals)
              Female  Male
Adjective      -1.62  1.62
Non-Adjective   1.62 -1.62
> write.csv(std_residuals, "benign_std_residuals.csv", row.names = TRUE)
>
> inscrutable_data <- data[data$Item == "inscrutable" & data$Part_of_Speech == "JJ", ]
> female_count <- sum(inscrutable_data$Count[inscrutable_data$Source == "female"])
> male_count <- sum(inscrutable_data$Count[inscrutable_data$Source == "male"])
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
X-squared = 0.27456, df = 1, p-value = 0.6003

> print(std_residuals)
              Female  Male
Adjective       0.64 -0.64
Non-Adjective  -0.64  0.64
> write.csv(std_residuals, "inscrutable_std_residuals.csv", row.names = TRUE)
>
> noteworthy_data <- data[data$Item == "noteworthy" & data$Part_of_Speech == "JJ", ]
> female_count <- sum(noteworthy_data$Count[noteworthy_data$Source == "female"])
> male_count <- sum(noteworthy_data$Count[noteworthy_data$Source == "male"])
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
X-squared = 8.5263, df = 1, p-value = 0.0035

> print(std_residuals)
              Female  Male
Adjective      -3.08  3.08
Non-Adjective   3.08 -3.08
> write.csv(std_residuals, "noteworthy_std_residuals.csv", row.names = TRUE)
>
> fun_data <- data[data$Item == "fun" & data$Part_of_Speech == "JJ", ]
> female_count <- sum(fun_data$Count[fun_data$Source == "female"])
> male_count <- sum(fun_data$Count[fun_data$Source == "male"])
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
X-squared = 0.049057, df = 1, p-value = 0.8247

> print(std_residuals)
              Female  Male
Adjective       0.45 -0.45
Non-Adjective  -0.45  0.45
> write.csv(std_residuals, "fun_std_residuals.csv", row.names = TRUE)
>
> phlegmatic_data <- data[data$Item == "phlegmatic" & data$Part_of_Speech == "JJ", ]
> female_count <- sum(phlegmatic_data$Count[phlegmatic_data$Source == "female"])
> male_count <- sum(phlegmatic_data$Count[phlegmatic_data$Source == "male"])
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
X-squared = 1.2033, df = 1, p-value = 0.2727

> print(std_residuals)
              Female  Male
Adjective       1.28 -1.28
Non-Adjective  -1.28  1.28
> write.csv(std_residuals, "phlegmatic_std_residuals.csv", row.names = TRUE)
>
> forceful_data <- data[data$Item == "forceful" & data$Part_of_Speech == "JJ", ]
> female_count <- sum(forceful_data$Count[forceful_data$Source == "female"])
> male_count <- sum(forceful_data$Count[forceful_data$Source == "male"])
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
X-squared = 0.082547, df = 1, p-value = 0.7739

> print(std_residuals)
              Female  Male
Adjective      -0.52  0.52
Non-Adjective   0.52 -0.52
> write.csv(std_residuals, "forceful_std_residuals.csv", row.names = TRUE)
>
> vivacious_data <- data[data$Item == "vivacious" & data$Part_of_Speech == "JJ", ]
> female_count <- sum(vivacious_data$Count[vivacious_data$Source == "female"])
> male_count <- sum(vivacious_data$Count[vivacious_data$Source == "male"])
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
X-squared = 2.0095, df = 1, p-value = 0.1563

> print(std_residuals)
              Female  Male
Adjective       1.57 -1.57
Non-Adjective  -1.57  1.57
> write.csv(std_residuals, "vivacious_std_residuals.csv", row.names = TRUE)
>
> associate_data <- data[data$Item == "associate" & data$Part_of_Speech == "JJ", ]
> female_count <- sum(associate_data$Count[associate_data$Source == "female"])
> male_count <- sum(associate_data$Count[associate_data$Source == "male"])
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
X-squared = 2.2704, df = 1, p-value = 0.1319

> print(std_residuals)
              Female  Male
Adjective      -1.78  1.78
Non-Adjective   1.78 -1.78
> write.csv(std_residuals, "associate_std_residuals.csv", row.names = TRUE)
>
> domed_data <- data[data$Item == "domed" & data$Part_of_Speech == "JJ", ]
> female_count <- sum(domed_data$Count[domed_data$Source == "female"])
> male_count <- sum(domed_data$Count[domed_data$Source == "male"])
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
X-squared = 0.94016, df = 1, p-value = 0.3322

> print(std_residuals)
              Female  Male
Adjective      -1.24  1.24
Non-Adjective   1.24 -1.24
> write.csv(std_residuals, "domed_std_residuals.csv", row.names = TRUE)
>
> setwd("/Users/mac/Desktop")
> data <- read.csv("combined_author_word_analysis(New).csv")
> adj_data <- data[data$Part_of_Speech == "JJ", ]
> adj_data$Singleton <- adj_data$Count == 1
> contingency_table <- table(adj_data$Singleton, adj_data$Source)
> print(contingency_table)
       
        female  male
  FALSE   9224 11678
  TRUE    5960  9125
> write.csv(as.data.frame(contingency_table), "singleton_vs_reuse_adjectives.csv", row.names = TRUE)
> chisq_test <- chisq.test(contingency_table)
> print(chisq_test)

	Pearson's Chi-squared test with Yates' continuity correction

data:  contingency_table
X-squared = 76.495, df = 1, p-value < 2.2e-16

> std_residuals <- round(chisq_test$stdres, 6)
> print(std_residuals)
         Female      Male
FALSE  8.756966 -8.756966
TRUE  -8.756966  8.756966
> write.csv(std_residuals, "standard_residuals_singleton_vs_reuse.csv", row.names = TRUE)
>
> setwd("/Users/mac/Desktop")
> data <- read.csv("combined_author_word_analysis(New).csv")
> noun_data <- data[data$Part_of_Speech == "NN", ]
> noun_data <- noun_data %>%
+   mutate(Relative_Frequency = Count / Complement_Count)
> filtered_noun_data <- noun_data[!noun_data$Item %in% c("d", "t", "s"), ]
> library(dplyr)
> top10_noun <- filtered_noun_data %>%
+   group_by(Source) %>%
+   arrange(desc(Count)) %>%
+   slice_head(n = 10) %>%
+   select(Source, Item, Count, Complement_Count, Relative_Frequency)
> write.csv(top10_noun, "3top10_nouns_filtered_analysis.csv", row.names = FALSE)
> library(ggplot2)
> ggplot(top10_noun, aes(x = reorder(Item, -Relative_Frequency), y = Relative_Frequency, fill = Source)) +
+   geom_bar(stat = "identity", position = "dodge") +
+   scale_fill_manual(values = c("male" = "#0be4ff", "female" = "#4fff76")) +
+   labs(title = "Top 10 Nouns Frequency Comparison (Filtered)",
+        x = "Nouns",
+        y = "Relative Frequency",
+        fill = "Source") +
+   theme_minimal() +
+   theme(axis.text.x = element_text(angle = 45, hjust = 1),
+         plot.title = element_text(hjust = 0.5))
> ggsave("top10_nouns_filtered_comparison.png", width = 10, height = 6)
>
> setwd("/Users/mac/Desktop")
> data <- read.csv("combined_author_word_analysis(New).csv")
> man_data <- data[data$Item == "man" & data$Part_of_Speech == "NN", ]
> female_count <- sum(man_data$Count[man_data$Source == "female"])
> male_count <- sum(man_data$Count[man_data$Source == "male"])
> female_non_noun_count <- sum(data$Count[data$Source == "female"]) - female_count
> male_non_noun_count <- sum(data$Count[data$Source == "male"]) - male_count
> observed <- matrix(c(female_count, male_count, female_non_noun_count, male_non_noun_count),
+                    nrow = 2,
+                    byrow = TRUE,
+                    dimnames = list(c("Noun", "Non-Noun"),
+                                    c("Female", "Male")))
> chisq_test <- chisq.test(observed)
> std_residuals <- chisq_test$stdres
> print(chisq_test)

	Pearson's Chi-squared test with Yates' continuity correction

data:  observed
X-squared = 762.44, df = 1, p-value < 2.2e-16

> print(std_residuals)
            Female      Male
Noun     -27.61798  27.61798
Non-Noun  27.61798 -27.61798
> write.csv(std_residuals, "man_std_residuals.csv", row.names = TRUE)
>
> setwd("/Users/mac/Desktop")
> data <- read.csv("combined_author_word_analysis(New).csv")
> time_data <- data[data$Item == "time" & data$Part_of_Speech == "NN", ]
> female_count <- sum(time_data$Count[time_data$Source == "female"])
> male_count <- sum(time_data$Count[time_data$Source == "male"])
> female_non_noun_count <- sum(data$Count[data$Source == "female"]) - female_count
> male_non_noun_count <- sum(data$Count[data$Source == "male"]) - male_count
> observed <- matrix(c(female_count, male_count, female_non_noun_count, male_non_noun_count),
+                    nrow = 2,
+                    byrow = TRUE,
+                    dimnames = list(c("Noun", "Non-Noun"),
+                                    c("Female", "Male")))
> chisq_test <- chisq.test(observed)
> std_residuals <- round(chisq_test$stdres, 2)
> print(chisq_test)

	Pearson's Chi-squared test with Yates' continuity correction

data:  observed
X-squared = 11.664, df = 1, p-value = 0.0006371

> print(std_residuals)
         Female  Male
Noun       3.42 -3.42
Non-Noun  -3.42  3.42
> write.csv(std_residuals, "time_std_residuals.csv", row.names = TRUE)
>
> day_data <- data[data$Item == "day" & data$Part_of_Speech == "NN", ]
> female_count <- sum(day_data$Count[day_data$Source == "female"])
> male_count <- sum(day_data$Count[day_data$Source == "male"])
> female_non_noun_count <- sum(data$Count[data$Source == "female"]) - female_count
> male_non_noun_count <- sum(data$Count[data$Source == "male"]) - male_count
> observed <- matrix(c(female_count, male_count, female_non_noun_count, male_non_noun_count),
+                    nrow = 2,
+                    byrow = TRUE,
+                    dimnames = list(c("Noun", "Non-Noun"),
+                                    c("Female", "Male")))
> chisq_test <- chisq.test(observed)
> std_residuals <- round(chisq_test$stdres, 2)
> print(chisq_test)

	Pearson's Chi-squared test with Yates' continuity correction

data:  observed
X-squared = 83.827, df = 1, p-value < 2.2e-16

> print(std_residuals)
         Female  Male
Noun       9.16 -9.16
Non-Noun  -9.16  9.16
> write.csv(std_residuals, "day_std_residuals.csv", row.names = TRUE)
>
> way_data <- data[data$Item == "way" & data$Part_of_Speech == "NN", ]
> female_count <- sum(way_data$Count[way_data$Source == "female"])
> male_count <- sum(way_data$Count[way_data$Source == "male"])
> female_non_noun_count <- sum(data$Count[data$Source == "female"]) - female_count
> male_non_noun_count <- sum(data$Count[data$Source == "male"]) - male_count
> observed <- matrix(c(female_count, male_count, female_non_noun_count, male_non_noun_count),
+                    nrow = 2,
+                    byrow = TRUE,
+                    dimnames = list(c("Noun", "Non-Noun"),
+                                    c("Female", "Male")))
> chisq_test <- chisq.test(observed)
> std_residuals <- round(chisq_test$stdres, 2)
> print(chisq_test)

	Pearson's Chi-squared test with Yates' continuity correction

data:  observed
X-squared = 68.442, df = 1, p-value < 2.2e-16

> print(std_residuals)
         Female  Male
Noun       8.28 -8.28
Non-Noun  -8.28  8.28
> write.csv(std_residuals, "way_std_residuals.csv", row.names = TRUE)
>
> life_data <- data[data$Item == "life" & data$Part_of_Speech == "NN", ]
> female_count <- sum(life_data$Count[life_data$Source == "female"])
> male_count <- sum(life_data$Count[life_data$Source == "male"])
> female_non_noun_count <- sum(data$Count[data$Source == "female"]) - female_count
> male_non_noun_count <- sum(data$Count[data$Source == "male"]) - male_count
> observed <- matrix(c(female_count, male_count, female_non_noun_count, male_non_noun_count),
+                    nrow = 2,
+                    byrow = TRUE,
+                    dimnames = list(c("Noun", "Non-Noun"),
+                                    c("Female", "Male")))
> chisq_test <- chisq.test(observed)
> std_residuals <- round(chisq_test$stdres, 2)
> print(chisq_test)

	Pearson's Chi-squared test with Yates' continuity correction

data:  observed
X-squared = 2.0921, df = 1, p-value = 0.1481

> print(std_residuals)
         Female  Male
Noun       1.45 -1.45
Non-Noun  -1.45  1.45
> write.csv(std_residuals, "life_std_residuals.csv", row.names = TRUE)
>
> hand_data <- data[data$Item == "hand" & data$Part_of_Speech == "NN", ]
> female_count <- sum(hand_data$Count[hand_data$Source == "female"])
> male_count <- sum(hand_data$Count[hand_data$Source == "male"])
> female_non_noun_count <- sum(data$Count[data$Source == "female"]) - female_count
> male_non_noun_count <- sum(data$Count[data$Source == "male"]) - male_count
> observed <- matrix(c(female_count, male_count, female_non_noun_count, male_non_noun_count),
+                    nrow = 2,
+                    byrow = TRUE,
+                    dimnames = list(c("Noun", "Non-Noun"),
+                                    c("Female", "Male")))
> chisq_test <- chisq.test(observed)
> std_residuals <- round(chisq_test$stdres, 2)
> print(chisq_test)

	Pearson's Chi-squared test with Yates' continuity correction

data:  observed
X-squared = 13.695, df = 1, p-value = 0.000215

> print(std_residuals)
         Female  Male
Noun       3.71 -3.71
Non-Noun  -3.71  3.71
> write.csv(std_residuals, "hand_std_residuals.csv", row.names = TRUE)
>
> face_data <- data[data$Item == "face" & data$Part_of_Speech == "NN", ]
> female_count <- sum(face_data$Count[face_data$Source == "female"])
> male_count <- sum(face_data$Count[face_data$Source == "male"])
> female_non_noun_count <- sum(data$Count[data$Source == "female"]) - female_count
> male_non_noun_count <- sum(data$Count[data$Source == "male"]) - male_count
> observed <- matrix(c(female_count, male_count, female_non_noun_count, male_non_noun_count),
+                    nrow = 2,
+                    byrow = TRUE,
+                    dimnames = list(c("Noun", "Non-Noun"),
+                                    c("Female", "Male")))
> chisq_test <- chisq.test(observed)
> std_residuals <- round(chisq_test$stdres, 2)
> print(chisq_test)

	Pearson's Chi-squared test with Yates' continuity correction

data:  observed
X-squared = 616.29, df = 1, p-value < 2.2e-16

> print(std_residuals)
         Female   Male
Noun      24.83 -24.83
Non-Noun -24.83  24.83
> write.csv(std_residuals, "face_std_residuals.csv", row.names = TRUE)
>
> nothing_data <- data[data$Item == "nothing" & data$Part_of_Speech == "NN", ]
> female_count <- sum(nothing_data$Count[nothing_data$Source == "female"])
> male_count <- sum(nothing_data$Count[nothing_data$Source == "male"])
> female_non_noun_count <- sum(data$Count[data$Source == "female"]) - female_count
> male_non_noun_count <- sum(data$Count[data$Source == "male"]) - male_count
> observed <- matrix(c(female_count, male_count, female_non_noun_count, male_non_noun_count),
+                    nrow = 2,
+                    byrow = TRUE,
+                    dimnames = list(c("Noun", "Non-Noun"),
+                                    c("Female", "Male")))
> chisq_test <- chisq.test(observed)
> std_residuals <- round(chisq_test$stdres, 2)
> print(chisq_test)

	Pearson's Chi-squared test with Yates' continuity correction

data:  observed
X-squared = 40.421, df = 1, p-value = 2.048e-10

> print(std_residuals)
         Female  Male
Noun       6.37 -6.37
Non-Noun  -6.37  6.37
> write.csv(std_residuals, "nothing_std_residuals.csv", row.names = TRUE)
>
> house_data <- data[data$Item == "house" & data$Part_of_Speech == "NN", ]
> female_count <- sum(house_data$Count[house_data$Source == "female"])
> male_count <- sum(house_data$Count[house_data$Source == "male"])
> female_non_noun_count <- sum(data$Count[data$Source == "female"]) - female_count
> male_non_noun_count <- sum(data$Count[data$Source == "male"]) - male_count
> observed <- matrix(c(female_count, male_count, female_non_noun_count, male_non_noun_count),
+                    nrow = 2,
+                    byrow = TRUE,
+                    dimnames = list(c("Noun", "Non-Noun"),
+                                    c("Female", "Male")))
> chisq_test <- chisq.test(observed)
> std_residuals <- round(chisq_test$stdres, 2)
> print(chisq_test)

	Pearson's Chi-squared test with Yates' continuity correction

data:  observed
X-squared = 205.26, df = 1, p-value < 2.2e-16

> print(std_residuals)
         Female   Male
Noun      14.34 -14.34
Non-Noun -14.34  14.34
> write.csv(std_residuals, "house_std_residuals.csv", row.names = TRUE)
>
> mother_data <- data[data$Item == "mother" & data$Part_of_Speech == "NN", ]
> female_count <- sum(mother_data$Count[mother_data$Source == "female"])
> male_count <- sum(mother_data$Count[mother_data$Source == "male"])
> female_non_noun_count <- sum(data$Count[data$Source == "female"]) - female_count
> male_non_noun_count <- sum(data$Count[data$Source == "male"]) - male_count
> observed <- matrix(c(female_count, male_count, female_non_noun_count, male_non_noun_count),
+                    nrow = 2,
+                    byrow = TRUE,
+                    dimnames = list(c("Noun", "Non-Noun"),
+                                    c("Female", "Male")))
> chisq_test <- chisq.test(observed)
> std_residuals <- round(chisq_test$stdres, 2)
> print(chisq_test)

	Pearson's Chi-squared test with Yates' continuity correction

data:  observed
X-squared = 752.03, df = 1, p-value < 2.2e-16

> print(std_residuals)
         Female   Male
Noun      27.43 -27.43
Non-Noun -27.43  27.43
> write.csv(std_residuals, "mother_std_residuals.csv", row.names = TRUE)
>
> head_data <- data[data$Item == "head" & data$Part_of_Speech == "NN", ]
> female_count <- sum(head_data$Count[head_data$Source == "female"])
> male_count <- sum(head_data$Count[head_data$Source == "male"])
> female_non_noun_count <- sum(data$Count[data$Source == "female"]) - female_count
> male_non_noun_count <- sum(data$Count[data$Source == "male"]) - male_count
> observed <- matrix(c(female_count, male_count, female_non_noun_count, male_non_noun_count),
+                    nrow = 2,
+                    byrow = TRUE,
+                    dimnames = list(c("Noun", "Non-Noun"),
+                                    c("Female", "Male")))
> chisq_test <- chisq.test(observed)
> std_residuals <- round(chisq_test$stdres, 2)
> print(chisq_test)

	Pearson's Chi-squared test with Yates' continuity correction

data:  observed
X-squared = 22.151, df = 1, p-value = 2.52e-06

> print(std_residuals)
         Female  Male
Noun       4.72 -4.72
Non-Noun  -4.72  4.72
> write.csv(std_residuals, "head_std_residuals.csv", row.names = TRUE)
>
> place_data <- data[data$Item == "place" & data$Part_of_Speech == "NN", ]
> female_count <- sum(place_data$Count[place_data$Source == "female"])
> male_count <- sum(place_data$Count[place_data$Source == "male"])
> female_non_noun_count <- sum(data$Count[data$Source == "female"]) - female_count
> male_non_noun_count <- sum(data$Count[data$Source == "male"]) - male_count
> observed <- matrix(c(female_count, male_count, female_non_noun_count, male_non_noun_count),
+                    nrow = 2,
+                    byrow = TRUE,
+                    dimnames = list(c("Noun", "Non-Noun"),
+                                    c("Female", "Male")))
> chisq_test <- chisq.test(observed)
> std_residuals <- round(chisq_test$stdres, 2)
> print(chisq_test)

	Pearson's Chi-squared test with Yates' continuity correction

data:  observed
X-squared = 3.7374, df = 1, p-value = 0.05321

> print(std_residuals)
         Female  Male
Noun      -1.94  1.94
Non-Noun   1.94 -1.94
> write.csv(std_residuals, "place_std_residuals.csv", row.names = TRUE)
>
> library(dplyr) 
> setwd("/Users/mac/Desktop")
> data <- read.csv("high and medium_freq_words.csv")
> noun_data <- data[data$Part_of_Speech == "NN", ]
> quantiles <- quantile(noun_data$Relative_Frequency, probs = c(0.25, 0.75))
> mid_freq_noun_data <- noun_data[noun_data$Relative_Frequency >= quantiles[1] & noun_data$Relative_Frequency <= quantiles[2], ]
> random_noun_female <- mid_freq_noun_data %>%
+   filter(Source == "female") %>%
+   sample_n(5) %>%
+   select(Source, Item, Count, Complement_Count)
> random_noun_male <- mid_freq_noun_data %>%
+   filter(Source == "male") %>%
+   sample_n(5) %>%
+   select(Source, Item, Count, Complement_Count)
> print(random_noun_female)
  Source        Item Count Complement_Count
1 female        damn    27          7866002
2 female complacency    50          7865979
3 female     license    51          7865978
4 female  saleswoman    10          7866019
5 female      frying     9          7866020
> print(random_noun_male)
  Source      Item Count Complement_Count
1   male damnation    43          9600052
2   male   vestige    26          9600069
3   male      girt    24          9600071
4   male  informer    22          9600073
5   male  behavior    53          9600042
> write.csv(random_noun_female, "random_mid_freq_noun_female_25_75.csv", row.names = FALSE)
> write.csv(random_noun_male, "random_mid_freq_noun_male_25_75.csv", row.names = FALSE)
>
> setwd("/Users/mac/Desktop")
> data <- read.csv("combined_author_word_analysis(New).csv")
> damn_data <- data[data$Item == "damn" & data$Part_of_Speech == "NN", ]
> female_count <- sum(damn_data$Count[damn_data$Source == "female"])
> male_count <- sum(damn_data$Count[damn_data$Source == "male"])
> female_non_noun_count <- sum(data$Count[data$Source == "female"]) - female_count
> male_non_noun_count <- sum(data$Count[data$Source == "male"]) - male_count
> observed <- matrix(c(female_count, male_count, female_non_noun_count, male_non_noun_count),
+                    nrow = 2,
+                    byrow = TRUE,
+                    dimnames = list(c("Noun", "Non-Noun"),
+                                    c("Female", "Male")))
> chisq_test <- chisq.test(observed)
> std_residuals <- round(chisq_test$stdres, 2)
> print(chisq_test)

	Pearson's Chi-squared test with Yates' continuity correction

data:  observed
X-squared = 3.3372, df = 1, p-value = 0.06773

> print(std_residuals)
         Female  Male
Noun      -1.94  1.94
Non-Noun   1.94 -1.94
> write.csv(std_residuals, "damn_std_residuals.csv", row.names = TRUE)
>
> complacency_data <- data[data$Item == "complacency" & data$Part_of_Speech == "NN", ]
> female_count <- sum(complacency_data$Count[complacency_data$Source == "female"])
> male_count <- sum(complacency_data$Count[complacency_data$Source == "male"])
> female_non_noun_count <- sum(data$Count[data$Source == "female"]) - female_count
> male_non_noun_count <- sum(data$Count[data$Source == "male"]) - male_count
> observed <- matrix(c(female_count, male_count, female_non_noun_count, male_non_noun_count),
+                    nrow = 2,
+                    byrow = TRUE,
+                    dimnames = list(c("Noun", "Non-Noun"),
+                                    c("Female", "Male")))
> chisq_test <- chisq.test(observed)
> std_residuals <- round(chisq_test$stdres, 2)
> print(chisq_test)

	Pearson's Chi-squared test with Yates' continuity correction

data:  observed
X-squared = 9.911, df = 1, p-value = 0.001643

> print(std_residuals)
         Female  Male
Noun       3.26 -3.26
Non-Noun  -3.26  3.26
> write.csv(std_residuals, "complacency_std_residuals.csv", row.names = TRUE)
>
> license_data <- data[data$Item == "license" & data$Part_of_Speech == "NN", ]
> female_count <- sum(license_data$Count[license_data$Source == "female"])
> male_count <- sum(license_data$Count[license_data$Source == "male"])
> female_non_noun_count <- sum(data$Count[data$Source == "female"]) - female_count
> male_non_noun_count <- sum(data$Count[data$Source == "male"]) - male_count
> observed <- matrix(c(female_count, male_count, female_non_noun_count, male_non_noun_count),
+                    nrow = 2,
+                    byrow = TRUE,
+                    dimnames = list(c("Noun", "Non-Noun"),
+                                    c("Female", "Male")))
> chisq_test <- chisq.test(observed)
> std_residuals <- round(chisq_test$stdres, 2)
> print(chisq_test)

	Pearson's Chi-squared test with Yates' continuity correction

data:  observed
X-squared = 0.019157, df = 1, p-value = 0.8899

> print(std_residuals)
         Female  Male
Noun      -0.23  0.23
Non-Noun   0.23 -0.23
> write.csv(std_residuals, "license_std_residuals.csv", row.names = TRUE)
>
> saleswoman_data <- data[data$Item == "saleswoman" & data$Part_of_Speech == "NN", ]
> female_count <- sum(saleswoman_data$Count[saleswoman_data$Source == "female"])
> male_count <- sum(saleswoman_data$Count[saleswoman_data$Source == "male"])
> female_non_noun_count <- sum(data$Count[data$Source == "female"]) - female_count
> male_non_noun_count <- sum(data$Count[data$Source == "male"]) - male_count
> observed <- matrix(c(female_count, male_count, female_non_noun_count, male_non_noun_count),
+                    nrow = 2,
+                    byrow = TRUE,
+                    dimnames = list(c("Noun", "Non-Noun"),
+                                    c("Female", "Male")))
> chisq_test <- chisq.test(observed)
> std_residuals <- round(chisq_test$stdres, 2)
> print(chisq_test)

	Pearson's Chi-squared test with Yates' continuity correction

data:  observed
X-squared = 2.9456, df = 1, p-value = 0.08611

> print(std_residuals)
         Female  Male
Noun       1.98 -1.98
Non-Noun  -1.98  1.98
> write.csv(std_residuals, "saleswoman_std_residuals.csv", row.names = TRUE)
>
> frying_data <- data[data$Item == "frying" & data$Part_of_Speech == "NN", ]
> female_count <- sum(frying_data$Count[frying_data$Source == "female"])
> male_count <- sum(frying_data$Count[frying_data$Source == "male"])
> female_non_noun_count <- sum(data$Count[data$Source == "female"]) - female_count
> male_non_noun_count <- sum(data$Count[data$Source == "male"]) - male_count
> observed <- matrix(c(female_count, male_count, female_non_noun_count, male_non_noun_count),
+                    nrow = 2,
+                    byrow = TRUE,
+                    dimnames = list(c("Noun", "Non-Noun"),
+                                    c("Female", "Male")))
> chisq_test <- chisq.test(observed)
> std_residuals <- round(chisq_test$stdres, 2)
> print(chisq_test)

	Pearson's Chi-squared test with Yates' continuity correction

data:  observed
X-squared = 3.2262, df = 1, p-value = 0.07247

> print(std_residuals)
         Female  Male
Noun       2.09 -2.09
Non-Noun  -2.09  2.09
> write.csv(std_residuals, "frying_std_residuals.csv", row.names = TRUE)
>
> damnation_data <- data[data$Item == "damnation" & data$Part_of_Speech == "NN", ]
> female_count <- sum(damnation_data$Count[damnation_data$Source == "female"])
> male_count <- sum(damnation_data$Count[damnation_data$Source == "male"])
> female_non_noun_count <- sum(data$Count[data$Source == "female"]) - female_count
> male_non_noun_count <- sum(data$Count[data$Source == "male"]) - male_count
> observed <- matrix(c(female_count, male_count, female_non_noun_count, male_non_noun_count),
+                    nrow = 2,
+                    byrow = TRUE,
+                    dimnames = list(c("Noun", "Non-Noun"),
+                                    c("Female", "Male")))
> chisq_test <- chisq.test(observed)
> std_residuals <- round(chisq_test$stdres, 2)
> print(chisq_test)

	Pearson's Chi-squared test with Yates' continuity correction

data:  observed
X-squared = 11.058, df = 1, p-value = 0.0008831

> print(std_residuals)
         Female  Male
Noun      -3.46  3.46
Non-Noun   3.46 -3.46
> write.csv(std_residuals, "damnation_std_residuals.csv", row.names = TRUE)
>
> vestige_data <- data[data$Item == "vestige" & data$Part_of_Speech == "NN", ]
> female_count <- sum(vestige_data$Count[vestige_data$Source == "female"])
> male_count <- sum(vestige_data$Count[vestige_data$Source == "male"])
> female_non_noun_count <- sum(data$Count[data$Source == "female"]) - female_count
> male_non_noun_count <- sum(data$Count[data$Source == "male"]) - male_count
> observed <- matrix(c(female_count, male_count, female_non_noun_count, male_non_noun_count),
+                    nrow = 2,
+                    byrow = TRUE,
+                    dimnames = list(c("Noun", "Non-Noun"),
+                                    c("Female", "Male")))
> chisq_test <- chisq.test(observed)
> std_residuals <- round(chisq_test$stdres, 2)
> print(chisq_test)

	Pearson's Chi-squared test with Yates' continuity correction

data:  observed
X-squared = 6.4291, df = 1, p-value = 0.01123

> print(std_residuals)
         Female  Male
Noun       2.66 -2.66
Non-Noun  -2.66  2.66
> write.csv(std_residuals, "vestige_std_residuals.csv", row.names = TRUE)
>
> girt_data <- data[data$Item == "girt" & data$Part_of_Speech == "NN", ]
> female_count <- sum(girt_data$Count[girt_data$Source == "female"])
> male_count <- sum(girt_data$Count[girt_data$Source == "male"])
> female_non_noun_count <- sum(data$Count[data$Source == "female"]) - female_count
> male_non_noun_count <- sum(data$Count[data$Source == "male"]) - male_count
> observed <- matrix(c(female_count, male_count, female_non_noun_count, male_non_noun_count),
+                    nrow = 2,
+                    byrow = TRUE,
+                    dimnames = list(c("Noun", "Non-Noun"),
+                                    c("Female", "Male")))
> chisq_test <- chisq.test(observed)
> std_residuals <- round(chisq_test$stdres, 2)
> print(chisq_test)

	Pearson's Chi-squared test with Yates' continuity correction

data:  observed
X-squared = 7.9626, df = 1, p-value = 0.004775

> print(std_residuals)
         Female  Male
Noun      -3.01  3.01
Non-Noun   3.01 -3.01
> write.csv(std_residuals, "girt_std_residuals.csv", row.names = TRUE)
>
> informer_data <- data[data$Item == "informer" & data$Part_of_Speech == "NN", ]
> female_count <- sum(informer_data$Count[informer_data$Source == "female"])
> male_count <- sum(informer_data$Count[informer_data$Source == "male"])
> female_non_noun_count <- sum(data$Count[data$Source == "female"]) - female_count
> male_non_noun_count <- sum(data$Count[data$Source == "male"]) - male_count
> observed <- matrix(c(female_count, male_count, female_non_noun_count, male_non_noun_count),
+                    nrow = 2,
+                    byrow = TRUE,
+                    dimnames = list(c("Noun", "Non-Noun"),
+                                    c("Female", "Male")))
> chisq_test <- chisq.test(observed)
> std_residuals <- round(chisq_test$stdres, 2)
> print(chisq_test)

	Pearson's Chi-squared test with Yates' continuity correction

data:  observed
X-squared = 1.3836, df = 1, p-value = 0.2395

> print(std_residuals)
         Female  Male
Noun      -1.35  1.35
Non-Noun   1.35 -1.35
> write.csv(std_residuals, "informer_std_residuals.csv", row.names = TRUE)
>
> behavior_data <- data[data$Item == "behavior" & data$Part_of_Speech == "NN", ]
> female_count <- sum(behavior_data$Count[behavior_data$Source == "female"])
> male_count <- sum(behavior_data$Count[behavior_data$Source == "male"])
> female_non_noun_count <- sum(data$Count[data$Source == "female"]) - female_count
> male_non_noun_count <- sum(data$Count[data$Source == "male"]) - male_count
> observed <- matrix(c(female_count, male_count, female_non_noun_count, male_non_noun_count),
+                    nrow = 2,
+                    byrow = TRUE,
+                    dimnames = list(c("Noun", "Non-Noun"),
+                                    c("Female", "Male")))
> chisq_test <- chisq.test(observed)
> std_residuals <- round(chisq_test$stdres, 2)
> print(chisq_test)

	Pearson's Chi-squared test with Yates' continuity correction

data:  observed
X-squared = 15.868, df = 1, p-value = 6.793e-05

> print(std_residuals)
         Female  Male
Noun       4.07 -4.07
Non-Noun  -4.07  4.07
> write.csv(std_residuals, "behavior_std_residuals.csv", row.names = TRUE)
>
> setwd("/Users/mac/Desktop")
> data <- read.csv("combined_author_word_analysis(New).csv")
> noun_data <- data[data$Part_of_Speech == "NN", ]
> noun_data$Singleton <- noun_data$Count == 1
> contingency_table <- table(noun_data$Singleton, noun_data$Source)
> print(contingency_table)
       
        female  male
  FALSE  18519 23725
  TRUE    9625 16482
> write.csv(as.data.frame(contingency_table), "singleton_vs_reuse_nouns.csv", row.names = TRUE)
> chisq_test <- chisq.test(contingency_table)
> print(chisq_test)

	Pearson's Chi-squared test with Yates' continuity correction

data:  contingency_table
X-squared = 323.4, df = 1, p-value < 2.2e-16

> std_residuals <- round(chisq_test$stdres, 6)
> print(std_residuals)
       
           female      male
  FALSE  17.99137 -17.99137
  TRUE  -17.99137  17.99137
> write.csv(std_residuals, "standard_residuals_singleton_vs_reuse_nouns.csv", row.names = TRUE)
>
> setwd("/Users/mac/Desktop")
> data <- read.csv("combined_author_word_analysis(New).csv")
> verb_data <- data[data$Part_of_Speech == "VB", ]
> library(dplyr)
> verb_data <- verb_data %>%
+   group_by(Source) %>%
+   mutate(Relative_Frequency = Count / sum(Count))
> top10_verb <- verb_data %>%
+   arrange(desc(Count)) %>%
+   slice_head(n = 10) %>%
+   select(Source, Item, Count, Complement_Count, Relative_Frequency)
> write.csv(top10_verb, "top10_verbs_analysis.csv", row.names = FALSE)
> library(ggplot2)
> ggplot(top10_verb, aes(x = reorder(Item, -Relative_Frequency), y = Relative_Frequency, fill = Source)) +
+   geom_bar(stat = "identity", position = "dodge") +
+   scale_fill_manual(values = c("male" = "#0be4ff", "female" = "#4fff76")) +
+   labs(title = "Top 10 Verbs Frequency Comparison",
+        x = "Verbs",
+        y = "Relative Frequency",
+        fill = "Source") +
+   theme_minimal() +
+   theme(axis.text.x = element_text(angle = 45, hjust = 1),
+         plot.title = element_text(hjust = 0.5))
> ggsave("top10_verbs_comparison.png", width = 10, height = 6)
>
> setwd("/Users/mac/Desktop")
> data <- read.csv("combined_author_word_analysis(New).csv")
> be_data <- data[data$Item == "be" & data$Part_of_Speech == "VB", ]
> female_count <- sum(be_data$Count[be_data$Source == "female"])
> male_count <- sum(be_data$Count[be_data$Source == "male"])
> female_non_verb_count <- sum(data$Count[data$Source == "female"]) - female_count
> male_non_verb_count <- sum(data$Count[data$Source == "male"]) - male_count
> observed <- matrix(c(female_count, male_count, female_non_verb_count, male_non_verb_count),
+                    nrow = 2,
+                    byrow = TRUE,
+                    dimnames = list(c("Verb", "Non-Verb"),
+                                    c("Female", "Male")))
> chisq_test <- chisq.test(observed)
> std_residuals <- chisq_test$stdres
> print(chisq_test)

	Pearson's Chi-squared test with Yates' continuity correction

data:  observed
X-squared = 18.283, df = 1, p-value = 1.904e-05

> print(std_residuals)
            Female      Male
Verb     -4.279046  4.279046
Non-Verb  4.279046 -4.279046
> write.csv(std_residuals, "be_std_residuals.csv", row.names = TRUE)
>
> setwd("/Users/mac/Desktop")
> data <- read.csv("combined_author_word_analysis(New).csv")
> have_data <- data[data$Item == "have" & data$Part_of_Speech == "VB", ]
> female_count <- sum(have_data$Count[have_data$Source == "female"])
> male_count <- sum(have_data$Count[have_data$Source == "male"])
> female_non_verb_count <- sum(data$Count[data$Source == "female"]) - female_count
> male_non_verb_count <- sum(data$Count[data$Source == "male"]) - male_count
> observed <- matrix(c(female_count, male_count, female_non_verb_count, male_non_verb_count),
+                    nrow = 2,
+                    byrow = TRUE,
+                    dimnames = list(c("Verb", "Non-Verb"),
+                                    c("Female", "Male")))
> chisq_test <- chisq.test(observed)
> std_residuals <- round(chisq_test$stdres, 2)
> print(chisq_test)

	Pearson's Chi-squared test with Yates' continuity correction

data:  observed
X-squared = 101.45, df = 1, p-value < 2.2e-16

> print(std_residuals)
         Female   Male
Verb      10.08 -10.08
Non-Verb -10.08  10.08
> write.csv(std_residuals, "have_std_residuals.csv", row.names = TRUE)
>
> see_data <- data[data$Item == "see" & data$Part_of_Speech == "VB", ]
> female_count <- sum(see_data$Count[see_data$Source == "female"])
> male_count <- sum(see_data$Count[see_data$Source == "male"])
> female_non_verb_count <- sum(data$Count[data$Source == "female"]) - female_count
> male_non_verb_count <- sum(data$Count[data$Source == "male"]) - male_count
> observed <- matrix(c(female_count, male_count, female_non_verb_count, male_non_verb_count),
+                    nrow = 2,
+                    byrow = TRUE,
+                    dimnames = list(c("Verb", "Non-Verb"),
+                                    c("Female", "Male")))
> chisq_test <- chisq.test(observed)
> std_residuals <- round(chisq_test$stdres, 2)
> print(chisq_test)

	Pearson's Chi-squared test with Yates' continuity correction

data:  observed
X-squared = 138.8, df = 1, p-value < 2.2e-16

> print(std_residuals)
         Female   Male
Verb      11.79 -11.79
Non-Verb -11.79  11.79
> write.csv(std_residuals, "see_std_residuals.csv", row.names = TRUE)
>
> do_data <- data[data$Item == "do" & data$Part_of_Speech == "VB", ]
> female_count <- sum(do_data$Count[do_data$Source == "female"])
> male_count <- sum(do_data$Count[do_data$Source == "male"])
> female_non_verb_count <- sum(data$Count[data$Source == "female"]) - female_count
> male_non_verb_count <- sum(data$Count[data$Source == "male"]) - male_count
> observed <- matrix(c(female_count, male_count, female_non_verb_count, male_non_verb_count),
+                    nrow = 2,
+                    byrow = TRUE,
+                    dimnames = list(c("Verb", "Non-Verb"),
+                                    c("Female", "Male")))
> chisq_test <- chisq.test(observed)
> std_residuals <- round(chisq_test$stdres, 2)
> print(chisq_test)

	Pearson's Chi-squared test with Yates' continuity correction

data:  observed
X-squared = 271.93, df = 1, p-value < 2.2e-16

> print(std_residuals)
         Female  Male
Verb       16.5 -16.5
Non-Verb  -16.5  16.5
> write.csv(std_residuals, "do_std_residuals.csv", row.names = TRUE)
>
> go_data <- data[data$Item == "go" & data$Part_of_Speech == "VB", ]
> female_count <- sum(go_data$Count[go_data$Source == "female"])
> male_count <- sum(go_data$Count[go_data$Source == "male"])
> female_non_verb_count <- sum(data$Count[data$Source == "female"]) - female_count
> male_non_verb_count <- sum(data$Count[data$Source == "male"]) - male_count
> observed <- matrix(c(female_count, male_count, female_non_verb_count, male_non_verb_count),
+                    nrow = 2,
+                    byrow = TRUE,
+                    dimnames = list(c("Verb", "Non-Verb"),
+                                    c("Female", "Male")))
> chisq_test <- chisq.test(observed)
> std_residuals <- round(chisq_test$stdres, 2)
> print(chisq_test)

	Pearson's Chi-squared test with Yates' continuity correction

data:  observed
X-squared = 265.76, df = 1, p-value < 2.2e-16

> print(std_residuals)
         Female   Male
Verb      16.31 -16.31
Non-Verb -16.31  16.31
> write.csv(std_residuals, "go_std_residuals.csv", row.names = TRUE)
> 
> make_data <- data[data$Item == "make" & data$Part_of_Speech == "VB", ]
> female_count <- sum(make_data$Count[make_data$Source == "female"])
> male_count <- sum(make_data$Count[make_data$Source == "male"])
> female_non_verb_count <- sum(data$Count[data$Source == "female"]) - female_count
> male_non_verb_count <- sum(data$Count[data$Source == "male"]) - male_count
> observed <- matrix(c(female_count, male_count, female_non_verb_count, male_non_verb_count),
+                    nrow = 2,
+                    byrow = TRUE,
+                    dimnames = list(c("Verb", "Non-Verb"),
+                                    c("Female", "Male")))
> chisq_test <- chisq.test(observed)
> std_residuals <- round(chisq_test$stdres, 2)
> print(chisq_test)

	Pearson's Chi-squared test with Yates' continuity correction

data:  observed
X-squared = 35.032, df = 1, p-value = 3.243e-09

> print(std_residuals)
         Female  Male
Verb       5.93 -5.93
Non-Verb  -5.93  5.93
> write.csv(std_residuals, "make_std_residuals.csv", row.names = TRUE)
>
> say_data <- data[data$Item == "say" & data$Part_of_Speech == "VB", ]
> female_count <- sum(say_data$Count[say_data$Source == "female"])
> male_count <- sum(say_data$Count[say_data$Source == "male"])
> female_non_verb_count <- sum(data$Count[data$Source == "female"]) - female_count
> male_non_verb_count <- sum(data$Count[data$Source == "male"]) - male_count
> observed <- matrix(c(female_count, male_count, female_non_verb_count, male_non_verb_count),
+                    nrow = 2,
+                    byrow = TRUE,
+                    dimnames = list(c("Verb", "Non-Verb"),
+                                    c("Female", "Male")))
> chisq_test <- chisq.test(observed)
> std_residuals <- round(chisq_test$stdres, 2)
> print(chisq_test)

	Pearson's Chi-squared test with Yates' continuity correction

data:  observed
X-squared = 40.048, df = 1, p-value = 2.478e-10

> print(std_residuals)
         Female  Male
Verb       6.34 -6.34
Non-Verb  -6.34  6.34
> write.csv(std_residuals, "say_std_residuals.csv", row.names = TRUE)
>
> let_data <- data[data$Item == "let" & data$Part_of_Speech == "VB", ]
> female_count <- sum(let_data$Count[let_data$Source == "female"])
> male_count <- sum(let_data$Count[let_data$Source == "male"])
> female_non_verb_count <- sum(data$Count[data$Source == "female"]) - female_count
> male_non_verb_count <- sum(data$Count[data$Source == "male"]) - male_count
> observed <- matrix(c(female_count, male_count, female_non_verb_count, male_non_verb_count),
+                    nrow = 2,
+                    byrow = TRUE,
+                    dimnames = list(c("Verb", "Non-Verb"),
+                                    c("Female", "Male")))
> chisq_test <- chisq.test(observed)
> std_residuals <- round(chisq_test$stdres, 2)
> print(chisq_test)

	Pearson's Chi-squared test with Yates' continuity correction

data:  observed
X-squared = 12.843, df = 1, p-value = 0.0003387

> print(std_residuals)
         Female  Male
Verb      -3.59  3.59
Non-Verb   3.59 -3.59
> write.csv(std_residuals, "let_std_residuals.csv", row.names = TRUE)
>
> take_data <- data[data$Item == "take" & data$Part_of_Speech == "VB", ]
> female_count <- sum(take_data$Count[take_data$Source == "female"])
> male_count <- sum(take_data$Count[take_data$Source == "male"])
> female_non_verb_count <- sum(data$Count[data$Source == "female"]) - female_count
> male_non_verb_count <- sum(data$Count[data$Source == "male"]) - male_count
> observed <- matrix(c(female_count, male_count, female_non_verb_count, male_non_verb_count),
+                    nrow = 2,
+                    byrow = TRUE,
+                    dimnames = list(c("Verb", "Non-Verb"),
+                                    c("Female", "Male")))
> chisq_test <- chisq.test(observed)
> std_residuals <- round(chisq_test$stdres, 2)
> print(chisq_test)

	Pearson's Chi-squared test with Yates' continuity correction

data:  observed
X-squared = 52.632, df = 1, p-value = 4.023e-13

> print(std_residuals)
         Female  Male
Verb       7.26 -7.26
Non-Verb  -7.26  7.26
> write.csv(std_residuals, "take_std_residuals.csv", row.names = TRUE)
>
> know_data <- data[data$Item == "know" & data$Part_of_Speech == "VB", ]
> female_count <- sum(know_data$Count[know_data$Source == "female"])
> male_count <- sum(know_data$Count[know_data$Source == "male"])
> female_non_verb_count <- sum(data$Count[data$Source == "female"]) - female_count
> male_non_verb_count <- sum(data$Count[data$Source == "male"]) - male_count
> observed <- matrix(c(female_count, male_count, female_non_verb_count, male_non_verb_count),
+                    nrow = 2,
+                    byrow = TRUE,
+                    dimnames = list(c("Verb", "Non-Verb"),
+                                    c("Female", "Male")))
> chisq_test <- chisq.test(observed)
> std_residuals <- round(chisq_test$stdres, 2)
> print(chisq_test)

	Pearson's Chi-squared test with Yates' continuity correction

data:  observed
X-squared = 270.91, df = 1, p-value < 2.2e-16

> print(std_residuals)
         Female   Male
Verb      16.47 -16.47
Non-Verb -16.47  16.47
> write.csv(std_residuals, "know_std_residuals.csv", row.names = TRUE)
>
> come_data <- data[data$Item == "come" & data$Part_of_Speech == "VB", ]
> female_count <- sum(come_data$Count[come_data$Source == "female"])
> male_count <- sum(come_data$Count[come_data$Source == "male"])
> female_non_verb_count <- sum(data$Count[data$Source == "female"]) - female_count
> male_non_verb_count <- sum(data$Count[data$Source == "male"]) - male_count
> observed <- matrix(c(female_count, male_count, female_non_verb_count, male_non_verb_count),
+                    nrow = 2,
+                    byrow = TRUE,
+                    dimnames = list(c("Verb", "Non-Verb"),
+                                    c("Female", "Male")))
> chisq_test <- chisq.test(observed)
> std_residuals <- round(chisq_test$stdres, 2)
> print(chisq_test)

	Pearson's Chi-squared test with Yates' continuity correction

data:  observed
X-squared = 89.203, df = 1, p-value < 2.2e-16

> print(std_residuals)
         Female  Male
Verb       9.45 -9.45
Non-Verb  -9.45  9.45
> write.csv(std_residuals, "come_std_residuals.csv", row.names = TRUE)
>
> library(dplyr)
> setwd("/Users/mac/Desktop")
> data <- read.csv("high and medium_freq_words.csv")
> verb_data <- data[data$Part_of_Speech == "VB", ]
> quantiles <- quantile(verb_data$Relative_Frequency, probs = c(0.25, 0.75))
> mid_freq_verb_data <- verb_data[verb_data$Relative_Frequency >= quantiles[1] & verb_data$Relative_Frequency <= quantiles[2], ]
> random_verb_female <- mid_freq_verb_data %>%
+   filter(Source == "female") %>%
+   sample_n(5) %>%
+   select(Source, Item, Count, Complement_Count)
> random_verb_male <- mid_freq_verb_data %>%
+   filter(Source == "male") %>%
+   sample_n(5) %>%
+   select(Source, Item, Count, Complement_Count)
> print(random_verb_female)
  Source       Item Count Complement_Count
1 female       envy    34          7865995
2 female experience    38          7865991
3 female        pat    28          7866001
4 female      wrung    14          7866015
5 female      drown    31          7865998
> print(random_verb_male)
  Source     Item Count Complement_Count
1   male  supplant    24          9600071
2   male    flung    35          9600060
3   male     deem    50          9600045
4   male    shorten    21          9600074
5   male    wound    34          9600061
> write.csv(random_verb_female, "random_mid_freq_verb_female_25_75.csv", row.names = FALSE)
> write.csv(random_verb_male, "random_mid_freq_verb_male_25_75.csv", row.names = FALSE)
>
> setwd("/Users/mac/Desktop")
> data <- read.csv("combined_author_word_analysis(New).csv")
> envy_data <- data[data$Item == "envy" & data$Part_of_Speech == "VB", ]
> female_count <- sum(envy_data$Count[envy_data$Source == "female"])
> male_count <- sum(envy_data$Count[envy_data$Source == "male"])
> female_non_verb_count <- sum(data$Count[data$Source == "female"]) - female_count
> male_non_verb_count <- sum(data$Count[data$Source == "male"]) - male_count
> observed <- matrix(c(female_count, male_count, female_non_verb_count, male_non_verb_count),
+                    nrow = 2,
+                    byrow = TRUE,
+                    dimnames = list(c("Verb", "Non-Verb"),
+                                    c("Female", "Male")))
> chisq_test <- chisq.test(observed)
> std_residuals <- round(chisq_test$stdres, 2)
> print(chisq_test)

	Pearson's Chi-squared test with Yates' continuity correction

data:  observed
X-squared = 1.3808, df = 1, p-value = 0.24

> print(std_residuals)
         Female Male
Verb        1.3 -1.3
Non-Verb   -1.3  1.3
> write.csv(std_residuals, "envy_std_residuals.csv", row.names = TRUE)
>
> experience_data <- data[data$Item == "experience" & data$Part_of_Speech == "VB", ]
> female_count <- sum(experience_data$Count[experience_data$Source == "female"])
> male_count <- sum(experience_data$Count[experience_data$Source == "male"])
> female_non_verb_count <- sum(data$Count[data$Source == "female"]) - female_count
> male_non_verb_count <- sum(data$Count[data$Source == "male"]) - male_count
> observed <- matrix(c(female_count, male_count, female_non_verb_count, male_non_verb_count),
+                    nrow = 2,
+                    byrow = TRUE,
+                    dimnames = list(c("Verb", "Non-Verb"),
+                                    c("Female", "Male")))
> chisq_test <- chisq.test(observed)
> std_residuals <- round(chisq_test$stdres, 2)
> print(chisq_test)

	Pearson's Chi-squared test with Yates' continuity correction

data:  observed
X-squared = 2.8085, df = 1, p-value = 0.09377

> print(std_residuals)
         Female Male
Verb        1.8 -1.8
Non-Verb   -1.8  1.8
> write.csv(std_residuals, "experience_std_residuals.csv", row.names = TRUE)
>
> pat_data <- data[data$Item == "pat" & data$Part_of_Speech == "VB", ]
> female_count <- sum(pat_data$Count[pat_data$Source == "female"])
> male_count <- sum(pat_data$Count[pat_data$Source == "male"])
> female_non_verb_count <- sum(data$Count[data$Source == "female"]) - female_count
> male_non_verb_count <- sum(data$Count[data$Source == "male"]) - male_count
> observed <- matrix(c(female_count, male_count, female_non_verb_count, male_non_verb_count),
+                    nrow = 2,
+                    byrow = TRUE,
+                    dimnames = list(c("Verb", "Non-Verb"),
+                                    c("Female", "Male")))
> chisq_test <- chisq.test(observed)
> std_residuals <- round(chisq_test$stdres, 2)
> print(chisq_test)

	Pearson's Chi-squared test with Yates' continuity correction

data:  observed
X-squared = 11.468, df = 1, p-value = 0.0007079

> print(std_residuals)
         Female  Male
Verb       3.55 -3.55
Non-Verb  -3.55  3.55
> write.csv(std_residuals, "pat_std_residuals.csv", row.names = TRUE)
>
> wrung_data <- data[data$Item == "wrung" & data$Part_of_Speech == "VB", ]
> female_count <- sum(wrung_data$Count[wrung_data$Source == "female"])
> male_count <- sum(wrung_data$Count[wrung_data$Source == "male"])
> female_non_verb_count <- sum(data$Count[data$Source == "female"]) - female_count
> male_non_verb_count <- sum(data$Count[data$Source == "male"]) - male_count
> observed <- matrix(c(female_count, male_count, female_non_verb_count, male_non_verb_count),
+                    nrow = 2,
+                    byrow = TRUE,
+                    dimnames = list(c("Verb", "Non-Verb"),
+                                    c("Female", "Male")))
> chisq_test <- chisq.test(observed)
> std_residuals <- round(chisq_test$stdres, 2)
> print(chisq_test)

	Pearson's Chi-squared test with Yates' continuity correction

data:  observed
X-squared = 2.3694, df = 1, p-value = 0.1237

> print(std_residuals)
         Female  Male
Verb       1.75 -1.75
Non-Verb  -1.75  1.75
> write.csv(std_residuals, "wrung_std_residuals.csv", row.names = TRUE)
>
> drown_data <- data[data$Item == "drown" & data$Part_of_Speech == "VB", ]
> female_count <- sum(drown_data$Count[drown_data$Source == "female"])
> male_count <- sum(drown_data$Count[drown_data$Source == "male"])
> female_non_verb_count <- sum(data$Count[data$Source == "female"]) - female_count
> male_non_verb_count <- sum(data$Count[data$Source == "male"]) - male_count
> observed <- matrix(c(female_count, male_count, female_non_verb_count, male_non_verb_count),
+                    nrow = 2,
+                    byrow = TRUE,
+                    dimnames = list(c("Verb", "Non-Verb"),
+                                    c("Female", "Male")))
> chisq_test <- chisq.test(observed)
> std_residuals <- round(chisq_test$stdres, 2)
> print(chisq_test)

	Pearson's Chi-squared test with Yates' continuity correction

data:  observed
X-squared = 2.4561, df = 1, p-value = 0.1171

> print(std_residuals)
         Female  Male
Verb      -1.68  1.68
Non-Verb   1.68 -1.68
> write.csv(std_residuals, "drown_std_residuals.csv", row.names = TRUE)
>
> supplant_data <- data[data$Item == "supplant" & data$Part_of_Speech == "VB", ]
> female_count <- sum(supplant_data$Count[supplant_data$Source == "female"])
> male_count <- sum(supplant_data$Count[supplant_data$Source == "male"])
> female_non_verb_count <- sum(data$Count[data$Source == "female"]) - female_count
> male_non_verb_count <- sum(data$Count[data$Source == "male"]) - male_count
> observed <- matrix(c(female_count, male_count, female_non_verb_count, male_non_verb_count),
+                    nrow = 2,
+                    byrow = TRUE,
+                    dimnames = list(c("Verb", "Non-Verb"),
+                                    c("Female", "Male")))
> chisq_test <- chisq.test(observed)
> std_residuals <- round(chisq_test$stdres, 2)
> print(chisq_test)

	Pearson's Chi-squared test with Yates' continuity correction

data:  observed
X-squared = 11.22, df = 1, p-value = 0.0008091

> print(std_residuals)
         Female  Male
Verb      -3.54  3.54
Non-Verb   3.54 -3.54
> write.csv(std_residuals, "supplant_std_residuals.csv", row.names = TRUE)
>
> flung_data <- data[data$Item == "flung" & data$Part_of_Speech == "VB", ]
> female_count <- sum(flung_data$Count[flung_data$Source == "female"])
> male_count <- sum(flung_data$Count[flung_data$Source == "male"])
> female_non_verb_count <- sum(data$Count[data$Source == "female"]) - female_count
> male_non_verb_count <- sum(data$Count[data$Source == "male"]) - male_count
> observed <- matrix(c(female_count, male_count, female_non_verb_count, male_non_verb_count),
+                    nrow = 2,
+                    byrow = TRUE,
+                    dimnames = list(c("Verb", "Non-Verb"),
+                                    c("Female", "Male")))
> chisq_test <- chisq.test(observed)
> std_residuals <- round(chisq_test$stdres, 2)
> print(chisq_test)

	Pearson's Chi-squared test with Yates' continuity correction

data:  observed
X-squared = 3.209, df = 1, p-value = 0.07324

> print(std_residuals)
         Female Male
Verb        1.9 -1.9
Non-Verb   -1.9  1.9
> write.csv(std_residuals, "flung_std_residuals.csv", row.names = TRUE)
>
> deem_data <- data[data$Item == "deem" & data$Part_of_Speech == "VB", ]
> female_count <- sum(deem_data$Count[deem_data$Source == "female"])
> male_count <- sum(deem_data$Count[deem_data$Source == "male"])
> female_non_verb_count <- sum(data$Count[data$Source == "female"]) - female_count
> male_non_verb_count <- sum(data$Count[data$Source == "male"]) - male_count
> observed <- matrix(c(female_count, male_count, female_non_verb_count, male_non_verb_count),
+                    nrow = 2,
+                    byrow = TRUE,
+                    dimnames = list(c("Verb", "Non-Verb"),
+                                    c("Female", "Male")))
> chisq_test <- chisq.test(observed)
> std_residuals <- round(chisq_test$stdres, 2)
> print(chisq_test)

	Pearson's Chi-squared test with Yates' continuity correction

data:  observed
X-squared = 9.6855, df = 1, p-value = 0.001857

> print(std_residuals)
         Female  Male
Verb      -3.23  3.23
Non-Verb   3.23 -3.23
> write.csv(std_residuals, "deem_std_residuals.csv", row.names = TRUE)
>
> shorten_data <- data[data$Item == "shorten" & data$Part_of_Speech == "VB", ]
> female_count <- sum(shorten_data$Count[shorten_data$Source == "female"])
> male_count <- sum(shorten_data$Count[shorten_data$Source == "male"])
> female_non_verb_count <- sum(data$Count[data$Source == "female"]) - female_count
> male_non_verb_count <- sum(data$Count[data$Source == "male"]) - male_count
> observed <- matrix(c(female_count, male_count, female_non_verb_count, male_non_verb_count),
+                    nrow = 2,
+                    byrow = TRUE,
+                    dimnames = list(c("Verb", "Non-Verb"),
+                                    c("Female", "Male")))
> chisq_test <- chisq.test(observed)
> std_residuals <- round(chisq_test$stdres, 2)
> print(chisq_test)

	Pearson's Chi-squared test with Yates' continuity correction

data:  observed
X-squared = 0.184, df = 1, p-value = 0.668

> print(std_residuals)
         Female Male
Verb       -0.6  0.6
Non-Verb    0.6 -0.6
> write.csv(std_residuals, "shorten_std_residuals.csv", row.names = TRUE)
>
> wound_data <- data[data$Item == "wound" & data$Part_of_Speech == "VB", ]
> female_count <- sum(wound_data$Count[wound_data$Source == "female"])
> male_count <- sum(wound_data$Count[wound_data$Source == "male"])
> female_non_verb_count <- sum(data$Count[data$Source == "female"]) - female_count
> male_non_verb_count <- sum(data$Count[data$Source == "male"]) - male_count
> observed <- matrix(c(female_count, male_count, female_non_verb_count, male_non_verb_count),
+                    nrow = 2,
+                    byrow = TRUE,
+                    dimnames = list(c("Verb", "Non-Verb"),
+                                    c("Female", "Male")))
> chisq_test <- chisq.test(observed)
> std_residuals <- round(chisq_test$stdres, 2)
> print(chisq_test)

	Pearson's Chi-squared test with Yates' continuity correction

data:  observed
X-squared = 5.0083, df = 1, p-value = 0.02523

> print(std_residuals)
         Female  Male
Verb       2.35 -2.35
Non-Verb  -2.35  2.35
> write.csv(std_residuals, "wound_std_residuals.csv", row.names = TRUE)
>
> setwd("/Users/mac/Desktop")
> data <- read.csv("combined_author_word_analysis(New).csv")
> verb_data <- data[data$Part_of_Speech == "VB", ]
> verb_data$Singleton <- verb_data$Count == 1
> contingency_table <- table(verb_data$Singleton, verb_data$Source)
> print(contingency_table)
       
        female male
  FALSE   4373 5417
  TRUE    2899 4159
> write.csv(as.data.frame(contingency_table), "singleton_vs_reuse_verbs.csv", row.names = TRUE)
> chisq_test <- chisq.test(contingency_table)
> print(chisq_test)

	Pearson's Chi-squared test with Yates' continuity correction

data:  contingency_table
X-squared = 21.448, df = 1, p-value = 3.635e-06

> std_residuals <- round(chisq_test$stdres, 6)
> print(std_residuals)
       
           female      male
  FALSE  4.647013 -4.647013
  TRUE  -4.647013  4.647013
> write.csv(std_residuals, "standard_residuals_singleton_vs_reuse_verbs.csv", row.names = TRUE)
>
> file_path <- "/Users/mac/Desktop/FilesAugust13 - FilesAugust13.tsv"
> data <- read.csv(file_path, sep="\t")
> data_female <- subset(data, Gender == "F")
> data_male <- subset(data, Gender == "M")
> shapiro_test_female <- shapiro.test(data_female$AvgWLength)
> shapiro_test_male <- shapiro.test(data_male$AvgWLength)
> print(shapiro_test_female)

	Shapiro-Wilk normality test

data:  data_female$AvgWLength
W = 0.9649, p-value = 0.002122

> print(shapiro_test_male)

	Shapiro-Wilk normality test

data:  data_male$AvgWLength
W = 0.98475, p-value = 0.1862

> combined_data <- c(data_female$AvgWLength, data_male$AvgWLength)
> shapiro_test_combined <- shapiro.test(combined_data)
> print(shapiro_test_combined)

	Shapiro-Wilk normality test

data:  combined_data
W = 0.98401, p-value = 0.00662

> wilcoxon_test <- wilcox.test(data_female$AvgWLength, data_male$AvgWLength)
> print(wilcoxon_test)

	Wilcoxon rank sum test with continuity correction

data:  data_female$AvgWLength and data_male$AvgWLength
W = 7690, p-value = 0.8371
alternative hypothesis: true location shift is not equal to 0

> library(effsize)
> cohen_d_result <- cohen.d(data_female$AvgWLength, data_male$AvgWLength)
> print(cohen_d_result)

Cohen's d

d estimate: 0.003897507 (negligible)
95 percent confidence interval:
     lower      upper 
-0.2453083  0.2531033 
>
> library(ggplot2)
> histogram <- ggplot(data, aes(x=AvgWLength, fill=Gender)) +
+   geom_histogram(aes(y=..density..), position="identity", alpha=0.6, bins=30, color="black") +
+   geom_density(alpha=0.6) +
+   labs(title="Histogram of the Average Word Length by Gender", 
+        x="Average Word Length", 
+        y="Density") +
+   theme_minimal() +
+   scale_fill_manual(values=c("F"="#6ffb9c", "M"="#7db9eb")) +  
+   theme(plot.title = element_text(hjust = 0.5))
> histogram_path <- "/Users/mac/Desktop/histogram_avg_word_length_custom_colors_centered.png"
> ggsave(histogram_path, plot = histogram, width=8, height=6)
> boxplot <- ggplot(data, aes(x=Gender, y=AvgWLength, fill=Gender)) +
+   geom_boxplot(color="black") +  
+   labs(title="Boxplot of the Average Word Length by Gender", 
+        x="Gender", 
+        y="Average Word Length") +
+   theme_minimal() +
+   scale_fill_manual(values=c("F"="#6ffb9c", "M"="#7db9eb")) +  
+   theme(plot.title = element_text(hjust = 0.5))                                  
> boxplot_path <- "/Users/mac/Desktop/boxplot_avg_word_length_custom_colors_centered.png"
> ggsave(boxplot_path, plot = boxplot, width=8, height=6)
>
> file_path <- "/Users/mac/Desktop/FilesAugust13 - FilesAugust13.tsv"
> data <- read.csv(file_path, sep="\t")
> data_female <- subset(data, Gender == "F")
> data_male <- subset(data, Gender == "M")
> female_mean_ttr <- mean(data_female$TTR)
> female_median_ttr <- median(data_female$TTR)
> female_min_ttr <- min(data_female$TTR)
> female_max_ttr <- max(data_female$TTR)
> male_mean_ttr <- mean(data_male$TTR)
> male_median_ttr <- median(data_male$TTR)
> male_min_ttr <- min(data_male$TTR)
> male_max_ttr <- max(data_male$TTR)
> cat("Mean:", female_mean_ttr, "\n")
Mean: 0.1309251 
> cat("Median:", female_median_ttr, "\n")
Median: 0.1092053 
> cat("Minimum:", female_min_ttr, "\n")
Minimum: 0.03421749 
> cat("Maximum:", female_max_ttr, "\n\n")
Maximum: 0.3585769 
> cat("Mean:", male_mean_ttr, "\n")
Mean: 0.1104688 
> cat("Median:", male_median_ttr, "\n")
Median: 0.09157944 
> cat("Minimum:", male_min_ttr, "\n")
Minimum: 0.02802377 
> cat("Maximum:", male_max_ttr, "\n")
Maximum: 0.328899
>
> shapiro_test_female <- shapiro.test(data_female$TTR)
> shapiro_test_male <- shapiro.test(data_male$TTR)
> print(shapiro_test_female)

	Shapiro-Wilk normality test

data:  data_female$TTR
W = 0.81501, p-value = 2.113e-11

> print(shapiro_test_male)

	Shapiro-Wilk normality test

data:  data_male$TTR
W = 0.83515, p-value = 2.342e-10

> wilcoxon_test_result <- wilcox.test(data_female$TTR, data_male$TTR)
> print(wilcoxon_test_result)

	Wilcoxon rank sum test with continuity correction

data:  data_female$TTR and data_male$TTR
W = 9258, p-value = 0.01121
alternative hypothesis: true location shift is not equal to 0
>
> male_text_path <- "/Users/mac/Desktop/merged_texts(male author).txt"
> female_text_path <- "/Users/mac/Desktop/merged_texts(female author).txt"
> male_text <- readLines(male_text_path, warn = FALSE)
> female_text <- readLines(female_text_path, warn = FALSE)
> male_text <- paste(male_text, collapse = " ")
> female_text <- paste(female_text, collapse = " ")
> install.packages("udpipe")
> library(udpipe)
> model_file_path <- "/Users/mac/Desktop/english-ewt-ud-2.5-191206.udpipe"
> model <- udpipe_download_model(language = "english")
> ud_model <- udpipe_load_model(model$file_model)
> male_annotation <- udpipe_annotate(ud_model, x = male_text)
> male_annotation <- as.data.frame(male_annotation)
> female_annotation <- udpipe_annotate(ud_model, x = female_text)
> female_annotation <- as.data.frame(female_annotation)
> male_dependency_freq <- table(male_annotation$dep_rel)
> male_dependency_freq <- sort(male_dependency_freq, decreasing = TRUE)
> female_dependency_freq <- table(female_annotation$dep_rel)
> female_dependency_freq <- sort(female_dependency_freq, decreasing = TRUE)
> print(male_dependency_freq)

       punct         case          det        nsubj       advmod          obl 
     1962208      1174538      1001258       929476       684277       588925 
         obj         root         conj         amod           cc         nmod 
      571161       545146       518951       487241       443247       441858 
        mark          aux    nmod:poss        advcl          cop     compound 
      404933       307208       307102       223810       208700       199411 
       ccomp    acl:relcl        xcomp          acl    parataxis     aux:pass 
      154637       136479       136328        91639        86111        85555 
       appos   nsubj:pass       nummod         flat compound:prt    discourse 
       80459        68138        66304        57674        40726        34252 
        expl   det:predet        fixed         iobj     obl:tmod    obl:npmod 
       26821        21779        19487        14264        12042        11741 
  cc:preconj         list     goeswith        csubj     vocative   nmod:npmod 
        4640         4625         4164         3980         3646         3093 
   nmod:tmod flat:foreign   reparandum       orphan 
         931           69           34           29 
> print(female_dependency_freq)

       punct         case        nsubj          det       advmod          obj 
     1593583       892424       842843       743794       607121       478907 
         obl         root         conj         amod           cc         mark 
      478432       464479       426817       405088       376390       341317 
        nmod          aux    nmod:poss        advcl          cop     compound 
      303882       289314       269906       192235       189799       148997 
       ccomp        xcomp    acl:relcl          acl    parataxis     aux:pass 
      148991       128117       102805        71752        69376        64032 
        flat   nsubj:pass        appos compound:prt       nummod    discourse 
       61309        51321        46725        39400        37420        28889 
        expl   det:predet        fixed     obl:tmod         iobj    obl:npmod 
       24802        17097        14966        13350        12697        11896 
       csubj     vocative   cc:preconj   nmod:npmod         list     goeswith 
        3818         3524         2862         2103         1698         1346 
   nmod:tmod flat:foreign   reparandum       orphan 
         911           23           18           17 
> write.csv(as.data.frame(male_dependency_freq), file = male_output_path, row.names = TRUE)
> dependency_data <- data.frame(
+   male = as.numeric(male_dependency_freq),
+   female = as.numeric(female_dependency_freq)
+ )
> rownames(dependency_data) <- names(male_dependency_freq)
> chi_square_result <- chisq.test(dependency_data)
> print(chi_square_result)

	Pearson's Chi-squared test

data:  dependency_data
X-squared = 22205, df = 45, p-value < 2.2e-16

> library(ggplot2)
> dependency_long <- data.frame(
+   relation = rep(rownames(dependency_data), 2),
+   frequency = c(dependency_data$male, dependency_data$female),
+   gender = rep(c("male", "female"), each = nrow(dependency_data))
+ )
> p <- ggplot(dependency_long, aes(x = relation, y = frequency, fill = gender)) +
+   geom_bar(stat = "identity", position = "dodge") +
+   scale_fill_manual(values = c("female" = "#6ffb9c", "male" = "#7db9eb")) +  
+   theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
+   labs(title = "Comparison of Dependency Relations by Gender", x = "Dependency Relation", y = "Frequency") +
+   theme_minimal() +
+   theme(
+     plot.title = element_text(hjust = 0.5),  
+     axis.text.x = element_text(angle = 45, hjust = 1)
+   )
> print(p)
> ggsave("~/Desktop/dependency_relations_comparison.png", plot = p, width = 10, height = 7)
>
> library(udpipe)
> model <- udpipe_download_model(language = "english")
> ud_model <- udpipe_load_model(model$file_model)
> male_text <- readLines("/Users/mac/Desktop/merged_texts(male author).txt")
> male_annotations <- udpipe_annotate(ud_model, x = male_text)
> male_df <- as.data.frame(male_annotations)
> female_text <- readLines("/Users/mac/Desktop/merged_texts(female author).txt")
> female_text <- readLines("/Users/mac/Desktop/merged_texts(female author).txt")
> female_annotations <- udpipe_annotate(ud_model, x = female_text)
> female_df <- as.data.frame(female_annotations)
> write.csv(male_df, "/Users/mac/Desktop/male_author_annotations.csv", row.names = FALSE)
> write.csv(female_df, "/Users/mac/Desktop/female_author_annotations.csv", row.names = FALSE)
> library(dplyr)
> det_male <- male_df %>%
+   filter(dep_rel == "det") %>%
+   select(doc_id, sentence_id, token, lemma, dep_rel, sentence)
> det_female <- female_df %>%
+   filter(dep_rel == "det") %>%
+   select(doc_id, sentence_id, token, lemma, dep_rel, sentence)
> nsubj_male <- male_df %>%
+   filter(dep_rel == "nsubj") %>%
+   select(doc_id, sentence_id, token, lemma, dep_rel, sentence)
> nsubj_female <- female_df %>%
+   filter(dep_rel == "nsubj") %>%
+   select(doc_id, sentence_id, token, lemma, dep_rel, sentence)
> write.csv(det_male, "/Users/mac/Desktop/det_male_analysis.csv", row.names = FALSE)
> write.csv(det_female, "/Users/mac/Desktop/det_female_analysis.csv", row.names = FALSE)
> write.csv(nsubj_male, "/Users/mac/Desktop/nsubj_male_analysis.csv", row.names = FALSE)
> write.csv(nsubj_female, "/Users/mac/Desktop/nsubj_female_analysis.csv", row.names = FALSE)
>
> install.packages("readxl")
> library(readxl)
> file_path_female <- "/Users/mac/Desktop/female_DT NN.xlsx"
> file_path_male <- "/Users/mac/Desktop/male_DT NN.xlsx"
> data_female <- read_excel(file_path_female)                                                                           
> data_male <- read_excel(file_path_male)         
> nn_freq_female <- sort(table(data_female$NN), decreasing = TRUE)
> head(nn_freq_female)

  time    man    day    way moment  house 
  5914   5260   4660   3980   3846   3793 
> nn_freq_male <- sort(table(data_male$NN), decreasing = TRUE)
> head(nn_freq_male)

   man   time    day  world moment    way 
 11712   8209   5706   5473   5047   4753 
> write.csv(as.data.frame(nn_freq_female), "/Users/mac/Desktop/female_nn_frequency.csv", row.names = FALSE)
> write.csv(as.data.frame(nn_freq_male), "/Users/mac/Desktop/male_nn_frequency.csv", row.names = FALSE)
>
> library(readxl)
> file_path_male <- "/Users/mac/Desktop/DTNN_Male_Contigency.xlsx"
> file_path_female <- "/Users/mac/Desktop/DTNN_Female_Contigency.xlsx"
> data_male <- read_excel(file_path_male)                                                                               
> data_female <- read_excel(file_path_female)                                                                                 
> man_male_count <- data_male$Count[data_male$Male == "man"]
> man_male_complement <- data_male$`Complement Count`[data_male$Male == "man"]
> man_female_count <- data_female$Count[data_female$Female == "man"]
> man_female_complement <- data_female$`Complement Count`[data_female$Female == "man"]
> contingency_table <- matrix(c(
+   man_male_count, man_male_complement,
+   man_female_count, man_female_complement
+ ), nrow = 2, byrow = TRUE)
> colnames(contingency_table) <- c("Observed", "Complement")
> rownames(contingency_table) <- c("Male", "Female")
> print(round(contingency_table, 2))
       Observed Complement
Male      11712    9588383
Female     5260    7860769
> chi_result <- chisq.test(contingency_table)
> print(chi_result)

	Pearson's Chi-squared test with Yates' continuity correction

data:  contingency_table
X-squared = 1353, df = 1, p-value < 2.2e-16

> std_residuals <- round(chi_result$stdres, 2)
> print(std_residuals)
       Observed Complement
Male      36.79     -36.79
Female   -36.79      36.79
>
> time_male_count <- data_male$Count[data_male$Male == "time"]
> time_male_complement <- data_male$`Complement Count`[data_male$Male == "time"]
> time_female_count <- data_female$Count[data_female$Female == "time"]
> time_female_complement <- data_female$`Complement Count`[data_female$Female == "time"]
> contingency_table <- matrix(c(
+   time_male_count, time_male_complement,
+   time_female_count, time_female_complement
+ ), nrow = 2, byrow = TRUE)
> colnames(contingency_table) <- c("Observed", "Complement")
> rownames(contingency_table) <- c("Male", "Female")
> print(round(contingency_table, 2))
       Observed Complement
Male       8209    9591886
Female     5914    7860115
> chi_result <- chisq.test(contingency_table)
> print(chi_result)

	Pearson's Chi-squared test with Yates' continuity correction

data:  contingency_table
X-squared = 56.925, df = 1, p-value = 4.527e-14

> std_residuals <- round(chi_result$stdres, 2)
> print(std_residuals)
       Observed Complement
Male       7.55      -7.55
Female    -7.55       7.55
>
> day_male_count <- data_male$Count[data_male$Male == "day"]
> day_male_complement <- data_male$`Complement Count`[data_male$Male == "day"]
> day_female_count <- data_female$Count[data_female$Female == "day"]
> day_female_complement <- data_female$`Complement Count`[data_female$Female == "day"]
> contingency_table <- matrix(c(
+   day_male_count, day_male_complement,
+   day_female_count, day_female_complement
+ ), nrow = 2, byrow = TRUE)
> colnames(contingency_table) <- c("Observed", "Complement")
> rownames(contingency_table) <- c("Male", "Female")
> print(round(contingency_table, 2))
       Observed Complement
Male       5706    9594389
Female     4660    7861369
> chi_result <- chisq.test(contingency_table)
> print(chi_result)

	Pearson's Chi-squared test with Yates' continuity correction

data:  contingency_table
X-squared = 0.024479, df = 1, p-value = 0.8757

> std_residuals <- round(chi_result$stdres, 2)
> print(std_residuals)
       Observed Complement
Male       0.17      -0.17
Female    -0.17       0.17
>
> way_male_count <- data_male$Count[data_male$Male == "way"]
> way_male_complement <- data_male$`Complement Count`[data_male$Male == "way"]
> way_female_count <- data_female$Count[data_female$Female == "way"]
> way_female_complement <- data_female$`Complement Count`[data_female$Female == "way"]
> contingency_table <- matrix(c(
+   way_male_count, way_male_complement,
+   way_female_count, way_female_complement
+ ), nrow = 2, byrow = TRUE)
> colnames(contingency_table) <- c("Observed", "Complement")
> rownames(contingency_table) <- c("Male", "Female")
> print(round(contingency_table, 2))
       Observed Complement
Male       4753    9595342
Female     3980    7862049
> chi_result <- chisq.test(contingency_table)
> print(chi_result)

	Pearson's Chi-squared test with Yates' continuity correction

data:  contingency_table
X-squared = 1.0013, df = 1, p-value = 0.317

> std_residuals <- round(chi_result$stdres, 2)
> print(std_residuals)
       Observed Complement
Male      -1.01       1.01
Female     1.01      -1.01
>
> moment_male_count <- data_male$Count[data_male$Male == "moment"]
> moment_male_complement <- data_male$`Complement Count`[data_male$Male == "moment"]
> moment_female_count <- data_female$Count[data_female$Female == "moment"]
> moment_female_complement <- data_female$`Complement Count`[data_female$Female == "moment"]
> contingency_table <- matrix(c(
+   moment_male_count, moment_male_complement,
+   moment_female_count, moment_female_complement
+ ), nrow = 2, byrow = TRUE)
> colnames(contingency_table) <- c("Observed", "Complement")
> rownames(contingency_table) <- c("Male", "Female")
> print(round(contingency_table, 2))
       Observed Complement
Male       5047    9595048
Female     3846    7862183
> chi_result <- chisq.test(contingency_table)
> print(chi_result)

	Pearson's Chi-squared test with Yates' continuity correction

data:  contingency_table
X-squared = 11.424, df = 1, p-value = 0.0007248

> std_residuals <- round(chi_result$stdres, 2)
> print(std_residuals)
       Observed Complement
Male       3.39      -3.39
Female    -3.39       3.39
>
> house_male_count <- data_male$Count[data_male$Male == "house"]
> house_male_complement <- data_male$`Complement Count`[data_male$Male == "house"]
> house_female_count <- data_female$Count[data_female$Female == "house"]
> house_female_complement <- data_female$`Complement Count`[data_female$Female == "house"]
> contingency_table <- matrix(c(
+   house_male_count, house_male_complement,
+   house_female_count, house_female_complement
+ ), nrow = 2, byrow = TRUE)
> colnames(contingency_table) <- c("Observed", "Complement")
> rownames(contingency_table) <- c("Male", "Female")
> print(round(contingency_table, 2))
       Observed Complement
Male       4089    9596006
Female     3793    7862236
> chi_result <- chisq.test(contingency_table)
> print(chi_result)

	Pearson's Chi-squared test with Yates' continuity correction

data:  contingency_table
X-squared = 30.221, df = 1, p-value = 3.855e-08

> std_residuals <- round(chi_result$stdres, 2)
> print(std_residuals)
       Observed Complement
Male      -5.51       5.51
Female     5.51      -5.51
>
> world_male_count <- data_male$Count[data_male$Male == "world"]
> world_male_complement <- data_male$`Complement Count`[data_male$Male == "world"]
> world_female_count <- data_female$Count[data_female$Female == "world"]
> world_female_complement <- data_female$`Complement Count`[data_female$Female == "world"]
> contingency_table <- matrix(c(
+   world_male_count, world_male_complement,
+   world_female_count, world_female_complement
+ ), nrow = 2, byrow = TRUE)
> colnames(contingency_table) <- c("Observed", "Complement")
> rownames(contingency_table) <- c("Male", "Female")
> print(round(contingency_table, 2))
       Observed Complement
Male       5473    9594622
Female     3612    7862417
> chi_result <- chisq.test(contingency_table)
> print(chi_result)

	Pearson's Chi-squared test with Yates' continuity correction

data:  contingency_table
X-squared = 102.08, df = 1, p-value < 2.2e-16

> std_residuals <- round(chi_result$stdres, 2)
> print(std_residuals)
       Observed Complement
Male      10.11     -10.11
Female   -10.11      10.11
>
> door_male_count <- data_male$Count[data_male$Male == "door"]
> door_male_complement <- data_male$`Complement Count`[data_male$Male == "door"]
> door_female_count <- data_female$Count[data_female$Female == "door"]
> door_female_complement <- data_female$`Complement Count`[data_female$Female == "door"]
> contingency_table <- matrix(c(
+   door_male_count, door_male_complement,
+   door_female_count, door_female_complement
+ ), nrow = 2, byrow = TRUE)
> colnames(contingency_table) <- c("Observed", "Complement")
> rownames(contingency_table) <- c("Male", "Female")
> print(round(contingency_table, 2))
       Observed Complement
Male       4198    9595897
Female     3267    7862762
> chi_result <- chisq.test(contingency_table)
> print(chi_result)

	Pearson's Chi-squared test with Yates' continuity correction

data:  contingency_table
X-squared = 4.8278, df = 1, p-value = 0.028

> std_residuals <- round(chi_result$stdres, 2)
> print(std_residuals)
       Observed Complement
Male       2.21      -2.21
Female    -2.21       2.21
>
> room_male_count <- data_male$Count[data_male$Male == "room"]
> room_male_complement <- data_male$`Complement Count`[data_male$Male == "room"]
> room_female_count <- data_female$Count[data_female$Female == "room"]
> room_female_complement <- data_female$`Complement Count`[data_female$Female == "room"]
> contingency_table <- matrix(c(
+   room_male_count, room_male_complement,
+   room_female_count, room_female_complement
+ ), nrow = 2, byrow = TRUE)
> colnames(contingency_table) <- c("Observed", "Complement")
> rownames(contingency_table) <- c("Male", "Female")
> print(round(contingency_table, 2))
       Observed Complement
Male       2987    9597108
Female     3062    7862967
> chi_result <- chisq.test(contingency_table)
> print(chi_result)

	Pearson's Chi-squared test with Yates' continuity correction

data:  contingency_table
X-squared = 75.998, df = 1, p-value < 2.2e-16

> std_residuals <- round(chi_result$stdres, 2)
> print(std_residuals)
       Observed Complement
Male      -8.73       8.73
Female     8.73      -8.73
>
> woman_male_count <- data_male$Count[data_male$Male == "woman"]
> woman_male_complement <- data_male$`Complement Count`[data_male$Male == "woman"]
> woman_female_count <- data_female$Count[data_female$Female == "woman"]
> woman_female_complement <- data_female$`Complement Count`[data_female$Female == "woman"]
> contingency_table <- matrix(c(
+   woman_male_count, woman_male_complement,
+   woman_female_count, woman_female_complement
+ ), nrow = 2, byrow = TRUE)
> colnames(contingency_table) <- c("Observed", "Complement")
> rownames(contingency_table) <- c("Male", "Female")
> print(round(contingency_table, 2))
       Observed Complement
Male       2933    9597162
Female     2505    7863524
> chi_result <- chisq.test(contingency_table)
> print(chi_result)

	Pearson's Chi-squared test with Yates' continuity correction

data:  contingency_table
X-squared = 2.2846, df = 1, p-value = 0.1307

> std_residuals <- round(chi_result$stdres, 2)
> print(std_residuals)
       Observed Complement
Male      -1.53       1.53
Female     1.53      -1.53
>
> end_male_count <- data_male$Count[data_male$Male == "end"]
> end_male_complement <- data_male$`Complement Count`[data_male$Male == "end"]
> end_female_count <- data_female$Count[data_female$Female == "end"]
> end_female_complement <- data_female$`Complement Count`[data_female$Female == "end"]
> contingency_table <- matrix(c(
+   end_male_count, end_male_complement,
+   end_female_count, end_female_complement
+ ), nrow = 2, byrow = TRUE)
> colnames(contingency_table) <- c("Observed", "Complement")
> rownames(contingency_table) <- c("Male", "Female")
> print(round(contingency_table, 2))
       Observed Complement
Male       3211    9596884
Female     1873    7864156
> chi_result <- chisq.test(contingency_table)
> print(chi_result)

	Pearson's Chi-squared test with Yates' continuity correction

data:  contingency_table
X-squared = 137.64, df = 1, p-value < 2.2e-16

> std_residuals <- round(chi_result$stdres, 2)
> print(std_residuals)
       Observed Complement
Male      11.75     -11.75
Female   -11.75      11.75
>
> place_male_count <- data_male$Count[data_male$Male == "place"]
> place_male_complement <- data_male$`Complement Count`[data_male$Male == "place"]
> place_female_count <- data_female$Count[data_female$Female == "place"]
> place_female_complement <- data_female$`Complement Count`[data_female$Female == "place"]
> contingency_table <- matrix(c(
+   place_male_count, place_male_complement,
+   place_female_count, place_female_complement
+ ), nrow = 2, byrow = TRUE)
> colnames(contingency_table) <- c("Observed", "Complement")
> rownames(contingency_table) <- c("Male", "Female")
> print(round(contingency_table, 2))
       Observed Complement
Male       3199    9596896
Female     1939    7864090
> chi_result <- chisq.test(contingency_table)
> print(chi_result)

	Pearson's Chi-squared test with Yates' continuity correction

data:  contingency_table
X-squared = 110.27, df = 1, p-value < 2.2e-16

> std_residuals <- round(chi_result$stdres, 2)
> print(std_residuals)
       Observed Complement
Male      10.52     -10.52
Female   -10.52      10.52
>
> male_nsubj_freq <- as.data.frame(table(nsubj_male_analysis$lemma))
> male_nsubj_freq <- male_nsubj_freq[order(male_nsubj_freq$Freq, decreasing = TRUE), ]
> colnames(male_nsubj_freq) <- c("lemma", "male_frequency")
> female_nsubj_freq <- as.data.frame(table(nsubj_female_analysis$lemma))
> female_nsubj_freq <- female_nsubj_freq[order(female_nsubj_freq$Freq, decreasing = TRUE), ]
> colnames(female_nsubj_freq) <- c("lemma", "female_frequency")
> total_male_count <- sum(male_nsubj_freq$male_frequency)
> total_female_count <- sum(female_nsubj_freq$female_frequency)
> cat("Total count of male subjects: ", total_male_count, "\n")
Total count of male subjects:  922753 
> cat("Total count of female subjects: ", total_female_count, "\n")
Total count of female subjects:  832796 
> male_nsubj_freq$male_relative_frequency <- male_nsubj_freq$male_frequency / total_male_count
> female_nsubj_freq$female_relative_frequency <- female_nsubj_freq$female_frequency / total_female_count
> write.csv(male_nsubj_freq, file = "~/Desktop/male_nsubj_sorted_with_relative_frequency.csv", row.names = FALSE)
> write.csv(female_nsubj_freq, file = "~/Desktop/female_nsubj_sorted_with_relative_frequency.csv", row.names = FALSE)
>
> library(readxl)
> file_path_male <- "/Users/mac/Desktop/Male_Top 10 Subjects.xlsx"
> file_path_female <- "/Users/mac/Desktop/Female_Top 10 Subjects.xlsx"
> data_male <- read_excel(file_path_male)                                                                                 
> data_female <- read_excel(file_path_female)
                                                                                  
> i_male_count <- data_male$Count[data_male$Male == "I"]
> i_male_complement <- data_male$`Complement Count`[data_male$Male == "I"]
> i_female_count <- data_female$Count[data_female$Female == "I"]
> i_female_complement <- data_female$`Complement Count`[data_female$Female == "I"]
> contingency_table <- matrix(c(
+   i_male_count, i_male_complement,
+   i_female_count, i_female_complement
+ ), nrow = 2, byrow = TRUE)
> colnames(contingency_table) <- c("Observed", "Complement")
> rownames(contingency_table) <- c("Male", "Female")
> print(round(contingency_table, 2))
       Observed Complement
Male     137065    9463030
Female   139949    7726080
> chi_result <- chisq.test(contingency_table)
> print(chi_result)

	Pearson's Chi-squared test with Yates' continuity correction

data:  contingency_table
X-squared = 3420.4, df = 1, p-value < 2.2e-16

> std_residuals <- round(chi_result$stdres, 2)
> print(std_residuals)
       Observed Complement
Male     -58.49      58.49
Female    58.49     -58.49
>
> he_male_count <- data_male$Count[data_male$Male == "he"]
> he_male_complement <- data_male$`Complement Count`[data_male$Male == "he"]
> he_female_count <- data_female$Count[data_female$Female == "he"]
> he_female_complement <- data_female$`Complement Count`[data_female$Female == "he"]
> contingency_table <- matrix(c(
+   he_male_count, he_male_complement,
+   he_female_count, he_female_complement
+ ), nrow = 2, byrow = TRUE)
> colnames(contingency_table) <- c("Observed", "Complement")
> rownames(contingency_table) <- c("Male", "Female")
> print(round(contingency_table, 2))
       Observed Complement
Male     123673    9476422
Female    87630    7778399
> chi_result <- chisq.test(contingency_table)
> print(chi_result)

	Pearson's Chi-squared test with Yates' continuity correction

data:  contingency_table
X-squared = 1097.8, df = 1, p-value < 2.2e-16

> std_residuals <- round(chi_result$stdres, 2)
> print(std_residuals)
       Observed Complement
Male      33.14     -33.14
Female   -33.14      33.14
>
> she_male_count <- data_male$Count[data_male$Male == "she"]
> she_male_complement <- data_male$`Complement Count`[data_male$Male == "she"]
> she_female_count <- data_female$Count[data_female$Female == "she"]
> she_female_complement <- data_female$`Complement Count`[data_female$Female == "she"]
> contingency_table <- matrix(c(
+   she_male_count, she_male_complement,
+   she_female_count, she_female_complement
+ ), nrow = 2, byrow = TRUE)
> colnames(contingency_table) <- c("Observed", "Complement")
> rownames(contingency_table) <- c("Male", "Female")
> print(round(contingency_table, 2))
       Observed Complement
Male      49416    9550679
Female    86663    7779366
> chi_result <- chisq.test(contingency_table)
> print(chi_result)

	Pearson's Chi-squared test with Yates' continuity correction

data:  contingency_table
X-squared = 19270, df = 1, p-value < 2.2e-16

> std_residuals <- round(chi_result$stdres, 2)
> print(std_residuals)
       Observed Complement
Male    -138.82     138.82
Female   138.82    -138.82
>
> you_male_count <- data_male$Count[data_male$Male == "you"]
> you_male_complement <- data_male$`Complement Count`[data_male$Male == "you"]
> you_female_count <- data_female$Count[data_female$Female == "you"]
> you_female_complement <- data_female$`Complement Count`[data_female$Female == "you"]
> contingency_table <- matrix(c(
+   you_male_count, you_male_complement,
+   you_female_count, you_female_complement
+ ), nrow = 2, byrow = TRUE)
> colnames(contingency_table) <- c("Observed", "Complement")
> rownames(contingency_table) <- c("Male", "Female")
> print(round(contingency_table, 2))
       Observed Complement
Male      52695    9547400
Female    55379    7810650
> chi_result <- chisq.test(contingency_table)
> print(chi_result)

	Pearson's Chi-squared test with Yates' continuity correction

data:  contingency_table
X-squared = 1691.7, df = 1, p-value < 2.2e-16

> std_residuals <- round(chi_result$stdres, 2)
> print(std_residuals)
       Observed Complement
Male     -41.13      41.13
Female    41.13     -41.13
>
> it_male_count <- data_male$Count[data_male$Male == "it"]
> it_male_complement <- data_male$`Complement Count`[data_male$Male == "it"]
> it_female_count <- data_female$Count[data_female$Female == "it"]
> it_female_complement <- data_female$`Complement Count`[data_female$Female == "it"]
> contingency_table <- matrix(c(
+   it_male_count, it_male_complement,
+   it_female_count, it_female_complement
+ ), nrow = 2, byrow = TRUE)
> colnames(contingency_table) <- c("Observed", "Complement")
> rownames(contingency_table) <- c("Male", "Female")
> print(round(contingency_table, 2))
       Observed Complement
Male      53256    9546839
Female    50514    7815515
> chi_result <- chisq.test(contingency_table)
> print(chi_result)

	Pearson's Chi-squared test with Yates' continuity correction

data:  contingency_table
X-squared = 559.5, df = 1, p-value < 2.2e-16

> std_residuals <- round(chi_result$stdres, 2)
> print(std_residuals)
       Observed Complement
Male     -23.66      23.66
Female    23.66     -23.66
>
> they_male_count <- data_male$Count[data_male$Male == "they"]
> they_male_complement <- data_male$`Complement Count`[data_male$Male == "they"]
> they_female_count <- data_female$Count[data_female$Female == "they"]
> they_female_complement <- data_female$`Complement Count`[data_female$Female == "they"]
> contingency_table <- matrix(c(
+   they_male_count, they_male_complement,
+   they_female_count, they_female_complement
+ ), nrow = 2, byrow = TRUE)
> colnames(contingency_table) <- c("Observed", "Complement")
> rownames(contingency_table) <- c("Male", "Female")
> print(round(contingency_table, 2))
       Observed Complement
Male      42290    9557805
Female    32540    7833489
> chi_result <- chisq.test(contingency_table)
> print(chi_result)

	Pearson's Chi-squared test with Yates' continuity correction

data:  contingency_table
X-squared = 72.941, df = 1, p-value < 2.2e-16

> std_residuals <- round(chi_result$stdres, 2)
> print(std_residuals)
       Observed Complement
Male       8.54      -8.54
Female    -8.54       8.54
>
> we_male_count <- data_male$Count[data_male$Male == "we"]
> we_male_complement <- data_male$`Complement Count`[data_male$Male == "we"]
> we_female_count <- data_female$Count[data_female$Female == "we"]
> we_female_complement <- data_female$`Complement Count`[data_female$Female == "we"]
> contingency_table <- matrix(c(
+   we_male_count, we_male_complement,
+   we_female_count, we_female_complement
+ ), nrow = 2, byrow = TRUE)
> colnames(contingency_table) <- c("Observed", "Complement")
> rownames(contingency_table) <- c("Male", "Female")
> print(round(contingency_table, 2))
       Observed Complement
Male      28505    9571590
Female    26684    7839345
> chi_result <- chisq.test(contingency_table)
> print(chi_result)

	Pearson's Chi-squared test with Yates' continuity correction

data:  contingency_table
X-squared = 245.55, df = 1, p-value < 2.2e-16

> std_residuals <- round(chi_result$stdres, 2)
> print(std_residuals)
       Observed Complement
Male     -15.67      15.67
Female    15.67     -15.67
>
> that_male_count <- data_male$Count[data_male$Male == "that"]
> that_male_complement <- data_male$`Complement Count`[data_male$Male == "that"]
> that_female_count <- data_female$Count[data_female$Female == "that"]
> that_female_complement <- data_female$`Complement Count`[data_female$Female == "that"]
> contingency_table <- matrix(c(
+   that_male_count, that_male_complement,
+   that_female_count, that_female_complement
+ ), nrow = 2, byrow = TRUE)
> colnames(contingency_table) <- c("Observed", "Complement")
> rownames(contingency_table) <- c("Male", "Female")
> print(round(contingency_table, 2))
       Observed Complement
Male      29134    9570961
Female    22531    7843498
> chi_result <- chisq.test(contingency_table)
> print(chi_result)

	Pearson's Chi-squared test with Yates' continuity correction

data:  contingency_table
X-squared = 42.518, df = 1, p-value = 7.005e-11

> std_residuals <- round(chi_result$stdres, 2)
> print(std_residuals)
       Observed Complement
Male       6.52      -6.52
Female    -6.52       6.52
>
> who_male_count <- data_male$Count[data_male$Male == "who"]
> who_male_complement <- data_male$`Complement Count`[data_male$Male == "who"]
> who_female_count <- data_female$Count[data_female$Female == "who"]
> who_female_complement <- data_female$`Complement Count`[data_female$Female == "who"]
> contingency_table <- matrix(c(
+   who_male_count, who_male_complement,
+   who_female_count, who_female_complement
+ ), nrow = 2, byrow = TRUE)
> colnames(contingency_table) <- c("Observed", "Complement")
> rownames(contingency_table) <- c("Male", "Female")
> print(round(contingency_table, 2))
       Observed Complement
Male      24805    9575290
Female    17895    7848134
> chi_result <- chisq.test(contingency_table)
> print(chi_result)

	Pearson's Chi-squared test with Yates' continuity correction

data:  contingency_table
X-squared = 168.99, df = 1, p-value < 2.2e-16

> std_residuals <- round(chi_result$stdres, 2)
> print(std_residuals)
       Observed Complement
Male         13        -13
Female      -13         13
>
> which_male_count <- data_male$Count[data_male$Male == "which"]
> which_male_complement <- data_male$`Complement Count`[data_male$Male == "which"]
> which_female_count <- data_female$Count[data_female$Female == "which"]
> which_female_complement <- data_female$`Complement Count`[data_female$Female == "which"]
> contingency_table <- matrix(c(
+   which_male_count, which_male_complement,
+   which_female_count, which_female_complement
+ ), nrow = 2, byrow = TRUE)
> colnames(contingency_table) <- c("Observed", "Complement")
> rownames(contingency_table) <- c("Male", "Female")
> print(round(contingency_table, 2))
       Observed Complement
Male      21925    9578170
Female    12941    7853088
> chi_result <- chisq.test(contingency_table)
> print(chi_result)

	Pearson's Chi-squared test with Yates' continuity correction

data:  contingency_table
X-squared = 884.86, df = 1, p-value < 2.2e-16

> std_residuals <- round(chi_result$stdres, 2)
> print(std_residuals)
       Observed Complement
Male      29.75     -29.75
Female   -29.75      29.75
>
> male_types <- length(unique(nsubj_male_analysis$lemma)) 
> male_tokens <- nrow(nsubj_male_analysis) 
> male_ttr <- male_types / male_tokens
> cat("Male TTR:", male_ttr, "\n")
Male TTR: 0.03091362 
> female_types <- length(unique(nsubj_female_analysis$lemma)) 
> female_tokens <- nrow(nsubj_female_analysis) 
> female_ttr <- female_types / female_tokens
> cat("Female TTR:", female_ttr, "\n")
Female TTR: 0.02533217
>
> shannon_index <- function(freq) {
+   p <- freq / sum(freq)
+   H <- -sum(p * log(p))
+   return(H)
+ }
> male_freq <- table(nsubj_male_analysis$lemma)
> male_shannon <- shannon_index(male_freq)
> cat("Male Shannon Diversity Index:", male_shannon, "\n")
Male Shannon Diversity Index: 5.060518 
> female_freq <- table(nsubj_female_analysis$lemma)
> female_shannon <- shannon_index(female_freq)
> cat("Female Shannon Diversity Index:", female_shannon, "\n")
Female Shannon Diversity Index: 4.736777
>
> simpson_index <- function(freq) {
+   p <- freq / sum(freq)
+   D <- 1 - sum(p^2)
+   return(D)
+ }
> male_simpson <- simpson_index(male_freq)
> female_simpson <- simpson_index(female_freq)
> cat("Male Simpson Diversity Index:", male_simpson, "\n")
Male Simpson Diversity Index: 0.9447961 
> cat("Female Simpson Diversity Index:", female_simpson, "\n")
Female Simpson Diversity Index: 0.9374316
>
> library(stringr)
> female_text <- readLines("~/Desktop/merged_texts(female author).txt")
> male_text <- readLines("~/Desktop/merged_texts(male author).txt")
> calculate_sentence_lengths <- function(text) {
+   text_combined <- paste(text, collapse = "\n")
+   sentences <- unlist(strsplit(text_combined, split = "[.!?]|\\n\\n"))
+   sentence_lengths <- sapply(sentences, function(sentence) {
+     words <- str_split(sentence, "\\s+")[[1]]
+     word_count <- length(words[words != ""])  
+     return(word_count)
+   })
+   return(sentence_lengths)
+ }
> female_sentence_lengths <- calculate_sentence_lengths(female_text)
> male_sentence_lengths <- calculate_sentence_lengths(male_text)
> female_avg_length <- mean(female_sentence_lengths)
> male_avg_length <- mean(male_sentence_lengths)
> cat(female_avg_length, "\n")
12.39413 
> cat(male_avg_length, "\n")
13.4623 
> ks_test_female <- ks.test(female_sentence_lengths, "pnorm", mean(female_sentence_lengths), sd(female_sentence_lengths))
> ks_test_male <- ks.test(male_sentence_lengths, "pnorm", mean(male_sentence_lengths), sd(male_sentence_lengths))
> print(ks_test_female)

	Asymptotic one-sample Kolmogorov-Smirnov test

data:  female_standardized
D = 0.19679, p-value < 2.2e-16
alternative hypothesis: two-sided

> print(ks_test_male)

	Asymptotic one-sample Kolmogorov-Smirnov test

data:  male_standardized
D = 0.25307, p-value < 2.2e-16
alternative hypothesis: two-sided
> mann_whitney_test <- wilcox.test(female_sentence_lengths, male_sentence_lengths)
> print(mann_whitney_test)

	Wilcoxon rank sum test with continuity correction

data:  female_sentence_lengths and male_sentence_lengths
W = 2.4944e+11, p-value = 1.81e-12
alternative hypothesis: true location shift is not equal to 0
>
> library(stringr)
> library(dplyr)
> subordinating_conjunctions <- c("that", "when", "while", "after", "before", "since", "until", "as soon as", 
+                                 "because", "as", "if", "unless", "provided that", "as long as", "so that", 
+                                 "in order that", "although", "though", "even though", "whereas", 
+                                 "as if", "as though", "how", "where", "wherever", "once")
> read_and_split_sentences <- function(file_path) {
+   text <- readLines(file_path, warn = FALSE)
+   text_combined <- paste(text, collapse = " ")
+   sentences <- unlist(strsplit(text_combined, "/\\."))
+   return(sentences)
+ }
> count_verbs <- function(sentence) {
+   verbs <- str_extract_all(sentence, "/VB\\w?")
+   return(length(verbs[[1]]))
+ }
> count_conjunctions_with_verbs <- function(sentences, conjunctions) {
+   result <- data.frame(conjunction = character(), count = integer())
+   valid_sentences <- sentences[sapply(sentences, function(sentence) {
+     word_count <- length(unlist(strsplit(sentence, "\\s+")))  
+     return(count_verbs(sentence) >= 2 && word_count <= 100)   
+   })]
+   for (conjunction in conjunctions) {
+     count <- sum(str_detect(valid_sentences, paste0("\\b", conjunction, "\\b")))
+     result <- rbind(result, data.frame(conjunction = conjunction, count = count))
+   }
+   total_count <- sum(result$count)
+   result <- rbind(result, data.frame(conjunction = "Total", count = total_count))
+   result <- result %>% arrange(desc(count))
+   return(result)
+ }
> female_author_file <- "~/Desktop/tagged_texts(female author).txt"
> male_author_file <- "~/Desktop/tagged_texts(male author).txt"
> female_sentences <- read_and_split_sentences(female_author_file)
> male_sentences <- read_and_split_sentences(male_author_file)
> female_conjunctions <- count_conjunctions_with_verbs(female_sentences, subordinating_conjunctions)
> print(female_conjunctions)
     conjunction  count
1          Total 206565
2           that  70395
3             as  42268
4           when  18347
5             if  17195
6         before   9657
7          after   7891
8            how   6705
9          where   6694
10        though   6150
11         while   5367
12          once   5070
13       because   3875
14         since   2847
15         until   2350
16      although    774
17        unless    704
18      wherever    182
19       whereas     94
20    as soon as      0
21 provided that      0
22    as long as      0
23       so that      0
24 in order that      0
25   even though      0
26         as if      0
27     as though      0
> male_conjunctions <- count_conjunctions_with_verbs(male_sentences, subordinating_conjunctions)
> print(male_conjunctions)
     conjunction  count
1          Total 232877
2           that  84178
3             as  49018
4           when  18872
5             if  17737
6         before  10066
7          after   8566
8          where   7664
9         though   6933
10           how   6709
11         while   5894
12          once   5413
13       because   4019
14         since   2964
15         until   2427
16      although    963
17        unless    807
18       whereas    432
19      wherever    215
20    as soon as      0
21 provided that      0
22    as long as      0
23       so that      0
24 in order that      0
25   even though      0
26         as if      0
27     as though      0
> comparison <- merge(female_conjunctions, male_conjunctions, by = "conjunction", suffixes = c("_female", "_male"))
> comparison <- merge(female_conjunctions, male_conjunctions, by = "conjunction", suffixes = c("_female", "_male"))
> comparison <- comparison %>% arrange(desc(count_female), desc(count_male))
> print(comparison)
     conjunction count_female count_male
1          Total       206565     232877
2           that        70395      84178
3             as        42268      49018
4           when        18347      18872
5             if        17195      17737
6         before         9657      10066
7          after         7891       8566
8            how         6705       6709
9          where         6694       7664
10        though         6150       6933
11         while         5367       5894
12          once         5070       5413
13       because         3875       4019
14         since         2847       2964
15         until         2350       2427
16      although          774        963
17        unless          704        807
18      wherever          182        215
19       whereas           94        432
20         as if            0          0
21    as long as            0          0
22    as soon as            0          0
23     as though            0          0
24   even though            0          0
25 in order that            0          0
26 provided that            0          0
27       so that            0          0
> write.csv(comparison, "~/Desktop/subordinating_conjunctions_comparison.csv", row.names = FALSE)
>
> conjunctions_two_verbs <- c("as/\\w+ if/\\w+", "as/\\w+ long/\\w+ as/\\w+", "as/\\w+ soon/\\w+ as/\\w+", 
+                            "as/\\w+ though/\\w+", "even/\\w+ though/\\w+", "in/\\w+ order/\\w+ that/\\w+", 
+                            "provided/\\w+ that/\\w+", "so/\\w+ that/\\w+")
> read_and_split_sentences <- function(file_path) {
+   text <- readLines(file_path, warn = FALSE)
+   text_combined <- paste(text, collapse = " ")
+   sentences <- unlist(strsplit(text_combined, "/\\."))
+   return(sentences)
+ }
> count_verbs <- function(sentence) {
+   verbs <- str_extract_all(sentence, "/VB\\w?")
+   return(length(verbs[[1]]))
+ }
> count_conjunctions_with_two_verbs <- function(sentences, conjunctions) {
+   result <- data.frame(conjunction = character(), count = integer())
+   valid_sentences <- sentences[sapply(sentences, function(sentence) {
+     verb_count <- count_verbs(sentence)
+     word_count <- length(unlist(strsplit(sentence, "\\s+")))
+     return(verb_count >= 2 && word_count <= 100) 
+   })]
+   for (conjunction in conjunctions) {
+     count <- sum(str_detect(valid_sentences, conjunction))
+     result <- rbind(result, data.frame(conjunction = conjunction, count = count))
+   }
+   return(result)
+ }
> female_author_file <- "~/Desktop/tagged_texts(female author).txt"
> male_author_file <- "~/Desktop/tagged_texts(male author).txt"
> female_sentences <- read_and_split_sentences(female_author_file)
> male_sentences <- read_and_split_sentences(male_author_file)
> female_conjunctions <- count_conjunctions_with_two_verbs(female_sentences, conjunctions_two_verbs)
> print(female_conjunctions)
                   conjunction count
1              as/\\w+ if/\\w+  3517
2    as/\\w+ long/\\w+ as/\\w+   307
3    as/\\w+ soon/\\w+ as/\\w+   623
4          as/\\w+ though/\\w+  1248
5        even/\\w+ though/\\w+   117
6 in/\\w+ order/\\w+ that/\\w+    62
7      provided/\\w+ that/\\w+     9
8            so/\\w+ that/\\w+  1431
> male_conjunctions <- count_conjunctions_with_two_verbs(male_sentences, conjunctions_two_verbs)
> print(male_conjunctions)
                   conjunction count
1              as/\\w+ if/\\w+  2992
2    as/\\w+ long/\\w+ as/\\w+   269
3    as/\\w+ soon/\\w+ as/\\w+   704
4          as/\\w+ though/\\w+  1322
5        even/\\w+ though/\\w+   213
6 in/\\w+ order/\\w+ that/\\w+   171
7      provided/\\w+ that/\\w+    34
8            so/\\w+ that/\\w+  2007
> comparison <- merge(female_conjunctions, male_conjunctions, by = "conjunction", suffixes = c("_female", "_male"))
> comparison <- comparison %>% arrange(desc(count_female), desc(count_male))
> print(comparison)
                   conjunction count_female count_male
1              as/\\w+ if/\\w+         3517       2992
2            so/\\w+ that/\\w+         1431       2007
3          as/\\w+ though/\\w+         1248       1322
4    as/\\w+ soon/\\w+ as/\\w+          623        704
5    as/\\w+ long/\\w+ as/\\w+          307        269
6        even/\\w+ though/\\w+          117        213
7 in/\\w+ order/\\w+ that/\\w+           62        171
8      provided/\\w+ that/\\w+            9         34
> write.csv(comparison, "~/Desktop/subordinating_conjunctions_two_verbs_comparison.csv", row.names = FALSE)
>
> file_path <- "~/Desktop/subordinating_conjunctions_comparison.csv"
> data <- read.csv(file_path)
> that_data <- data[data$conjunction == "that", ]
> contingency_table <- matrix(
+   c(that_data$count_female, that_data$complement_female, that_data$count_male, that_data$complement_male),
+   nrow = 2, byrow = TRUE
+ )
> rownames(contingency_table) <- c("Female", "Male")
> colnames(contingency_table) <- c("Count", "Complement")
> print(contingency_table)
       Count Complement
Female 70395    7795634
Male   84178    9515917
> chi_test <- chisq.test(contingency_table)
> print(chi_test)

	Pearson's Chi-squared test with Yates' continuity correction

data:  contingency_table
X-squared = 16.089, df = 1, p-value = 6.042e-05

> print(chi_test$stdres)
           Count Complement
Female  4.013722  -4.013722
Male   -4.013722   4.013722
>
> as_data <- data[data$conjunction == "as", ]
> contingency_table <- matrix(c(as_data$count_female, as_data$complement_female, as_data$count_male, as_data$complement_male),nrow = 2,byrow = TRUE)
> rownames(contingency_table) <- c("Female","Male")
> colnames(contingency_table) <- c("Count","Complement")
> print(contingency_table)
       Count Complement
Female 42268    7823761
Male   49018    9551077
> chi_test <- chisq.test(contingency_table)
> print(chi_test)

	Pearson's Chi-squared test with Yates' continuity correction

data:  contingency_table
X-squared = 59.451, df = 1, p-value = 1.254e-14

> std_residuals <- round(chi_test$stdres,2)
> print(std_residuals)
       Count Complement
Female  7.71      -7.71
Male   -7.71       7.71
>
> when_data <- data[data$conjunction == "when", ]
> contingency_table <- matrix(c(when_data$count_female, when_data$complement_female, when_data$count_male, when_data$complement_male),nrow = 2,byrow = TRUE)
> rownames(contingency_table) <- c("Female","Male")
> colnames(contingency_table) <- c("Count","Complement")
> print(contingency_table)
       Count Complement
Female 18347    7847682
Male   18872    9581223
> chi_test <- chisq.test(contingency_table)
> print(chi_test)

	Pearson's Chi-squared test with Yates' continuity correction

data:  contingency_table
X-squared = 273.12, df = 1, p-value < 2.2e-16

> std_residuals <- round(chi_test$stdres,2)
> print(std_residuals)
        Count Complement
Female  16.53     -16.53
Male   -16.53      16.53
>
> if_data <- data[data$conjunction == "if", ]
> contingency_table <- matrix(c(if_data$count_female, if_data$complement_female, if_data$count_male, if_data$complement_male),nrow = 2,byrow = TRUE)
> rownames(contingency_table) <- c("Female","Male")
> colnames(contingency_table) <- c("Count","Complement")
> print(contingency_table)
       Count Complement
Female 17195    7848834
Male   17737    9582358
> chi_test <- chisq.test(contingency_table)
> print(chi_test)

	Pearson's Chi-squared test with Yates' continuity correction

data:  contingency_table
X-squared = 247.87, df = 1, p-value < 2.2e-16

> std_residuals <- round(chi_test$stdres,2)
> print(std_residuals)
        Count Complement
Female  15.75     -15.75
Male   -15.75      15.75
>
> before_data <- data[data$conjunction == "before", ]
> contingency_table <- matrix(c(before_data$count_female, before_data$complement_female, before_data$count_male, before_data$complement_male),nrow = 2,byrow = TRUE)
> rownames(contingency_table) <- c("Female","Male")
> colnames(contingency_table) <- c("Count","Complement")
> print(contingency_table)
       Count Complement
Female  9657    7856372
Male   10066    9590029
> chi_test <- chisq.test(contingency_table)
> print(chi_test)

	Pearson's Chi-squared test with Yates' continuity correction

data:  contingency_table
X-squared = 122.87, df = 1, p-value < 2.2e-16

> std_residuals <- round(chi_test$stdres,2)
> print(std_residuals)
        Count Complement
Female  11.09     -11.09
Male   -11.09      11.09
>
> > wherever_data <- data[data$conjunction == "wherever", ]
> contingency_table <- matrix(c(wherever_data$count_female, wherever_data$complement_female, wherever_data$count_male, wherever_data$complement_male),nrow = 2,byrow = TRUE)
> rownames(contingency_table) <- c("Female","Male")
> colnames(contingency_table) <- c("Count","Complement")
> print(contingency_table)
       Count Complement
Female   182    7865847
Male     215    9599880
> chi_test <- chisq.test(contingency_table)
> print(chi_test)

	Pearson's Chi-squared test with Yates' continuity correction

data:  contingency_table
X-squared = 0.074592, df = 1, p-value = 0.7848

> std_residuals <- round(chi_test$stdres,2)
> print(std_residuals)
       Count Complement
Female  0.32      -0.32
Male   -0.32       0.32
>
> as_long_as_data <- data[data$conjunction == "as long as", ]
> contingency_table <- matrix(c(as_long_as_data$count_female, as_long_as_data$complement_female, as_long_as_data$count_male, as_long_as_data$complement_male),nrow = 2,byrow = TRUE)
> rownames(contingency_table) <- c("Female","Male")
> colnames(contingency_table) <- c("Count","Complement")
> print(contingency_table)
       Count Complement
Female   307    7865722
Male     269    9599826
> chi_test <- chisq.test(contingency_table)
> print(chi_test)

	Pearson's Chi-squared test with Yates' continuity correction

data:  contingency_table
X-squared = 15.555, df = 1, p-value = 8.014e-05

> std_residuals <- round(chi_test$stdres,2)
> print(std_residuals)
       Count Complement
Female  3.99      -3.99
Male   -3.99       3.99
>
> > even_though_data <- data[data$conjunction == "even though", ]
> contingency_table <- matrix(c(even_though_data$count_female, even_though_data$complement_female, even_though_data$count_male, even_though_data$complement_male),nrow = 2,byrow = TRUE)
> rownames(contingency_table) <- c("Female","Male")
> colnames(contingency_table) <- c("Count","Complement")
> print(contingency_table)
       Count Complement
Female   117    7865912
Male     213    9599882
> chi_test <- chisq.test(contingency_table)
> print(chi_test)

	Pearson's Chi-squared test with Yates' continuity correction

data:  contingency_table
X-squared = 11.855, df = 1, p-value = 0.0005751

> std_residuals <- round(chi_test$stdres,2)
> print(std_residuals)
       Count Complement
Female  -3.5        3.5
Male     3.5       -3.5
>
> in_order_that_data <- data[data$conjunction == "in order that", ]
> contingency_table <- matrix(c(in_order_that_data$count_female, in_order_that_data$complement_female, in_order_that_data$count_male, in_order_that_data$complement_male),nrow = 2,byrow = TRUE)
> rownames(contingency_table) <- c("Female","Male")
> colnames(contingency_table) <- c("Count","Complement")
> print(contingency_table)
       Count Complement
Female    62    7865967
Male     171    9599924
> chi_test <- chisq.test(contingency_table)
> print(chi_test)

	Pearson's Chi-squared test with Yates' continuity correction

data:  contingency_table
X-squared = 31.22, df = 1, p-value = 2.304e-08

> std_residuals <- round(chi_test$stdres,2)
> print(std_residuals)
       Count Complement
Female -5.65       5.65
Male    5.65      -5.65
>
> provided_that_data <- data[data$conjunction == "provided that", ]
> contingency_table <- matrix(c(provided_that_data$count_female, provided_that_data$complement_female, provided_that_data$count_male, provided_that_data$complement_male),nrow = 2,byrow = TRUE)
> rownames(contingency_table) <- c("Female","Male")
> colnames(contingency_table) <- c("Count","Complement")
> print(contingency_table)
       Count Complement
Female     9    7866020
Male      34    9600061
> chi_test <- chisq.test(contingency_table)
> print(chi_test)

	Pearson's Chi-squared test with Yates' continuity correction

data:  contingency_table
X-squared = 9.1438, df = 1, p-value = 0.002496

> std_residuals <- round(chi_test$stdres,2)
> print(std_residuals)
       Count Complement
Female -3.18       3.18
Male    3.18      -3.18

Sentence Complexity
> library(stringr)
> read_and_split_sentences <- function(file_path) {
+   text <- readLines(file_path, warn = FALSE)
+   text_combined <- paste(text, collapse = " ")
+   sentences <- unlist(strsplit(text_combined, "/\\."))
+   return(sentences)
+ }
> count_verbs <- function(sentence) {
+   verbs <- str_extract_all(sentence, "/VB\\w?")
+   return(length(verbs[[1]]))
+ }
> count_valid_sentences <- function(sentences) {
+   valid_sentence_count <- 0
+   for (sentence in sentences) {
+     word_count <- length(str_split(sentence, "\\s+")[[1]])
+     if (word_count <= 100 && count_verbs(sentence) >= 2) {
+       valid_sentence_count <- valid_sentence_count + 1
+     }
+   }
+   return(valid_sentence_count)
+ }
> female_author_file <- "~/Desktop/tagged_texts(female author).txt"
> male_author_file <- "~/Desktop/tagged_texts(male author).txt"
> female_sentences <- read_and_split_sentences(female_author_file)
> male_sentences <- read_and_split_sentences(male_author_file)
> female_valid_sentence_count <- count_valid_sentences(female_sentences)
> cat(female_valid_sentence_count, "\n")
323485 
> male_valid_sentence_count <- count_valid_sentences(male_sentences)
> cat(male_valid_sentence_count, "\n")
337953 
> female_total_sentences <- length(female_sentences)
> cat(female_total_sentences, "\n")
449798 
> male_total_sentences <- length(male_sentences)
> cat(male_total_sentences, "\n")
494822 
> contingency_table <- matrix(c(323485, 126313, 337953, 156869), 
+                             nrow = 2, 
+                             byrow = TRUE,
+                             dimnames = list(c("Female", "Male"), c("Subordinate Clause", "Non-Subordinate Clause")))
> chisq_test_result <- chisq.test(contingency_table)
> print(chisq_test_result)

	Pearson's Chi-squared test with Yates' continuity correction

data:  contingency_table
X-squared = 1470.7, df = 1, p-value < 2.2e-16

> std_residuals <- chisq_test_result$stdres
> print(round(std_residuals, 2))
       Subordinate Clause Non-Subordinate Clause
Female              38.35                 -38.35
Male               -38.35                  38.35
>
> #phthon
> import os
> from vaderSentiment.vaderSentiment import SentimentIntensityAnalyzer
> import pandas as pd
> analyzer = SentimentIntensityAnalyzer()
> def analyze_sentiment(file_path):
    with open(file_path, 'r', encoding='utf-8') as file:
        lines = file.readlines()
    
    results = []
    for line in lines:
        vs = analyzer.polarity_scores(line)
        results.append({
            'text': line.strip(),
            'compound': vs['compound'],
            'positive': vs['pos'],
            'negative': vs['neg'],
            'neutral': vs['neu']
        })
    return results
> desktop_path = os.path.join(os.path.expanduser("~"), 'Desktop')
> female_file_path = os.path.join(desktop_path, 'merged_texts(female author).txt')
> male_file_path = os.path.join(desktop_path, 'merged_texts(male author).txt')
> female_results = analyze_sentiment(female_file_path)
> male_results = analyze_sentiment(male_file_path)
> female_df = pd.DataFrame(female_results)
> male_df = pd.DataFrame(male_results)
> female_df.to_csv(os.path.join(desktop_path, 'female_sentiment_analysis.csv'), index=False)
> male_df.to_csv(os.path.join(desktop_path, 'male_sentiment_analysis.csv'), index=False)
>
> library(dplyr)
> female_data <- read.csv("~/Desktop/female_sentiment_analysis.csv")
> male_data <- read.csv("~/Desktop/male_sentiment_analysis.csv")
> female_avg_compound <- mean(female_data$compound, na.rm = TRUE)
> male_avg_compound <- mean(male_data$compound, na.rm = TRUE)
> cat("Average compound sentiment score for female authors:", female_avg_compound, "\n")
Average compound sentiment score for female authors: 0.05087516 
> cat("Average compound sentiment score for male authors:", male_avg_compound, "\n")
Average compound sentiment score for male authors: 0.03840988 
> library(nortest)
> female_lillie_test <- lillie.test(female_data$compound)
> print(female_lillie_test)

	Lilliefors (Kolmogorov-Smirnov) normality test

data:  female_data$compound
D = 0.29492, p-value < 2.2e-16

> male_lillie_test <- lillie.test(male_data$compound)
> print(male_lillie_test)

	Lilliefors (Kolmogorov-Smirnov) normality test

data:  male_data$compound
D = 0.30003, p-value < 2.2e-16

> wilcox_test_result <- wilcox.test(female_data$compound, male_data$compound)
> print(wilcox_test_result)

	Wilcoxon rank sum test with continuity correction

data:  female_data$compound and male_data$compound
W = 5.3393e+11, p-value < 2.2e-16
alternative hypothesis: true location shift is not equal to 0
> female_stats <- summary(female_data$compound)
> female_median <- median(female_data$compound)
> male_stats <- summary(male_data$compound)
> male_median <- median(male_data$compound)
> cat("Female Author Sentiment Analysis:\n")
Female Author Sentiment Analysis:
> cat("Mean: ", female_stats["Mean"], "\n")
Mean:  0.05087516 
> cat("Median: ", female_median, "\n")
Median:  0 
> cat("Min: ", female_stats["Min."], "\n")
Min:  -0.9774 
> cat("Max: ", female_stats["Max."], "\n\n")
Max:  0.9773 
> cat("Male Author Sentiment Analysis:\n")
Male Author Sentiment Analysis:
> cat("Mean: ", male_stats["Mean"], "\n")
Mean:  0.03840988 
> cat("Median: ", male_median, "\n")
Median:  0 
> cat("Min: ", male_stats["Min."], "\n")
Min:  -0.9749 
> cat("Max: ", male_stats["Max."], "\n")
Max:  0.9829
> library(tidytext)
> library(tidyr)
> library(dplyr)
> library(textdata)
> nrc <- get_sentiments("nrc") %>% 
+   filter(sentiment %in% c("positive", "negative"))
Do you want to download:
 Name: NRC Word-Emotion Association Lexicon 
 URL: http://saifmohammad.com/WebPages/lexicons.html 
 License: License required for commercial use. Please contact Saif M. Mohammad (saif.mohammad@nrc-cnrc.gc.ca). 
 Size: 22.8 MB (cleaned 424 KB) 
 Download mechanism: http 
 Citation info:

This dataset was published in Saif M. Mohammad and Peter Turney. (2013), ``Crowdsourcing a Word-Emotion Association Lexicon.'' Computational Intelligence, 29(3): 436-465.

article{mohammad13,
author = {Mohammad, Saif M. and Turney, Peter D.},
title = {Crowdsourcing a Word-Emotion Association Lexicon},
journal = {Computational Intelligence},
volume = {29},
number = {3},
pages = {436-465},
doi = {10.1111/j.1467-8640.2012.00460.x},
url = {https://onlinelibrary.wiley.com/doi/abs/10.1111/j.1467-8640.2012.00460.x},
eprint = {https://onlinelibrary.wiley.com/doi/pdf/10.1111/j.1467-8640.2012.00460.x},
year = {2013}
}
If you use this lexicon, then please cite it. 

1: Yes
2: No

Selection: yes
Enter an item from the menu, or 0 to exit
Selection: 1
trying URL 'http://saifmohammad.com/WebDocs/Lexicons/NRC-Emotion-Lexicon.zip'
Content type 'application/zip' length 25878449 bytes (24.7 MB)
==================================================
downloaded 24.7 MB

                                                                                                                       
> female_text <- read_file("~/Desktop/merged_texts(female author).txt")
> male_text <- read_file("~/Desktop/merged_texts(male author).txt")
> female_bigrams <- female_text %>%
+   tibble(text = .) %>%
+   unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
+   separate(bigram, c("word1", "word2"), sep = " ") %>%
+   filter(word1 %in% nrc$word | word2 %in% nrc$word)
> female_bigrams_freq <- female_bigrams %>%
+   unite(bigram, word1, word2, sep = " ") %>%
+   count(bigram, sort = TRUE)
> write.csv(female_bigrams_freq, "~/Desktop/female_emotional_bigrams_frequency2.csv", row.names = FALSE)
> male_bigrams <- male_text %>%
+   tibble(text = .) %>%
+   unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
+   separate(bigram, c("word1", "word2"), sep = " ") %>%
+   filter(word1 %in% nrc$word | word2 %in% nrc$word)
> male_bigrams_freq <- male_bigrams %>%
+   unite(bigram, word1, word2, sep = " ") %>%
+   count(bigram, sort = TRUE)
> write.csv(male_bigrams_freq, "~/Desktop/male_emotional_bigrams_frequency2.csv", row.names = FALSE)
>
> #python
> import pandas as pd
> from vaderSentiment.vaderSentiment import SentimentIntensityAnalyzer
> analyzer = SentimentIntensityAnalyzer()
> def analyze_sentiments(file_path):
    df = pd.read_csv(file_path)

    df['compound'] = df['bigram'].apply(lambda x: analyzer.polarity_scores(x)['compound'])
    df['sentiment'] = df['compound'].apply(lambda x: 'positive' if x > 0.05 else 'negative' if x < -0.05 else 'neutral')
    output_file = file_path.replace(".csv", "_sentiment_analysis.csv")
    df.to_csv(output_file, index=False)
    print(f'Results saved to {output_file}')
> female_file_path = '/Users/mac/Desktop/female_emotional_bigrams_frequency2.csv'
> male_file_path = '/Users/mac/Desktop/male_emotional_bigrams_frequency2.csv'
> analyze_sentiments(female_file_path)
> Results saved to /Users/mac/Desktop/female_emotional_bigrams_frequency2_sentiment_analysis.csv
> analyze_sentiments(male_file_path)
> Results saved to /Users/mac/Desktop/male_emotional_bigrams_frequency2_sentiment_analysis.csv
> library(data.table)
> female_data <- fread("~/Desktop/female_emotional_bigrams_frequency_sentiment_analysis.csv")
> male_data <- fread("~/Desktop/male_emotional_bigrams_frequency_sentiment_analysis.csv")
> 
> process_and_save <- function(data, gender) {
+   # Filter for positive and negative sentiments
+   positive_data <- data[sentiment == "positive", .(bigram, n, compound)]
+   negative_data <- data[sentiment == "negative", .(bigram, n, compound)]
+   
+   # Sort data by frequency
+   positive_data <- positive_data[order(-n)]
+   negative_data <- negative_data[order(-n)]
+   
+   # Save to desktop
+   fwrite(positive_data, paste0("~/Desktop/", gender, "_positive_bigrams.csv"))
+   fwrite(negative_data, paste0("~/Desktop/", gender, "_negative_bigrams.csv"))
+ }
> process_and_save(female_data, "female")
> process_and_save(male_data, "male")
>
> library(data.table)
> process_and_save_by_compound <- function(file_path, gender, sentiment) {
+     data <- fread(file_path)
+     data <- data[order(-compound)]
+     new_file_path <- paste0("~/Desktop/", gender, "_", sentiment, "_bigrams_by_compound.csv")
+     fwrite(data, new_file_path)
+     print(paste("File saved:", new_file_path))
+ }
> female_positive_path <- "~/Desktop/female_positive_bigrams2.csv"
> female_negative_path <- "~/Desktop/female_negative_bigrams2.csv"
> male_negative_path <- "~/Desktop/male_negative_bigrams2.csv"
> male_positive_path <- "~/Desktop/male_positive_bigrams2.csv"
> process_and_save_by_compound(female_positive_path, "female", "positive")
[1] "File saved: ~/Desktop/female_positive_bigrams_by_compound.csv"
> process_and_save_by_compound(female_negative_path, "female", "negative")
[1] "File saved: ~/Desktop/female_negative_bigrams_by_compound.csv"
> process_and_save_by_compound(male_positive_path, "male", "positive")
[1] "File saved: ~/Desktop/male_positive_bigrams_by_compound.csv"
> process_and_save_by_compound(male_negative_path, "male", "negative")
[1]"File saved: ~/Desktop/male_negative_bigrams_by_compound.csv"
> 
> library(readxl)
> library(writexl)
> file_path_female <- "/Users/mac/Desktop/female_positive_Standard residual contingency table.xlsx"
> file_path_male <- "/Users/mac/Desktop/male_positive_Standard residual contingency table.xlsx"                                          
> a_good_female <- data_female[data_female$Female == "a good",]
> a_good_male <- data_male[data_male$Male == "a good",]
> contingency_table <- matrix(c(
+   a_good_female$Frequency, a_good_female$`Complement Count`,
+   a_good_male$Frequency, a_good_male$`Complement Count`),
+   nrow = 2, byrow = TRUE, dimnames = list(c("Female", "Male"), c("Observed", "Complement"))
+ )
> chi_test <- chisq.test(contingency_table)
> print(chi_test)

	Pearson's Chi-squared test with Yates' continuity correction

data:  contingency_table
X-squared = 131.55, df = 1, p-value < 2.2e-16

> print(chi_test$stdres)
        Observed Complement
Female  11.48392  -11.48392
Male   -11.48392   11.48392
>
> my_dear_female <- data_female[data_female$Female == "my dear",]
> my_dear_male <- data_male[data_male$Male == "my dear",]
> contingency_table <- matrix(c(
+   my_dear_female$Frequency, my_dear_female$`Complement Count`,
+   my_dear_male$Frequency, my_dear_male$`Complement Count`),
+   nrow = 2, byrow = TRUE, dimnames = list(c("Female", "Male"), c("Observed", "Complement"))
+ )
> chi_test <- chisq.test(contingency_table)
> print(chi_test)

	Pearson's Chi-squared test with Yates' continuity correction

data:  contingency_table
X-squared = 212.22, df = 1, p-value < 2.2e-16

> print(round(chi_test$stdres, 2))
       Observed Complement
Female    14.59     -14.59
Male     -14.59      14.59
>
> i_hope_female <- data_female[data_female$Female == "i hope",]
> i_hope_male <- data_male[data_male$Male == "i hope",]
> contingency_table <- matrix(c(i_hope_female$Frequency, i_hope_female$`Complement Count`, i_hope_male$Frequency, i_hope_male$`Complement Count`), nrow = 2, byrow = TRUE, dimnames = list(c("Female", "Male"), c("Observed", "Complement")))
> chi_test <- chisq.test(contingency_table)
> print(chi_test)

	Pearson's Chi-squared test with Yates' continuity correction

data:  contingency_table
X-squared = 123.53, df = 1, p-value < 2.2e-16

> print(round(chi_test$stdres, 2))
       Observed Complement
Female    11.14     -11.14
Male     -11.14      11.14
>
> the_truth_female <- data_female[data_female$Female == "the truth",]
> the_truth_male <- data_male[data_male$Male == "the truth",]
> contingency_table <- matrix(c(the_truth_female$Frequency, the_truth_female$`Complement Count`, the_truth_male$Frequency, the_truth_male$`Complement Count`), nrow = 2, byrow = TRUE, dimnames = list(c("Female", "Male"), c("Observed", "Complement")))
> chi_test <- chisq.test(contingency_table)
> print(chi_test)

	Pearson's Chi-squared test with Yates' continuity correction

data:  contingency_table
X-squared = 0.30789, df = 1, p-value = 0.579

> print(round(chi_test$stdres, 2))
       Observed Complement
Female    -0.58       0.58
Male       0.58      -0.58
>
> no_doubt_female <- data_female[data_female$Female == "no doubt",]
> no_doubt_male <- data_male[data_male$Male == "no doubt",]
> contingency_table <- matrix(c(no_doubt_female$Frequency, no_doubt_female$`Complement Count`, no_doubt_male$Frequency, no_doubt_male$`Complement Count`), nrow = 2, byrow = TRUE, dimnames = list(c("Female", "Male"), c("Observed", "Complement")))
> chi_test <- chisq.test(contingency_table)
> print(chi_test)

	Pearson's Chi-squared test with Yates' continuity correction

data:  contingency_table
X-squared = 1.1582, df = 1, p-value = 0.2818

> print(round(chi_test$stdres, 2))
       Observed Complement
Female     -1.1        1.1
Male        1.1       -1.1
>
> the_good_female <- data_female[data_female$Female == "the good",]
> the_good_male <- data_male[data_male$Male == "the good",]
> contingency_table <- matrix(c(the_good_female$Frequency, the_good_female$`Complement Count`, the_good_male$Frequency, the_good_male$`Complement Count`), nrow = 2, byrow = TRUE, dimnames = list(c("Female", "Male"), c("Observed", "Complement")))
> chi_test <- chisq.test(contingency_table)
> print(chi_test)

	Pearson's Chi-squared test with Yates' continuity correction

data:  contingency_table
X-squared = 51.357, df = 1, p-value = 7.702e-13

> print(round(chi_test$stdres, 2))
       Observed Complement
Female    -7.19       7.19
Male       7.19      -7.19
>
> a_kind_female <- data_female[data_female$Female == "a kind",]
> a_kind_male <- data_male[data_male$Male == "a kind",]
> contingency_table <- matrix(c(a_kind_female$Frequency, a_kind_female$`Complement Count`, a_kind_male$Frequency, a_kind_male$`Complement Count`), nrow = 2, byrow = TRUE, dimnames = list(c("Female", "Male"), c("Observed", "Complement")))
> chi_test <- chisq.test(contingency_table)
> print(chi_test)

	Pearson's Chi-squared test with Yates' continuity correction

data:  contingency_table
X-squared = 10.778, df = 1, p-value = 0.001027

> print(round(chi_test$stdres, 2))
       Observed Complement
Female     3.31      -3.31
Male      -3.31       3.31
>
> the_top_female <- data_female[data_female$Female == "the top",]
> the_top_male <- data_male[data_male$Male == "the top",]
> contingency_table <- matrix(c(the_top_female$Frequency, the_top_female$`Complement Count`, the_top_male$Frequency, the_top_male$`Complement Count`), nrow = 2, byrow = TRUE, dimnames = list(c("Female", "Male"), c("Observed", "Complement")))
> chi_test <- chisq.test(contingency_table)
> print(chi_test)

	Pearson's Chi-squared test with Yates' continuity correction

data:  contingency_table
X-squared = 0.67297, df = 1, p-value = 0.412

> print(round(chi_test$stdres, 2))
       Observed Complement
Female    -0.85       0.85
Male       0.85      -0.85
>
> glad_to_female <- data_female[data_female$Female == "glad to",]
> glad_to_male <- data_male[data_male$Male == "glad to",]
> contingency_table <- matrix(c(glad_to_female$Frequency, glad_to_female$`Complement Count`, glad_to_male$Frequency, glad_to_male$`Complement Count`), nrow = 2, byrow = TRUE, dimnames = list(c("Female", "Male"), c("Observed", "Complement")))
> chi_test <- chisq.test(contingency_table)
> print(chi_test)

	Pearson's Chi-squared test with Yates' continuity correction

data:  contingency_table
X-squared = 145.32, df = 1, p-value < 2.2e-16

> print(round(chi_test$stdres, 2))
       Observed Complement
Female    12.08     -12.08
Male     -12.08      12.08
>
> great_deal_female <- data_female[data_female$Female == "great deal",]
> great_deal_male <- data_male[data_male$Male == "great deal",]
> contingency_table <- matrix(c(great_deal_female$Frequency, great_deal_female$`Complement Count`, great_deal_male$Frequency, great_deal_male$`Complement Count`), nrow = 2, byrow = TRUE, dimnames = list(c("Female", "Male"), c("Observed", "Complement")))
> chi_test <- chisq.test(contingency_table)
> print(chi_test)

	Pearson's Chi-squared test with Yates' continuity correction

data:  contingency_table
X-squared = 159.34, df = 1, p-value < 2.2e-16

> print(round(chi_test$stdres, 2))
       Observed Complement
Female    12.65     -12.65
Male     -12.65      12.65
>
> of_god_female <- data_female[data_female$Female == "of god",]
> of_god_male <- data_male[data_male$Male == "of god",]
> contingency_table <- matrix(c(of_god_female$Frequency, of_god_female$`Complement Count`, of_god_male$Frequency, of_god_male$`Complement Count`), nrow = 2, byrow = TRUE, dimnames = list(c("Female", "Male"), c("Observed", "Complement")))
> chi_test <- chisq.test(contingency_table)
> print(chi_test)

	Pearson's Chi-squared test with Yates' continuity correction

data:  contingency_table
X-squared = 241.25, df = 1, p-value < 2.2e-16

> print(round(chi_test$stdres, 2))
       Observed Complement
Female   -15.56      15.56
Male      15.56     -15.56
>
> of_love_female <- data_female[data_female$Female == "of love",]
> of_love_male <- data_male[data_male$Male == "of love",]
> contingency_table <- matrix(c(of_love_female$Frequency, of_love_female$`Complement Count`, of_love_male$Frequency, of_love_male$`Complement Count`), nrow = 2, byrow = TRUE, dimnames = list(c("Female", "Male"), c("Observed", "Complement")))
> chi_test <- chisq.test(contingency_table)
> print(chi_test)

	Pearson's Chi-squared test with Yates' continuity correction

data:  contingency_table
X-squared = 36.389, df = 1, p-value = 1.616e-09

> print(round(chi_test$stdres, 2))
       Observed Complement
Female    -6.06       6.06
Male       6.06      -6.06
>
> library(readxl)
> file_path_female <- "/Users/mac/Desktop/negative_contingency_female.xlsx"
> file_path_male <- "/Users/mac/Desktop/negative_contingency_male.xlsx"                                                                                 
> to_leave_female <- data_female[data_female$Female == "to leave",]
> to_leave_male <- data_male[data_male$Male == "to leave",]
> contingency_table <- matrix(c(to_leave_female$Frequency, to_leave_female$`Complement Count`, to_leave_male$Frequency, to_leave_male$`Complement Count`), nrow = 2, byrow = TRUE, dimnames = list(c("Female", "Male"), c("Observed", "Complement")))
> chi_test <- chisq.test(contingency_table)
> print(chi_test)

	Pearson's Chi-squared test with Yates' continuity correction

data:  contingency_table
X-squared = 56.708, df = 1, p-value = 5.056e-14

> print(round(chi_test$stdres, 2))
       Observed Complement
Female     7.55      -7.55
Male      -7.55       7.55
>
> spite_of_female <- data_female[data_female$Female == "spite of",]
> spite_of_male <- data_male[data_male$Male == "spite of",]
> contingency_table_spite_of <- matrix(c(spite_of_female$Frequency, spite_of_female$`Complement Count`, spite_of_male$Frequency, spite_of_male$`Complement Count`), nrow = 2, byrow = TRUE, dimnames = list(Gender = c("Female", "Male"), Type = c("Observed", "Complement")))
> chi_test_spite_of <- chisq.test(contingency_table_spite_of)
> print(chi_test_spite_of)

	Pearson's Chi-squared test with Yates' continuity correction

data:  contingency_table_spite_of
X-squared = 81.522, df = 1, p-value < 2.2e-16

> print(round(chi_test_spite_of$stdres, 2))
        Type
Gender   Observed Complement
  Female     9.05      -9.05
  Male      -9.05       9.05
>
> in_spite_female <- data_female[data_female$Female == "in spite",]
> in_spite_male <- data_male[data_male$Male == "in spite",]
> contingency_table_in_spite <- matrix(c(in_spite_female$Frequency, in_spite_female$`Complement Count`, in_spite_male$Frequency, in_spite_male$`Complement Count`), nrow = 2, byrow = TRUE, dimnames = list(Gender = c("Female", "Male"), Type = c("Observed", "Complement")))
> chi_test_in_spite <- chisq.test(contingency_table_in_spite)
> print(chi_test_in_spite)

	Pearson's Chi-squared test with Yates' continuity correction

data:  contingency_table_in_spite
X-squared = 87.202, df = 1, p-value < 2.2e-16

> print(round(chi_test_in_spite$stdres, 2))
        Type
Gender   Observed Complement
  Female     9.36      -9.36
  Male      -9.36       9.36
>
> > the_devil_female <- data_female[data_female$Female == "the devil",]
> the_devil_male <- data_male[data_male$Male == "the devil",]
> contingency_table_the_devil <- matrix(c(the_devil_female$Frequency, the_devil_female$`Complement Count`, the_devil_male$Frequency, the_devil_male$`Complement Count`), nrow = 2, byrow = TRUE, dimnames = list(Gender = c("Female", "Male"), Type = c("Observed", "Complement")))
> chi_test_the_devil <- chisq.test(contingency_table_the_devil)
> print(chi_test_the_devil)

	Pearson's Chi-squared test with Yates' continuity correction

data:  contingency_table_the_devil
X-squared = 8.2822, df = 1, p-value = 0.004003

> print(round(chi_test_the_devil$stdres, 2))
        Type
Gender   Observed Complement
  Female     2.91      -2.91
  Male      -2.91       2.91
>
> to_pay_female <- data_female[data_female$Female == "to pay",]
> to_pay_male <- data_male[data_male$Male == "to pay",]
> contingency_table_to_pay <- matrix(c(to_pay_female$Frequency, to_pay_female$`Complement Count`, to_pay_male$Frequency, to_pay_male$`Complement Count`), nrow = 2, byrow = TRUE, dimnames = list(Gender = c("Female", "Male"), Type = c("Observed", "Complement")))
> chi_test_to_pay <- chisq.test(contingency_table_to_pay)
> print(chi_test_to_pay)

	Pearson's Chi-squared test with Yates' continuity correction

data:  contingency_table_to_pay
X-squared = 0.033582, df = 1, p-value = 0.8546

> print(round(chi_test_to_pay$stdres, 2))
        Type
Gender   Observed Complement
  Female     0.22      -0.22
  Male      -0.22       0.22
>
> the_war_female <- data_female[data_female$Female == "the war",]
> the_war_male <- data_male[data_male$Male == "the war",]
> contingency_table_the_war <- matrix(c(the_war_female$Frequency, the_war_female$`Complement Count`, the_war_male$Frequency, the_war_male$`Complement Count`), nrow = 2, byrow = TRUE, dimnames = list(Gender = c("Female", "Male"), Type = c("Observed", "Complement")))
> chi_test_the_war <- chisq.test(contingency_table_the_war)
> print(chi_test_the_war)

	Pearson's Chi-squared test with Yates' continuity correction

data:  contingency_table_the_war
X-squared = 15.407, df = 1, p-value = 8.668e-05

> print(round(chi_test_the_war$stdres, 2))
        Type
Gender   Observed Complement
  Female    -3.96       3.96
  Male       3.96      -3.96
>
> a_bad_female <- data_female[data_female$Female == "a bad",]
> a_bad_male <- data_male[data_male$Male == "a bad",]
> contingency_table_a_bad <- matrix(c(a_bad_female$Frequency, a_bad_female$`Complement Count`, a_bad_male$Frequency, a_bad_male$`Complement Count`), nrow = 2, byrow = TRUE, dimnames = list(Gender = c("Female", "Male"), Type = c("Observed", "Complement")))
> chi_test_a_bad <- chisq.test(contingency_table_a_bad)
> print(chi_test_a_bad)

	Pearson's Chi-squared test with Yates' continuity correction

data:  contingency_table_a_bad
X-squared = 26.012, df = 1, p-value = 3.393e-07

> print(round(chi_test_a_bad$stdres, 2))
        Type
Gender   Observed Complement
  Female     5.14      -5.14
  Male      -5.14       5.14
>
> the_darkness_female <- data_female[data_female$Female == "the darkness",]
> the_darkness_male <- data_male[data_male$Male == "the darkness",]
> contingency_table_the_darkness <- matrix(c(the_darkness_female$Frequency, the_darkness_female$`Complement Count`, the_darkness_male$Frequency, the_darkness_male$`Complement Count`), nrow = 2, byrow = TRUE, dimnames = list(Gender = c("Female", "Male"), Type = c("Observed", "Complement")))
> chi_test_the_darkness <- chisq.test(contingency_table_the_darkness)
> print(chi_test_the_darkness)

	Pearson's Chi-squared test with Yates' continuity correction

data:  contingency_table_the_darkness
X-squared = 1.029, df = 1, p-value = 0.3104

> print(round(chi_test_the_darkness$stdres, 2))
        Type
Gender   Observed Complement
  Female     1.05      -1.05
  Male      -1.05       1.05
>
> fear_of_female <- data_female[data_female$Female == "fear of",]
> fear_of_male <- data_male[data_male$Male == "fear of",]
> contingency_table_fear_of <- matrix(c(fear_of_female$Frequency, fear_of_female$`Complement Count`, fear_of_male$Frequency, fear_of_male$`Complement Count`), nrow = 2, byrow = TRUE, dimnames = list(Gender = c("Female", "Male"), Type = c("Observed", "Complement")))
> chi_test_fear_of <- chisq.test(contingency_table_fear_of)
> print(chi_test_fear_of)

	Pearson's Chi-squared test with Yates' continuity correction

data:  contingency_table_fear_of
X-squared = 4.8439, df = 1, p-value = 0.02774

> print(round(chi_test_fear_of$stdres, 2))
        Type
Gender   Observed Complement
  Female    -2.24       2.24
  Male       2.24      -2.24
>
> leave_the_female <- data_female[data_female$Female == "leave the",]
> leave_the_male <- data_male[data_male$Male == "leave the",]
> contingency_table_leave_the <- matrix(c(leave_the_female$Frequency, leave_the_female$`Complement Count`, leave_the_male$Frequency, leave_the_male$`Complement Count`), nrow = 2, byrow = TRUE, dimnames = list(Gender = c("Female", "Male"), Type = c("Observed", "Complement")))
> chi_test_leave_the <- chisq.test(contingency_table_leave_the)
> print(chi_test_leave_the)

	Pearson's Chi-squared test with Yates' continuity correction

data:  contingency_table_leave_the
X-squared = 0.043272, df = 1, p-value = 0.8352

> print(round(chi_test_leave_the$stdres, 2))
        Type
Gender   Observed Complement
  Female    -0.24       0.24
  Male       0.24      -0.24
>
> of_death_female <- data_female[data_female$Female == "of death",]
> of_death_male <- data_male[data_male$Male == "of death",]
> contingency_table_of_death <- matrix(c(of_death_female$Frequency, of_death_female$`Complement Count`, of_death_male$Frequency, of_death_male$`Complement Count`), nrow = 2, byrow = TRUE, dimnames = list(Gender = c("Female", "Male"), Type = c("Observed", "Complement")))
> chi_test_of_death <- chisq.test(contingency_table_of_death)
> print(chi_test_of_death)

	Pearson's Chi-squared test with Yates' continuity correction

data:  contingency_table_of_death
X-squared = 28.657, df = 1, p-value = 8.639e-08

> print(round(chi_test_of_death$stdres, 2))
        Type
Gender   Observed Complement
  Female    -5.39       5.39
  Male       5.39      -5.39
>
> the_death_female <- data_female[data_female$Female == "the death",]
> the_death_male <- data_male[data_male$Male == "the death",]
> contingency_table_the_death <- matrix(c(the_death_female$Frequency, the_death_female$`Complement Count`, the_death_male$Frequency, the_death_male$`Complement Count`), nrow = 2, byrow = TRUE, dimnames = list(Gender = c("Female", "Male"), Type = c("Observed", "Complement")))
> chi_test_the_death <- chisq.test(contingency_table_the_death)
> print(chi_test_the_death)

	Pearson's Chi-squared test with Yates' continuity correction

data:  contingency_table_the_death
X-squared = 62.771, df = 1, p-value = 2.321e-15

> print(round(chi_test_the_death$stdres, 2))
        Type
Gender   Observed Complement
  Female    -7.96       7.96
  Male       7.96      -7.96
>
> > to_death_female <- data_female[data_female$Female == "to death",]
> to_death_male <- data_male[data_male$Male == "to death",]
> contingency_table_to_death <- matrix(c(to_death_female$Frequency, to_death_female$`Complement Count`, to_death_male$Frequency, to_death_male$`Complement Count`), nrow = 2, byrow = TRUE, dimnames = list(Gender = c("Female", "Male"), Type = c("Observed", "Complement")))
> chi_test_to_death <- chisq.test(contingency_table_to_death)
> print(chi_test_to_death)

	Pearson's Chi-squared test with Yates' continuity correction

data:  contingency_table_to_death
X-squared = 19.522, df = 1, p-value = 9.945e-06

> print(round(chi_test_the_death$stdres, 2))
        Type
Gender   Observed Complement
  Female    -7.96       7.96
  Male       7.96      -7.96
