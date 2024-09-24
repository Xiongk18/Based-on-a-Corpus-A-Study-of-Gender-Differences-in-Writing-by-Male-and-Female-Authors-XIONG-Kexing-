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
