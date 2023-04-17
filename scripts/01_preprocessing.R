library(tidyverse)

data <- read_tsv('data/export_01.txt') %>% 
  select(-contains("span")) %>% 
  rename(tok_id = "1_id",
         tok = "1_anno_default_ns:norm",
         doc = "1_meta_annis:doc",
         pos_id = "2_id",
         pos = "2_anno_default_ns:pos_lang",
         cu_id = "3_id",
         cu = "3_anno_default_ns:cu") 

data <- data %>% 
  mutate(
    Group = case_when(
      startsWith(doc, "DE") ~ "Germany",
      startsWith(doc, "De") ~ "Germany",
      startsWith(doc, "US") ~ "USA",
      startsWith(doc, "Us") ~ "USA",
      startsWith(doc, "TU") ~ "Turkey",
      startsWith(doc, "Tu") ~ "Turkey"
      ),
    Register = case_when(
      grepl("_f", doc) ~ "formal",
      grepl("_i", doc) ~ "informal"
      ),
    mode = case_when(
      grepl("_fs", doc) ~ "spoken",
      grepl("_is", doc) ~ "spoken",
      grepl("_fw", doc) ~ "written",
      grepl("_iw", doc) ~ "written",
    )) %>% 
  mutate(Speaker = substr(doc, start=1, stop=6)) %>% 
  filter(mode == "spoken")

###########################
###   Preprocessing     ###
###########################
data <- data %>%
  mutate(discourse = ifelse(test = pos == "CO", yes = 1, no = 0),
         hesitation = ifelse(test = pos == "CO" & tok == "e", yes = 1, no = 0))

# filtering for multiple initial discourse/hesitation markers
filtered <- tibble()
for(i in 1:nrow(data)) {
  if(data[i, "discourse"] == 1 & data[i+1, "discourse"] == 1 & 
     # only filter if in same utterance
     data[i, "cu_id"] == data[i+1, "cu_id"]){}
  else{filtered <- filtered %>% rbind(data[i,])} 
}

for(i in 1:nrow(filtered)) {
  filtered[i, "Position"] <- ifelse(
   test = as.character(filtered[i, "cu_id"]) != as.character(filtered[i-1, "cu_id"]), 
   yes = "Initial", no = ifelse(
     test = as.character(filtered[i, "cu_id"]) != as.character(filtered[i+1, "cu_id"]), 
     yes = "Final", no = "Medial"))
}

nums <- data %>% filter(discourse != 1) %>% group_by(cu_id) %>% mutate(utt_length = n())
nums <- unique(nums[c("cu_id", "utt_length")])

final_data <- filtered %>% 
  left_join(nums, by = "cu_id") %>% 
  mutate(utt_length = ifelse(is.na(utt_length), 0, utt_length)) %>% 
  mutate(z_uttLength = (utt_length - mean(utt_length)) / sd(utt_length))

write_csv(final_data, 'data/data_preprocessed_dis.csv')
