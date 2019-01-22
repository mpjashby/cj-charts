library(ggrepel)
library(httr)
library(janitor)
library(magrittr)
library(tidyverse)

# download Home Office police workforce data and extract table of officers by
# rank and police force
workforce_file <- tempfile()
GET("https://www.gov.uk/government/uploads/system/uploads/attachment_data/file/728153/police-workforce-tabs-jul18.ods") %>% 
  content("raw") %>% 
  writeBin(workforce_file)
# skip the first four rows, which are metadata we don't need
workforce <- readODS::read_ods(workforce_file, sheet = 7, skip = 4, 
                               col_names = FALSE)

# extract column titles
ranks <- workforce[1, ] %>% 
  gather() %>% 
  pluck("value") %>% 
  na.omit() %>% 
  trimws() %>% 
  append("Force", after = 0)

# tidy data
workforce <- workforce[2:(nrow(workforce) - 8), 1:(ncol(workforce) - 4)] %>% 
  as_tibble() %>% 
  remove_empty() %>% 
  set_colnames(ranks) %>% 
  clean_names() %>% 
  select(-total_male_ranks, -total_female_ranks) %>% 
  rename_at(vars(ends_with("_2")), ~ str_replace(., "\\_2", "_f")) %>% 
  rename_at(vars(-ends_with("_f")), ~ paste0(., "_m")) %>% 
  gather(key = "rank", value = "count", -force_m) %>% 
  rename(force = force_m) %>% 
  mutate(count = as.integer(count))

# aggregate across sexes
workforce <- workforce %>% 
  mutate(rank = str_sub(rank, 0, -3)) %>% 
  group_by(force, rank) %>% 
  summarise(count = sum(count)) %>% 
  spread(rank, count) %>% 
  adorn_totals("col") %>% 
  rename(total = Total) %>% 
  mutate(
    senior_officer = chief_officer + chief_superintendent + superintendent,
    senior_ratio = constable / senior_officer
  )

# plot
workforce %>% 
  filter(!force %in% c("Total of 43 forces", "London, City of")) %>% 
  mutate(
    average = ifelse(senior_ratio > mean(senior_ratio), "above average",
                     "below average"),
    label = paste0(force, " (", round(senior_ratio), ")"),
    label = case_when(
      force %in% head(pluck(arrange(., desc(senior_ratio)), "force"), 10) ~ 
        label,
      force %in% head(pluck(arrange(., senior_ratio), "force"), 10) ~ label,
      force %in% head(pluck(arrange(., desc(total)), "force"), 3) ~ label,
      total == min(total) ~ label,
      TRUE ~ ""
    )
  ) %>% 
  ggplot(aes(x = constable, y = senior_ratio, label = label)) +
  geom_hline(
    yintercept = mean(workforce$senior_ratio[!workforce$force %in% c("Total of 43 forces", "London, City of")]),
    colour = "grey50",
    linetype = "44"
  ) +
  geom_smooth(colour = "grey60", fill = "grey80", size = 0.5, se = FALSE) +
  geom_label_repel(size = 2.5, label.size = 0, min.segment.length = 0, force = 10,
                   label.padding = unit(0.1, "lines")) +
  geom_point(shape = 21) +
  annotate("label", 
           x = max(workforce$constable[workforce$force != "Total of 43 forces"]),
           y = mean(workforce$senior_ratio[!workforce$force %in% c("Total of 43 forces", "London, City of")]),
           label = paste0("national average = ", round(mean(workforce$senior_ratio[!workforce$force %in% c("Total of 43 forces", "London, City of")])), " PCs per senior officer"), 
           hjust = "right", size = 3, label.size = 0, lineheight = 0.9) +
  scale_x_log10(labels = scales::comma_format()) +
  scale_y_continuous() +
  labs(
    # title = "How many chiefs?",
    subtitle = paste("Some large forces have more than twice as many\nPCs",
                     "per senior officer as some small ones†"),
    caption = "* superintendent and above   †excluding City of London Police",
    x = "number of PCs (log scale)",
    y = "PCs per senior officer"
  ) +
  theme_minimal(base_family = "Helvetica") + 
  theme(
    axis.title = element_text(colour = "grey40", size = 9),
    axis.ticks = element_line(),
    legend.position = "bottom",
    panel.grid = element_blank(),
    plot.caption = element_text(colour = "grey40", size = 8)
  )

