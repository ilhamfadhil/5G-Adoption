library(tidyverse)

data_path <- file.path(".", "data", "5g_adoption interaksi-table.csv")

df <- read_csv(data_path, skip = 6)

dim(df)
colnames(df)
table(df$`[run number]`)
head(df)

adopt_df <- df %>%
  filter(`teman?` == TRUE && `memory?` == TRUE) %>%
  select(`[step]`, `count-adopt?`, `count-red-adopt?`, 
         `count-blue-adopt?`, `count-yellow-adopt?`,
         `memory?`, `teman?`) %>%
  rename(n = `[step]`, 
         adopt = `count-adopt?`, 
         red = `count-red-adopt?`, 
         blue = `count-blue-adopt?`, 
         yellow = `count-yellow-adopt?`, 
         memory = `memory?`, 
         teman = `teman?`)

group.colors <- c("red" = "firebrick3", 
                  "yellow" = "goldenrod3", 
                  "blue" = "royalblue3")

adopt_df %>%
  filter(n == max(n)) %>%
  select(-adopt) %>%
  gather("tipe", "value", -n, -teman, -memory) %>%
  ggplot(aes(x = value, 
             fill = tipe, 
             color = tipe)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = group.colors) +
  scale_color_manual(values = group.colors) +
  facet_wrap(~memory + teman) +
  labs(fill = "MNO", 
       color = "MNO",
       x = "Jumlah adopsi", 
       y = "Density")

adopt_df %>%
  group_by(n, memory, teman) %>%
  summarise(mean_adopt = mean(adopt)) %>%
  mutate(case = ifelse(memory == FALSE & teman == FALSE, 1, ifelse(
    memory == FALSE & teman == TRUE, 2, ifelse(
      memory == TRUE & teman == FALSE, 3, 4
    )))) %>%
  ggplot(aes(x = n , 
             y = mean_adopt, 
             color = factor(case))) +
  geom_line()
