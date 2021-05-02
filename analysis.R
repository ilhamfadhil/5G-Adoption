library(tidyverse)

data_path <- file.path(".", "data", "5g_adoption interaksi-table-2.csv")

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
  facet_wrap(~ memory + teman) +
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
  geom_line() +
  labs(x = "Tick", 
       y = "Jumlah orang", 
       color = "Case")

adopt_df %>%
  select(-adopt) %>%
  group_by(n, memory, teman) %>%
  summarise(red = mean(red), 
            yellow = mean(yellow), 
            blue = mean(blue)) %>%
  gather("tipe", "value", -n, -teman, -memory) %>%
  ggplot(aes(x = n, 
             y = value, 
             color = tipe)) +
  geom_line() +
  scale_color_manual(values = group.colors) +
  facet_wrap(~ memory + teman) +
  labs(x = "Tick", 
       y = "Jumlah orang", 
       color = "MNO")

adopt_df %>%
  select(n, memory, teman, adopt) %>%
  group_by(n, memory, teman) %>%
  summarise(adopt = mean(adopt)) %>%
  ggplot(aes(x = n ,
             y = adopt)) +
  geom_line() +
  facet_wrap(~ memory + teman) +
  labs(x = "Tick", 
       y = "Jumlah orang")

perc_df <- df %>%
  rename(n = `[step]`, 
        perc_ind = `perc-adopt?-industries`, 
        perc_adopt = `perc-adopt?`,
        memory = `memory?`,
        teman = `teman?`, 
        seed = `seed-number`) %>%
  select(n, perc_ind, perc_adopt, memory, teman, seed)

perc_df %>%
  select(-seed) %>%
  group_by(n, memory, teman) %>%
  summarise(perc_ind = mean(perc_ind), 
            perc_adopt = mean(perc_adopt)) %>%
  gather("tipe", "value", -n, -memory, -teman) %>%
  ggplot(aes(x = n, 
             y = value,
             color = tipe)) +
  geom_line(lwd = 1) +
  scale_y_continuous(labels = scales::percent_format()) +
  facet_wrap(~ memory + teman) +
  labs(x = "Ticks", 
       y = "Presentase", 
       color = "Jenis")
