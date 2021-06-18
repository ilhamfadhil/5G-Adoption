library(tidyverse)
library(forcats)

data_path <- file.path(".", "data", "5g_adoption experiment-final-table.csv")
data_path4232 <- file.path(".", "data", "5g_adoption experiment-final-table4232.csv")

df <- read_csv(data_path, skip = 6)
df4232 <- read_csv(data_path4232, skip = 6)

dim(df)
colnames(df)
table(df$`[run number]`)
head(df)

adopt_df <- df %>%
  select(`[step]`, `count-adopt?`, `count-red-adopt?`, 
         `count-blue-adopt?`, `count-yellow-adopt?`,
         `memory?`, `teman?`, `average-mno-sharing`, 
         `average-govt-incentive`, `average-local-govt-cooperation`, 
         `infra-co-innovation`) %>%
  rename(n = `[step]`, 
         adopt = `count-adopt?`, 
         red = `count-red-adopt?`, 
         blue = `count-blue-adopt?`, 
         yellow = `count-yellow-adopt?`, 
         memory = `memory?`, 
         teman = `teman?`, 
         mno_sharing = `average-mno-sharing`, 
         govt_incentive = `average-govt-incentive`, 
         govt_local_coop = `average-local-govt-cooperation`,
         infra_co_innov = `infra-co-innovation`)

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
  geom_line(lwd = 1) +
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

perc_df %>%
  group_by(memory, teman, seed) %>%
  filter(perc_ind > 0.95) %>%
  filter(n == min(n)) %>%
  ggplot(aes(x = n)) +
  geom_density(fill = "grey") +
  facet_wrap(~ memory + teman) +
  labs(x = "Ticks", 
       y = "Density")
  
perc_df %>%
  group_by(memory, teman, seed) %>%
  filter(perc_adopt > 0.95) %>%
  filter(n == min(n)) %>%
  ggplot(aes(x = n)) +
  geom_density(fill = "grey") +
  facet_wrap(~ memory + teman) +
  labs(x = "Ticks", 
       y = "Density")

##### Analisis Skenario parameter

adopt_scen_df <- df %>%
  select(`[step]`, `count-adopt?`, `count-red-adopt?`, 
         `count-blue-adopt?`, `count-yellow-adopt?`, `average-mno-sharing`, 
         `average-govt-incentive`, `average-local-govt-cooperation`, 
         `[run number]`, `perc-adopt?-industries`, `perc-adopt?`, 
         `seed-number`, 
         `infra-co-innovation`) %>%
  rename(n = `[step]`, 
         adopt = `count-adopt?`, 
         red = `count-red-adopt?`, 
         blue = `count-blue-adopt?`, 
         yellow = `count-yellow-adopt?`, 
         mno_sharing = `average-mno-sharing`, 
         govt_incentive = `average-govt-incentive`, 
         govt_local_coop = `average-local-govt-cooperation`, 
         run_number = `[run number]`, 
         perc_ind = `perc-adopt?-industries`, 
         perc_adopt = `perc-adopt?`,
         seed = `seed-number`, 
         infra_co_innov = `infra-co-innovation`) %>%
  mutate(group = str_c(mno_sharing, govt_incentive, govt_local_coop, infra_co_innov))

adopt_scen_df4232 <- df4232 %>%
  select(`[step]`, `count-adopt?`, `count-red-adopt?`, 
         `count-blue-adopt?`, `count-yellow-adopt?`, `average-mno-sharing`, 
         `average-govt-incentive`, `average-local-govt-cooperation`, 
         `[run number]`, `perc-adopt?-industries`, `perc-adopt?`, 
         `seed-number`, 
         `infra-co-innovation`) %>%
  rename(n = `[step]`, 
         adopt = `count-adopt?`, 
         red = `count-red-adopt?`, 
         blue = `count-blue-adopt?`, 
         yellow = `count-yellow-adopt?`, 
         mno_sharing = `average-mno-sharing`, 
         govt_incentive = `average-govt-incentive`, 
         govt_local_coop = `average-local-govt-cooperation`, 
         run_number = `[run number]`, 
         perc_ind = `perc-adopt?-industries`, 
         perc_adopt = `perc-adopt?`,
         seed = `seed-number`, 
         infra_co_innov = `infra-co-innovation`) %>%
  mutate(group = str_c(mno_sharing, govt_incentive, govt_local_coop, infra_co_innov))

comparison_dataset <- bind_rows(adopt_scen_df, adopt_scen_df4232) %>%
  filter(group %in% c("1110", "4232"))

dim(adopt_scen_df)
colSums(is.na(adopt_scen_df))

adopt_scen_df %>%
  group_by(n, mno_sharing, govt_incentive, govt_local_coop, infra_co_innov) %>%
  summarise(mean_adopt = mean(adopt)) %>%
  ggplot(aes(x = n, y = mean_adopt, group = group, color = group)) +
  geom_line()

adopt_scen_df %>%
  group_by(seed) %>%
  filter(perc_adopt > 0.95) %>%
  filter(n == min(n)) %>%
  mutate(group = str_c(mno_sharing, govt_incentive, govt_local_coop, infra_co_innov)) %>%
  ggplot(aes(x = n, fill = group)) +
  geom_density(alpha = 0.3) +
  labs(x = "Ticks", 
       y = "Density")

adopt_scen_df %>%
  group_by(seed) %>%
  filter(perc_adopt > 0.9) %>%
  filter(n == min(n)) %>%
  ungroup() %>%
  mutate(group = str_c(mno_sharing, govt_incentive, govt_local_coop, infra_co_innov)) %>%
  group_by(group) %>%
  mutate(mean_perc = mean(n)) %>%
  ggplot(aes(x = fct_rev(group), y = n)) +
  geom_boxplot() +
  geom_point(aes(y = mean_perc), 
             color = 'red') +
  labs(x = "Skenario", 
       y = "Final Tick", 
       caption = "Parameter: mno sharing/govt incentive/govt local coop/infra") +
  coord_flip() 

adopt_scen_df %>%
  group_by(seed) %>%
  filter(perc_ind > 0.9) %>%
  filter(n == min(n)) %>%
  ungroup() %>%
  mutate(group = str_c(mno_sharing, govt_incentive, govt_local_coop, infra_co_innov)) %>%
  group_by(group) %>%
  mutate(mean_perc = mean(n)) %>%
  ggplot(aes(x = fct_rev(group), y = n)) +
  geom_boxplot() +
  geom_point(aes(y = mean_perc), 
             color = 'red') +
  labs(x = "Skenario", 
       y = "Final Tick", 
       caption = "Parameter: mno sharing/govt incentive/govt local coop/infra") +
  coord_flip() 

comparison_dataset %>%
  group_by(seed) %>%
  filter(perc_adopt >= 0.95) %>%
  filter(n == min(n)) %>%
  ungroup() %>%
  mutate(group = str_c(mno_sharing, govt_incentive, govt_local_coop, infra_co_innov)) %>%
  group_by(group) %>%
  mutate(mean_perc = mean(n)) %>%
  ggplot(aes(x = fct_rev(group), y = n)) +
  geom_boxplot() +
  geom_point(aes(y = mean_perc), 
             color = 'red') +
  labs(x = "Skenario", 
       y = "Final Tick", 
       caption = "Parameter: mno sharing/govt incentive/govt local coop/infra",
       title = "Tick Reach 95% 5G Adoption") +
  coord_flip() +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))
