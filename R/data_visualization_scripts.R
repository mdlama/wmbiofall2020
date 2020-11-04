library(tidyverse)
library(gridExtra)
stemdata <- read_csv("https://qubeshub.org/community/groups/introbiostats/File:/uploads/stemdata.csv")

site <- c("BLD1", "BLD2", "PWR", "SKY", "YTB")

stemdata %>%
  group_by(site) %>%
  summarize(count = n()) %>%
  mutate(xmin = 0.55 + 0:4,
         xmax = 1.45 + 0:4,
         ymin = 0,
         ymax = count) %>%
  ggplot() +
  geom_rect(aes(xmin = xmin,
                xmax = xmax,
                ymin = ymin,
                ymax = ymax)) +
  scale_x_discrete(limits = as.character(1:5),
                   labels = site,
                   name = "site") +
  scale_y_continuous(name = "count",
                     limits = c(-10, 1900),
                     breaks = c(0, 500, 1000, 1500))

mybar <- stemdata %>%
  ggplot() +
  geom_bar(aes(x = site,
               y = after_stat(count)))

stemdata %>% select(year:h_apical, -X1, -X) %>% head() %>% grid.table(row = NULL)

stemdata %>% group_by(site) %>% summarize(count = n()) %>% grid.table(row = NULL)

stemdata %>%
  mutate(site = fct_recode(site, "1" = "BLD1", "2" = "BLD2", "3" = "PWR", "4" = "SKY", "5" = "YTB")) %>%
  select(x = site) %>%
  head() %>% grid.table(row = NULL)

layer_data(mybar) %>%
  select(x, count) %>%
  grid.table(rows = NULL)

layer_data(mybar) %>%
  select(x, y = count) %>%
  grid.table(rows = NULL)

layer_data(mybar) %>%
  select(x, y, xmin, xmax, ymin, ymax, fill) %>%
  grid.table(rows=NULL)

# Stacked bar plot
mystack <- stemdata %>%
  mutate(year = as.character(year)) %>%
  ggplot() +
  geom_bar(aes(x = site,
               fill = year))

mystack_id <- stemdata %>%
  ggplot() +
  geom_bar(aes(x = site,
               fill = as.factor(year)),
           position = "identity")

mystack_themed <- stemdata %>%
  mutate(year = as.character(year)) %>%
  ggplot() +
  geom_bar(aes(x = site,
               fill = year)) +
  theme_classic()

scales::hue_pal()(5)
stemdata %>%
  mutate(site = fct_recode(site, "1" = "BLD1", "2" = "BLD2", "3" = "PWR", "4" = "SKY", "5" = "YTB"),
         year = fct_recode(as.character(year), "#F8766D" = "2013", "#A3A500" = "2014", "#00BF7D" = "2015", "#00B0F6" = "2016", "#E76BF3" = "2017")) %>%
  select(x = site, fill = year) %>%
  head() %>% grid.table(row = NULL)

layer_data(mystack) %>%
  select(x, fill, count) %>%
  head() %>% grid.table(rows = NULL)

layer_data(mystack) %>%
  select(x, y = count, fill = fill) %>%
  head() %>% grid.table(rows = NULL)

layer_data(mystack_id) %>%
  select(x, y, xmin, xmax, ymin, ymax, fill) %>%
  head() %>% grid.table(rows=NULL)

layer_data(mystack) %>%
  select(x, y, xmin, xmax, ymin, ymax, fill) %>%
  head() %>% grid.table(rows=NULL)

# ---------

tmp <- stemdata %>%
  ggplot() +
  geom_bar(aes(x = site))

tmp2 <- stemdata %>%
  ggplot() +
  geom_rect(aes(x = site,
                ymax = after_stat(count)),
            xmin = 0.55 + 0:4,
            xmax = 1.45 + 0:4,
            ymin = 0,
            stat = "count") +
  scale_y_continuous(limits = c(0, 1900))

diamonds %>%
  ggplot() +
  geom_rect(
    aes(
      x = cut,
      ymax = after_stat(count)
    ),
    xmin = 0.55 + 0:4,
    xmax = 1.45 + 0:4,
    ymin = 0,
    stat = "count"
  )


tiles <- tribble(
  ~x, ~y, ~fill, ~color,
  1, 1, "forestgreen", "goldenrod1",
  1, -1, "forestgreen", "goldenrod1",
  -1, -1, "forestgreen", "goldenrod1",
  -1, 1, "forestgreen", "goldenrod1")

ggplot(data = tiles) +
  geom_tile(mapping = aes(x = x, y = y, fill = fill, color = color),
            width = 1,
            height = 1,
            size = 2) +
  scale_fill_identity() +
  scale_color_identity()

tiles <- tribble(
  ~x, ~y, ~fill, ~color,
  1, 1, "william", "mary",
  1, -1, "william", "mary",
  -1, -1, "william", "mary",
  -1, 1, "william", "mary")

ggplot(data = tiles) +
  geom_tile(mapping = aes(x = x, y = y, fill = fill, color = color),
            width = 1,
            height = 1,
            size = 2,
            show.legend = FALSE) +
  scale_fill_manual(values = c("william" = "forestgreen")) +
  scale_color_manual(values = c("mary" = "goldenrod1"))

stemdata %>%
  filter(herb_avg > 0) %>%
  ggplot(aes(x = h_apical,
             y = herb_avg)) +
  geom_point(alpha = 0.5,
             shape = 21) +
  geom_smooth(method = "lm", fill = NA) +
  scale_y_continuous(trans = "log10")

stemdata %>%
  filter(herb_avg > 0) %>%
  ggplot(aes(x = h_apical,
             y = herb_avg)) +
  geom_point(alpha = 0.5,
             shape = 21) +
  geom_smooth(method = "lm", fill = NA) +
  coord_trans(y = "log10")

# Pie chart
stemdata %>%
  mutate(year = as.character(year)) %>%
  ggplot() +
  geom_bar(aes(x = site,
               fill = year),
           color = "black") +
  scale_x_discrete(limits = c("BLD1")) +
  coord_polar(theta = "y", direction = -1) +
  labs(x = NULL, y = NULL) +
  theme_void()

# Pie chart w/ labels
stemdata %>%
  mutate(year = as.character(year)) %>%
  ggplot() +
  geom_bar(aes(x = site,
               fill = year),
           color = "black") +
  geom_text(aes(x = site,
                label = year,
                group = year),
            stat = "count",
            position = position_stack(vjust = 0.5)) +
  scale_x_discrete(limits = c("BLD1")) +
  scale_fill_discrete(guide = NULL) +
  coord_polar(theta = "y", direction = -1) +
  theme_void()

# Pie chart w/ labels (better positioning)
stemdata %>%
  mutate(year = as.character(year)) %>%
  ggplot() +
  geom_bar(aes(x = site,
               fill = year),
           color = "black") +
  geom_text(aes(x = stage(start = site, after_stat = x + 0.2),
                label = year,
                group = year),
            stat = "count",
            position = position_stack(vjust = 0.5)) +
  scale_x_discrete(limits = c("BLD1")) +
  scale_fill_discrete(guide = NULL) +
  coord_polar(theta = "y", direction = -1) +
  theme_void()

# Donut plot
stemdata %>%
  mutate(year = as.character(year)) %>%
  ggplot() +
  geom_bar(aes(x = stage(start = site, after_stat = x + 2),
               fill = year),
           color = "black") +
  scale_x_discrete(limits = c("BLD1")) +
  coord_polar(theta = "y", direction = -1) +
  labs(x = NULL, y = NULL) +
  theme_void()

# Bullseye plot
stemdata %>%
  mutate(year = as.character(year)) %>%
  ggplot() +
  geom_bar(aes(x = site,
               fill = year)) +
  scale_x_discrete(limits = c("BLD1"), expand = c(0, 0)) +
  coord_polar() +
  labs(x = NULL, y = NULL) +
  theme_void()

# Bullseye plot w/ edges
tmp <- stemdata %>%
  mutate(year = as.character(year)) %>%
  ggplot() +
  geom_bar(aes(x = site,
               fill = year)) +
  geom_segment(aes(x = stage(start = site, after_stat = 0.55),
                   xend = 1.45,
                   y = after_stat(count),
                   yend = stage(start = NULL, after_stat = count, after_scale = y), # can be y or ymax
                   group = year),
             stat = "count",
             position = "stack") +
  scale_x_discrete(limits = c("BLD1"), expand = c(0, 0)) +
  coord_polar(direction = 1) +
  labs(x = NULL, y = NULL) +
  theme_void()

stemdata %>%
  filter(site == "SKY", !is.na(h_apical), !is.na(h_apical.next)) %>%
  sample_n(10) %>%
  select(plantID, h_apical, h_apical.next) %>%
  pivot_longer(cols = starts_with("h_apical"), names_to = "time", values_to = "height") %>%
  ggplot(aes(x = time, y = height, group = plantID)) +
  geom_line() +
  geom_point(color = "red")

mean_data <- stemdata %>%
  filter(site == "BLD2", year == "2017") %>%
  summarize(mu = mean(h_apical, na.rm = TRUE),
            mu.next = mean(h_apical.next, na.rm = TRUE))

stemdata %>%
  mutate(year = as.character(year)) %>%
  filter(site == "BLD2", year == "2017") %>%
  ggplot(aes(x = h_apical, y = h_apical.next)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  geom_hline(data = mean_data,
             mapping = aes(yintercept = mu.next),
             color = "red") +
  geom_vline(data = mean_data,
             mapping = aes(xintercept = mu),
             color = "red") +
  coord_cartesian(ylim = c(0, 200))

longerstemdata <- read_csv("https://qubeshub.org/community/groups/introbiostats/File:/uploads/stemdata.csv") %>%
  mutate(year = as.character(year)) %>%
  filter(site == "SKY", !is.na(h_apical), !is.na(h_apical.next)) %>%
  select(plantID, h_apical, h_apical.next) %>%
  mutate(slope = h_apical.next - h_apical) %>%
  tidyr::pivot_longer(cols = starts_with("h_apical"), names_to = "month", values_to = "height") %>%
  mutate(month = forcats::fct_recode(month, June = "h_apical", September = "h_apical.next"))
