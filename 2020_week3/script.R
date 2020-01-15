# Metadata ----------------------------------------------------------------
# Title: TidyTuesday week 3 - Passwords
# Purpose: 
# Author(s): @pablocal
# Date Created: 2020-01-14
#
# Comments ----------------------------------------------------------------
# 
# 
# 
#
#
# Options and packages ----------------------------------------------------
library(tidyverse)
library(showtext)

font_add_google("IBM Plex Mono", "mo")
showtext_auto()
gfont <- "mo"

# 1. Data -----------------------------------------------------------------
data <- tidytuesdayR::tt_load("2020-01-14")
df <- data$passwords

glimpse(df)
table(df$category)

# 2. Data prep ------------------------------------------------------------

# Explore strength variable; should be between 0 and 10
df %>% 
  select(strength, category) %>% 
  group_by(category) %>% 
  sjmisc::descr()

# Clean labels before plotting
df <- df %>% 
  mutate(category = ifelse(category == "rebellious-rude", "rebel-rude", category),
         category = str_to_sentence(category),
         category = str_replace_all(category, "-", "\n"))

# Create a df with some of best and worst passwords 
best_pass <- df %>% 
  filter(strength == 10) %>% 
  arrange(category, -strength) %>% 
  group_by(category) %>% 
  summarise(stre = max(strength)+.5,
            pass = first(password))

worst_pass <- df %>% 
  filter(strength == 0) %>% 
  arrange(category, -strength) %>% 
  group_by(category) %>% 
  summarise(stre = min(strength)-.5,
            pass = last(password)) 

pass <- bind_rows(best_pass, worst_pass)


# 3. Plot -----------------------------------------------------------------

df %>% 
  filter(strength < 11) %>% 
  group_by(category) %>% 
  mutate(stre_mean = mean(strength)) %>%   
  ungroup() %>% 
  ggplot() +
  geom_violin(aes(reorder(category, -stre_mean), strength), 
              fill = "white", col = "darkgreen", alpha = .8) +
  geom_point(aes(reorder(category, -stre_mean), y = stre_mean), 
             col = "darkgreen", shape = 19, size = 3) +
  geom_text(data = pass, aes(x = category, y = stre, label = pass), 
            col = "orange", family = gfont, size = 3.5) +
  geom_curve(x = 1,  xend = 1.2, y = 8, yend = 9, col = "darkgreen", 
             arrow = arrow(angle = 20, ends = "first", type = "closed", length = unit(0.25, "cm")), 
             curvature = -.35, size = .2) +
  scale_y_continuous(limits = c(-1, 11),
                     breaks = seq(0, 10, 2)) +
  scale_x_discrete(position = "top") +
  annotate(geom = "text", x = 1.5, y = 9, label = "Average\nstrength", family = gfont, color = "darkgreen", 
           size = 2.75, fontface = "bold", lineheight = .8) +
  labs(title = "A typology of 'bad' passwords",
       subtitle = "An analysis of the strength of 500 usual passwords",
       x = "Password types",
       y = expression("- ← Password strength → +" ),
       caption = "@pablocalv for #TidyTuesday · 2020 W3") +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "black", color = "black"),
        panel.background = element_rect(fill = "black", color = "black"),
        panel.grid.major.y = element_line(color = "white"), 
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(family = gfont, color = "green"),
        plot.subtitle = element_text(family = gfont, color = "green", margin = margin(.2, .5, 1.2, .5, "cm")),
        plot.caption = element_text(family = gfont, color = "orange"),
        axis.title = element_text(family = gfont, color = "green"),
        axis.text = element_text(family = gfont, color = "yellow"),
        axis.ticks = element_blank())

ggsave("2020_week3/plot.pdf")
