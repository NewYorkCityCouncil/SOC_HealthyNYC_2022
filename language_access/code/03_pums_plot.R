### Plot

# Load data to start
# Have to add census api first
source("language_access/code/01_pums_pull.R")


### plot by PUMA ------------

plot_df <- top_langs %>%
  filter(LANP_label != "English", LANP_label != "Spanish") %>% 
  top_n(20, n)

p <- plot_df %>%
  ggplot(aes(x=reorder(LANP_label, n), y=n)) +
  geom_bar(stat="identity", width = .6, fill = "#2F56A6") + 
  labs(
    x = "",
    y = "Number of Residents Who Speak Language At Home (Thousands)",
    title = "Language Spoken at Home in NYC",
    subtitle = "A language other than English is spoken by ~50% of residents at home. 24% speak Spanish and the other top 20 are below.",
    caption = "Source: PUMS (ACS 5-Year 2019)"
    ) +
  geom_text(show.legend = F,
            label= paste0(round(plot_df$prop_lang, 4)*100, "%"),
            nudge_x = 0, nudge_y = 25000
  ) +
  scale_y_continuous(
    breaks = c(0, 100000, 200000, 300000, 400000, 500000),
    label = c("0", "100", "200", "300", "400", "500")
  ) +
  theme(legend.position="none", 
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        #panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "#E6E6E6"),
        panel.background = element_blank(),
        axis.line = element_line(colour = "#666666"),
        axis.title.x = element_text(margin = 
                                      margin(t = 10, r = 0, b = 0, l = 0)),
#        text = element_text(family = "Open Sans"),
        axis.text.y = element_text(size = 14, 
                                   margin = margin(t = 0, r = 10, b = 0, l = 0)),

        axis.text.x = element_text(size = 12, 
                                   margin = margin(t = 10, r = 0, b = 0, l = 0)),
        plot.subtitle=element_text(size=12),
        plot.title = element_text(family = "Georgia",size = 16)) +
  coord_flip()

p

ggsave(p, filename = "language_access/visual/group_comparison.png", 
       units = c("in"), width= 14, height= 8)

