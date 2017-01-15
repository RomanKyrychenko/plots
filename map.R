ggplot(all_user,aes(crop_photo.photo.long,crop_photo.photo.lat)) +
  borders("world", colour= "#4B4B4B", fill="#4F4F4F") +
  geom_point(color="#fbfcbf", size=0.001) +
  coord_cartesian(xlim=c(-20, 59), ylim=c(30, 71)) +
  theme_void() +
  theme(plot.background = element_rect( colour= "#708090", fill="#708090"),
        plot.title = element_text(size = 30, color = "white", margin = margin(b = 10)),
        plot.subtitle = element_text(size = 24, color = "white", margin = margin(b = 25)),
        plot.caption = element_text(size = 14, margin = margin(t = 10), color = "white", hjust = 0))