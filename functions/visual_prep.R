thm = theme_light() +
  theme(legend.key=element_rect(fill=NA),
        legend.position='bottom',
        legend.text=element_text(size=13),
        plot.title = element_text(hjust = 0.5),
        strip.background = element_rect(fill = '#00a188'),
        strip.text.x = element_text(color = "white", size = 13, face = 'bold'), 
        plot.margin = unit(rep(2,4),'mm'), panel.grid.minor.y = element_blank())

vectorBulletList <- function(vector) {
  if(length(vector > 1)) {
    paste0("<ul><li>", 
           paste0(
             paste0(vector, collpase = ""), collapse = "</li><li>"),
           "</li></ul>")   
  }
}
