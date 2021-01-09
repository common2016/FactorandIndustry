
draw_arrow <- function(p, pos){
  p1 <- p + geom_segment(x = pos$x[1],xend = pos$xend[1],y = pos$y[1], yend = pos$yend[1],
                            arrow = arrow(length = unit(0.3,'cm')),
                            size = 1,color = I('grey55')) +
    geom_segment(x = pos$x[2],xend = pos$xend[2],y = pos$y[2], yend = pos$yend[2],
                 arrow = arrow(length = unit(0.3,'cm')),
                 size = 1,color = I('grey55')) +
    geom_segment(x = pos$x[3],xend = pos$xend[3],y = pos$y[3], yend = pos$yend[3],
                 arrow = arrow(length = unit(0.3,'cm')),
                 size = 1,color = I('grey55')) +
    annotate('text',x = pos$xtext,y = pos$ytext,
             label = c('1','2','斜率最大'),size = 6,color = I('grey55')) +
    labs(x = 'CE')
}
