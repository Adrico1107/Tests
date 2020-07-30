library(dplyr)
library(plotly)

alpha <- 0.05
  
x <- rnorm(1000)
xmin <- quantile(x,0.05)
xmax <- quantile(x,0.95)

fit <- density(x)
df0 <- data.frame(fitx=fit$x,fity=fit$y)


df1 <- df0 %>% filter (fitx<=xmin)
df2 <- (df0 %>% filter (fitx>=xmin)) %>% filter (fitx<=xmax)
df3 <- df0 %>% filter (fitx>=xmax)

rangex <- max(df0$fitx)-min(df0$fitx)
res <- hist(x,breaks=seq(from=floor(min(df1$fitx)),to=ceiling(max(df3$fitx)),by=rangex/30))
#maxhist1 <- max(table(cut(df0$fitx,seq(min(df0$fitx),max(df0$fitx),dist(range(df0$fitx))/30)))) 
maxhist2 <- max(res$counts)
#print(c(maxhist1,maxhist2))
maxdens <- max(df2$fity)
coeff <- maxhist2/maxdens


fig <- plot_ly(x=x,type = "histogram",name = "Histogram : Difference \n of means in resamples")
fig <- fig %>% add_lines(x = xmin, y=seq(0,maxhist2/2.3), name = "alpha", line = list(width=5, color = "red"), hoverinfo='x', showlegend = FALSE) %>%
  add_annotations(x = xmin, y =maxhist2/2, xref = "x", yref = "y", text =  paste("alpha (",alpha,")"),
                  xanchor = "right", showarrow = F, textangle=0, font = list(color="red")) %>%
  add_lines(x = xmax, y=seq(0,maxhist2/2.3), name = "1-alpha", line = list(width=5, color = "rgb(51, 204, 0)"), hoverinfo='x', showlegend = FALSE) %>%
  add_annotations(x = xmax, y =maxhist2/2, xref = "x", yref = "y", text = paste("1-alpha (",1-alpha,")"),
                  xanchor = "left", showarrow = F, textangle=0, font = list(color="rgb(51, 204, 0)"))%>%
  add_lines(x = 0, y=seq(0,maxhist2*1.2), name = "Initial Observation", line = list(width=5, color = "rgb(255, 127, 14)",dash='dash'), hoverinfo='x', showlegend = FALSE) %>%
  add_annotations(x = 0, y =maxhist2*1.2, xref = "x", yref = "y", text = "Initial\n Observation",
                  xanchor = "left", showarrow = F, textangle=0, font = list(color="rgb(255, 127, 14)"))



fig <- fig %>% add_trace(x = df1$fitx, y = (df1$fity)*coeff, type = "scatter",hovertemplate = paste("Density :", round(df1$fity,2),
                                                                                                "<br> Difference of means :", round(df1$fitx,2)),
                         mode = "lines", fill = "tozeroy", fillcolor='rgba(255,0,0,0.5)', line = list(color = 'rgb(255,0,0,0.5)', width = 4),name = paste0("Values in the \n",100*alpha,"th percentile")) %>%
  
  add_trace(x = df2$fitx, y = (df2$fity)*coeff, type = "scatter", hovertemplate = paste("Density :", round(df2$fity,2),
                                                                                        "<br> Difference of means :", round(df2$fitx,2)),
            mode = "lines", fill = "tozeroy", fillcolor='rgba(180,180,180,0.5)',line = list(color = 'rgb(180,180,180,0.5)', width = 4),name="Middle values") %>% 
  add_trace(x = df3$fitx, y = (df3$fity)*coeff, type = "scatter", hovertemplate = paste("Density :", round(df3$fity,2),
                                                                                         "<br> Difference of means :", round(df3$fitx,2)), 
            mode = "lines", fill = "tozeroy", fillcolor='rgba(0,255,0,0.5)', line = list(color = 'rgb(51, 204, 0)', width = 4),name = paste0("Values beyond the \n",100*(1-alpha),"th percentile")) 
 
fig <- fig %>%  layout(title="Test plot"
                       ,legend = list(orientation="h", xanchor="center", x=0.5,y=-0.2)
                       ,xaxis=list(title="Differences of means between samples A and B")
                       ,yaxis=list(title="Count"))

fig
