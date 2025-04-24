# Data-Visualization-
📊 YouTube Stats Dashboard (R + Flexdashboard)
This repository contains an interactive Flexdashboard built with R that visualizes and analyzes statistics from a YouTube stats data. The dashboard helps uncover engagement patterns, keyword performance over time, and video titles.

🔧 Features
📈 Engagement Analysis (Likes, Comments, Views per keyword)

📅 Year-wise Trend Analysis by keyword

🎯 Average Engagement Rate per keyword

📊 Interactive plots using Plotly

📂 Clean, easy-to-read layout via Flexdashboard

📁 Files Included


README.md – This file

📦 Requirements
R (>= 4.0)

Libraries: flexdashboard, dplyr, ggplot2, plotly, lubridate, syuzhet, tidytext (optional for sentiment)

🚀 How to Run
Clone the repository:

bash
Copy
Edit
git clone https://github.com/your-username/youtube-dashboard.git
Open youtube_dashboard.Rmd in RStudio.

Click "Run Document" or "Knit to HTML".



source code ;

---
title: "YouTube Trend Dashboard"
output: 
  flexdashboard::flex_dashboard:
 
    orientation: rows
    vertical_layout: fill
    social: ["twitter", "facebook","menu"]
    
---
```{r setup, include=FALSE}
library(dplyr)
library(tidyr,ggplot2)
library(ggplot2)
library(plotly)
library(knitr)
library(DT)

library(openintro)

library(rpivotTable)
```



```{r,echo=FALSE}
video_stats <- read.delim("videos-stats.csv", sep=",", strip.white=T)

#dim(video_stats)
#str(video_stats)
#summary(video_stats)

#video_stats %>%
  #summarise_all(~ sum(is.na(.)))
video_stats <- video_stats %>%
  drop_na()

video_stats <- video_stats %>%
  mutate(
    LikesPer1k = round(Likes / (Views / 1000), 2),
    CommentsPer1k = round(Comments / (Views / 1000), 2),
    TitleLen = nchar(Title),
    Engagement_Rate = round((Likes + Comments) / Views, 2)
  )

video_stats <- video_stats %>%
  mutate(PubYear = as.factor(substr(Published.At,1,4)),
         Published.At = as.POSIXct(Published.At, format="%Y-%m-%d"))
```

```{r}
mycolors<-c("blue","#FFC125","darkgreen","darkorange")
```


Interactive Data Visualization
==========================================
Row
------------------------------------------
### Total Views

```{r}
total_views <- sum(video_stats$Views)
flexdashboard::valueBox(
  value = format(total_views, big.mark = ","),
  caption = "Total Views",
  icon = "fa-eye",
  color = "blue"
)

```

### Total Likes
```{r}

total_likes <- sum(video_stats$Likes)
flexdashboard::valueBox(
  value = format(total_likes, big.mark = ","),
  caption = "Total Likes",
  icon = "fa-thumbs-up",
  color = "green"
)

```


### Total Comments 
```{r}

total_comments <- sum(video_stats$Comments)
flexdashboard::valueBox(
  value = format(total_comments, big.mark = ","),
  caption = "Total Comments",
  icon = "fa-comments",
  color = "orange"
)

```

Row
---------------------
### Videos Per Year
```{r}
video_stats %>%
  ggplot(aes(x=PubYear)) +
  geom_bar(fill="#765add") +
  theme_minimal() +
  labs(title = "Number of videos by year", x = "Publication Year", y = "Count")
```

### Title Length
```{r}
video_stats %>%
ggplot(aes(x=TitleLen)) +
geom_histogram(fill="#765add", bins=30) +
theme_minimal() +
labs(title = "Distribution of title length", x = "Title Length (char)", y = "Frequency")
```



Row 
---------------------------------------------------------------

### Title Length by Keywords

```{r}
plot2 <- video_stats %>%
# get the average title length per keyword per year
group_by(PubYear, Keyword) %>%
summarize(avg_len = mean(TitleLen)) %>%
# create a ggplot colored by keywords
ggplot(aes(x=PubYear, y=avg_len, color=Keyword))+
geom_line(aes(group=1)) +
geom_point(size=0.5,alpha=0.5) +
ylab("Avg Title Length (char)") +
xlab("Published Year") +
labs(title="Avg Title Length by Category Overtime (by 1k)")+
theme_minimal()
# convert it into a plotly graph
ggplotly(plot2)

```

Correlation
===========================================
column
----------
### scatter plot
```{r}
video_stats %>%
  # Specify variables we want to include
  plot_ly(x=~LikesPer1k, y=~CommentsPer1k, color=~Keyword, type="scatter", mode="markers",
          size=~Views, sizes=c(5,70),
          # Add markers for each point and specify information to display on hover
          marker=list(sizemode="diameter", opacity=0.5), hoverinfo="text",
          			  # Customize hover text
                      text=~paste(
                        paste0("Likes per 1k views: ", LikesPer1k),
                        paste0("Comments per 1k views: ", CommentsPer1k),
                        paste0("Views (100k): ", round(Views/100000, 2)),
                        paste0("Keyword (Category): ", Keyword),
                      sep="<br>")) %>%
  # Label the axes
  layout(title = 'Likes VS Comments per 1k Views',
         xaxis = list(title = 'Likes per 1k'),
         yaxis = list(title = 'Comments per 1k'),
         legend = list(title=list(text='<b> Keyword </b>')))


```


Data Table
===============

```{r}
datatable(video_stats,
          caption = "YouTube=stats",
          rownames=T,
          filter = "top",
          options=list(pageLength=25))

```


Pivot table 
====================

```{r}

rpivotTable(
  data = video_stats,
  rows = "Keyword",
  cols = "PubYear",
  vals = "Views",
  aggregatorName = "Sum",
  rendererName = "Table Barchart",
  width = "100%",
  height = "500px"
)
```
Summary Report
=====================================
```{r}
total_videos <- nrow(video_stats)
total_views <- round(sum(video_stats$Views, na.rm = TRUE) / 1e6, 1)
avg_engagement <- round(mean(video_stats$Engagement_Rate, na.rm = TRUE), 3)
top_year <- names(sort(tapply(video_stats$Views, video_stats$PubYear, sum), decreasing = TRUE)[1])
top_keyword <- video_stats %>% group_by(Keyword) %>%
  summarise(Views = sum(Views, na.rm = TRUE)) %>%
  arrange(desc(Views)) %>% slice(1) %>% pull(Keyword)

cat(paste0(
  "This dashboard summarizes ", total_videos, " videos, reaching over ", total_views, " million views.\n",
  "The average engagement rate is ", avg_engagement, ".\n",
  "The most active year by views was ", top_year, ".\n",
  "The top-performing keyword is **", top_keyword, "**."
))

```

About Report
=============
Created by : omkar Ojha (Student of IIPS,Mumbai) 


Confidential: HIGHLY !   hahah...

