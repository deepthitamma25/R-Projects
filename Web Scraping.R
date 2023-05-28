R Script for Web Scraping HTML extraction
install.packages("rvest")
library(rvest)
data1 <- read_html("websiteurl")
data2<- (data1%>%
html_nodes("p") %>%
html_text())
data2
article2 <- data.frame(article2)
write.csv(data3,"test.csv")
Write csv
install.packages("pdftools")
library(pdftools)
PDF <- pdf_text("PDF address %>%
readr::read_lines() #open the PDF inside your project folder