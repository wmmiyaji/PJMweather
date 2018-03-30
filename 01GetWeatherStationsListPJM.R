library("pdftools")
library("tidyverse")

download.file("https://www.pjm.com/-/media/documents/manuals/m19.ashx", "m19.pdf")
pdf_file <- "m19.pdf" 
text <- pdf_text(pdf_file)
#need table that spans page 23, 24, 25 

page23 <- text[23]
page24 <- text[24]
page25 <- text[25]

extract_table <- function(TABLE.PIECE) {
  data.out <- data.frame(eachrow = strsplit(TABLE.PIECE, "\n")) %>% 
    head(25) %>% tail(2) %>% 
    rename(FourElements = !!names(.[1]))  %>% 
    mutate(FourElements = str_squish(FourElements),
           Zone = word(FourElements,1),
           Station = word(FourElements,2),
           Weight = word(FourElements, -1), 
           Airport_Name = str_remove(FourElements, paste0("^",Zone," ")),
           Airport_Name = str_remove(Airport_Name, paste0("^",Station," ")),
           Airport_Name = str_remove(Airport_Name, paste0(" ",Weight,"$"))) %>% 
    select(-FourElements)
}

page23.1 <- data.frame(eachrow = strsplit(page23, "\n")) %>% 
  head(25) %>% tail(2) %>% 
  rename(FourElements = !!names(.[1]))
page24.1 <- data.frame(eachrow = strsplit(page24, "\n"))%>% head(30) %>% tail(26) %>% 
  rename(FourElements = !!names(.[1]))
page25.1 <- data.frame(eachrow = strsplit(page25, "\n")) %>% head(16) %>% tail(12) %>% 
  rename(FourElements = !!names(.[1]))

station_table <- rbind(page23.1, page24.1, page25.1) %>% 
  mutate(FourElements = str_squish(FourElements),
         Zone = word(FourElements,1),
         Station = word(FourElements,2),
         Weight = word(FourElements, -1), 
         Airport_Name = str_remove(FourElements, paste0("^",Zone," ")),
         Airport_Name = str_remove(Airport_Name, paste0("^",Station," ")),
         Airport_Name = str_remove(Airport_Name, paste0(" ",Weight,"$"))) %>% 
  select(-FourElements)

rm(page23.1,page24.1, page25.1, page23, page24, page25, text, extract_table, pdf_file)
write.csv(station_table, file="station_table.csv")



