library(readxl)
library(dplyr)
files <- list.files(path = "~/GitHub/epl19-20/", pattern = "*.xlsx", full.names = T)
tbl <- sapply(files, read_excel, simplify=FALSE) %>% 
  bind_rows(.id = "id")


tbl2 <- separate(tbl, id, c("A","B"), sep = '/epl19-20')
tbl3 <- separate(tbl2, B, c("A","B"), sep = '.xlsx')
tbl$Team <- gsub("/", "", tbl3$A)
tbl <- separate(tbl, Result, c("Home","Away"), sep = '-')
tbl$Team <- str_to_sentence(tbl$Team)

CapStr <- function(y) {
  c <- strsplit(y, " ")[[1]]
  paste(toupper(substring(c, 1,1)), substring(c, 2),
        sep="", collapse=" ")
}

tbl$Home <- as.numeric(tbl$Home)
tbl$Away <- as.numeric(tbl$Away)

tbl$Home_Team <- ifelse(tbl$`Home Team` == tbl$Team, "Yes", "No")
tbl$Points <- ifelse(tbl$Home_Team == "Yes" & tbl$Home > tbl$Away, 3, ifelse(tbl$Home_Team == "No" & tbl$Home < tbl$Away, 3, 
                                                                             ifelse(tbl$Home_Team == "Yes" & tbl$Home < tbl$Away, 0,
                                                                                    ifelse(tbl$Home_Team == "No" & tbl$Home > tbl$Away, 0,
                                                                                           1))))
tbl$Status <- "No"
tbl$Status[tbl$Home == tbl$Away] <- "Draw"
tbl$Status[tbl$Home_Team == "Yes" & tbl$Home > tbl$Away] <- "Won"
tbl$Status[tbl$Home_Team == "No" & tbl$Home > tbl$Away] <- "Lost"
tbl$Status[tbl$Home_Team == "No" & tbl$Home < tbl$Away] <- "Won"
tbl$Status[tbl$Home_Team == "Yes" & tbl$Home < tbl$Away] <- "Lost"

tbl$Team[tbl$Team == "Astonvilla"] <- "Aston Villa"
tbl$Team[tbl$Team == "Bonemouth"] <- "Bournemouth"
tbl$Team[tbl$Team == "Crystalpalace"] <- "Crystal Palace"
tbl$Team[tbl$Team == "Mancity"] <- "Man City"
tbl$Team[tbl$Team == "Sheffieldutd"] <- "Sheffield Utd"
tbl$Team[tbl$Team == "Manutd"] <- "Man Utd"
tbl$Team[tbl$Team == "Westham"] <- "West Ham"


tabell$Points <- ifelse(tabell$Status == "Won", 3, ifelse(tabell$Status == "Lost", 0 , ifelse(tabell$Status == "Draw", 1,"")))
tabell$Goaldiff <- ifelse(tabell$Home_Team == "Yes", tabell$Home - tabell$Away, tabell$Away-tabell$Home)
tabell$`Round Number` <- as.numeric(tabell$`Round Number`)

standings <- tabell %>%
  group_by(Team, `Round Number`) %>%
  summarise(
    Team = Team,
    Points = Points)

standings$Points_total <- cumsum(standings$Points)
standings$Points_total <- do.call(c, tapply(standings$Points, standings$Team, FUN=cumsum))


plot <- ggplot(standings, aes(y = Points_total, fill = Team))+
  geom_bar()

gganimate::animate(plot, nframes = 200, fps = 5, duration = 30, width = 2000, height = 1200, renderer = gifski_renderer("anim.gif"))
