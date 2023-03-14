#NBA WEBSCRAPING
## Playerprofiles
lapply(c("rvest","tidyverse"), library, character.only=T) 
espnlink <- "https://www.espn.com/nba/teams" 
espnpage <- read_html(espnlink) #læser espnsite 
teamlink <- espnpage %>% html_elements(".n9:nth-child(3) .AnchorLink") %>% html_attr("href") #finder unikke holdsider 

players <- data.frame()

for (team in teamlink) {
  
  link <- paste0("https://www.espn.com", team)
  page <- read_html(link)

  name <- page %>% html_elements(".Table__TD+ .Table__TD .AnchorLink") %>% html_text()
  position <- page %>% html_elements(".Table__TD:nth-child(3)") %>% html_text()
  age <- page %>% html_elements(".Table__TD:nth-child(4) .inline") %>% html_text()
  height <- page %>% html_elements(".Table__TD:nth-child(5) .inline") %>% html_text()
  weight <- page %>% html_elements(".Table__TD:nth-child(6) .inline") %>% html_text()
  salary <- page %>% html_elements(".Table__TD:nth-child(8) .inline") %>% html_text()
  team <- page %>% html_elements("#fittPageContainer .db") %>% html_text()
  team <- paste(team, collapse = " ")
 
  bio_df <- data.frame(name, position, age, height, weight, salary) %>% 
                mutate(height = str_sub(height, 1, -2),
                       salary = str_replace_all(salary, ",", ""), #replacer , med .
                       salary = str_sub(salary, 2), #subsetter fra anden character, så første dollartegn undgås
                       weight = str_replace(weight, " lbs", "")) %>%   #fjerner $ som er første tegn
                separate(height, into = c("feet", "inches"), "'", 
                              convert = T, remove = T) %>% #convert sikrer de nye kolonner er integers, remove fjerner height            
                mutate(height = (12*feet + inches)*2.54) %>% 
                select(-feet, -inches) %>% 
                mutate_at(c("age","weight","salary"), as.numeric)
  bio_df$team <- team
  
  players <- rbind(players, bio_df)
} 
 
write.table(players, "players.csv", row.names = FALSE, sep =",")

#Team box scores pr game 
team <- "https://www.basketball-reference.com/leagues/NBA_2023.html"
team_html <- read_html(team)
team_fun <- function(x){team_html %>% 
                        html_elements(x) %>% 
                          html_text()} 

tbox <- team_fun("#per_game-team .center , #per_game-team tbody .right , #per_game-team tbody .left")
tbox <- data.frame(matrix(tbox, nrow = 31, byrow=T)) #byrow gør at matrix fyldes op vandret, dvs nrow egentlig er antal kolonner
colnames(tbox) <- tbox[1,]
tbox <- tbox[-1,]

obox <- team_fun("#per_game-opponent tbody .right , #per_game-opponent tbody .left , #per_game-opponent .center")
obox <- data.frame(matrix(obox, nrow = 31, byrow=T)) #byrow=T gør at matrix fyldes op vandret, false er default 
colnames(obox) <- obox[1,]
obox <- obox[-1,]

tbox <- tbox %>%  
          rename(FGp="FG%", FTp = "FT%", 
                 P3M = "3P", P3A="3PA", P3p="3P%",
                 P2M = "2P", P2A="2PA", P2p="2P%")

obox <- obox %>%  
          rename(FGp="FG%", FTp = "FT%", 
                 P3M = "3P", P3A="3PA", P3p="3P%",
                 P2M = "2P", P2A="2PA", P2p="2P%")

write.table(tbox, "tbox.csv", row.names = FALSE, sep= ",") #rownames = FALSE , ellers skriver den rownummer med i tabellen 
write.table(obox, "obox.csv", row.names = FALSE, sep=",")


#Player box scores  
espnlink <- "https://www.espn.com/nba/teams"
espnpage <- read_html(espnlink)
links <- espnpage %>% html_elements(".n9:nth-child(1) .AnchorLink") %>% html_attr("href") #virker IKKE med html_attrs, skal være uden s

pbox <- data.frame() #empty df

for (team in links){
  pages <- paste0("https://www.espn.com", team)
  
  pbox_html <- read_html(pages)
  names <- pbox_html %>% 
              html_elements(".ResponsiveWrapper+ .remove_capitalize .Table--fixed-left .Table__TH ,
                             .ResponsiveWrapper+ .remove_capitalize .Table__TD .AnchorLink") %>% html_text()
  stats1 <- pbox_html %>% 
              html_elements(".ResponsiveWrapper+ .remove_capitalize .clr-gray-01 , 
                            .ResponsiveWrapper+ .remove_capitalize .Table__Scroller .Table__TD") %>% html_text()
  stats2 <- pbox_html %>% 
              html_elements(".remove_capitalize+ .remove_capitalize .clr-gray-01 , .remove_capitalize+ 
                             .remove_capitalize .Table__Scroller .Table__TD") %>% html_text
  
  names <- matrix(names)
  stats1 <- head(stats1, -13) %>% #sidste 13obs er totals som skal væk 
            matrix(., ncol = 13, byrow = TRUE)
  stats2 <- head(stats2, -14) %>% 
              matrix(., ncol = 14, byrow = TRUE)
  
  playerstats <- cbind(names, stats1, stats2) 
  colnames(playerstats) <- playerstats[1,]
  playerstats <- playerstats[-1,]
  
  pbox <- rbind(pbox, playerstats) 
}

pbox <- pbox %>% 
          mutate(across(!c(Name), as.numeric)) %>% 
            rename(AST_TO="AST/TO", FGp="FG%", FTp = "FT%", 
                   P3M = "3PM", P3A="3PA", P3p="3P%",
                   P2M = "2PM", P2A="2PA", P2p="2P%",
                   Score_eff = "SC-EFF", Shoot_eff = "SH-EFF")

write.table(pbox, "pbox.csv", row.names = FALSE, sep=",")



