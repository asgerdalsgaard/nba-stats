#nba stats
lapply(c("tidyverse","BasketballAnalyzeR","fmsb", "RColorBrewer"), library, character.only = TRUE)

tadd <- Tadd %>% select(-Rank,-Playoff) %>% mutate(Team=case_when(Team == "LA Clippers" ~ "Los Angeles Clippers", 
                                                                  TRUE ~ as.character(Team))) #fra basketballAnalyzer pakke
player <- read_csv("players.csv") %>% rename(Team = team) %>% 
            mutate(Team=case_when(Team == "LA Clippers" ~ "Los Angeles Clippers", 
                                                   TRUE ~ as.character(Team))) %>% 
              left_join(tadd, by = c("Team"="Team"), suffix = c("long", "_abb"))
tbox <- read_csv("tbox.csv") %>% left_join(tadd, by = c("Team"="Team"), suffix = c("long", "_abb"))#pr game
obox <- read_csv("obox.csv") %>% left_join(tadd, by = c("Team"="Team"), suffix = c("long", "_abb"))#pr game
pbox <- read_csv("pbox.csv") %>% left_join(player, by = c("Name"="name"))#pr game


player %>% 
  group_by(team, Conference) %>% 
    summarize(height_gns = mean(height, na.rm=TRUE), 
              height_stddev = sd(height, na.rm=TRUE)) %>% 
    ggplot(aes(x = fct_reorder(team, height_gns), y = height_gns, color = Conference)) + 
    geom_point(size=4) + 
    coord_flip()+
      labs(x="Team",y="Height (cm)") +
    theme_linedraw(base_size=25) + 
      scale_color_discrete(labels=c("East","West"))

player %>% 
  group_by(team, Conference) %>% 
  summarize(mass = mean(weight/height, na.rm=TRUE), 
            mass_stddev = sd(weight/height, na.rm=TRUE)) %>% 
  ggplot(aes(x = fct_reorder(team, mass), y = mass, color = Conference)) + 
  geom_point(size=4) + 
  coord_flip()+
  labs(x="Team",y="mass (kg/cm)") +
  scale_color_discrete(labels=c("East","West")) +
  theme_linedraw(base_size=25)

player %>% 
  group_by(team, Conference) %>% 
  summarize(salary = mean(salary, na.rm=TRUE), 
            salary_stddev = sd(salary, na.rm=TRUE)) %>% 
  ggplot(aes(x = fct_reorder(team, salary), y = salary, color = Conference)) + 
  geom_point(size=4) + 
  coord_flip()+
  labs(x="Team",y="$") +
  theme_linedraw(base_size=25) +
  scale_y_continuous(labels = scales::number_format(accuracy = 1000)) +
  scale_color_discrete(labels=c("East","West"))

pbox %>% 
    dplyr::filter(PTS>25) %>% 
      ggplot(aes(x=MIN, y=PTS/MIN, label=Name))+geom_text() +
        labs(x = "Minutter/kamp", y = "Point/minut") +
          theme_classic(base_size=25)

pbox %>% dplyr::filter(PTS>25) %>% 
  ggplot(aes(x=MIN, y=Shoot_eff, label=Name))+geom_text() +
  labs(x = "Minutter/kamp", y = "Shooting efficiency") +
  theme_classic(base_size=25)

pbox %>% dplyr::filter(AST>6) %>% 
  ggplot(aes(x=AST, y=TO, label=Name))+
  geom_text(check_overlap = TRUE) +
  labs(x = "Assists/kamp", y = "Turnovers/kamp") +
  theme_classic(base_size=25)


### RADAR ###
radarplot <- function(i){ #5 mest spillende spilleres statistikker pr minut standardiserede 
radar <- pbox %>% 
  mutate(Assists = scale(AST/MIN, center = TRUE),
         Points = scale(PTS/MIN, center = TRUE), 
         Turnovers = scale(TO/MIN, center = TRUE), 
         Rebounds = scale(REB/MIN, center = TRUE), 
         P2 = scale(P2M/MIN, center = TRUE), 
         P3 = scale(P3M/MIN, center = TRUE),
         Blocks = scale(BLK/MIN, center = TRUE),
         Steals = scale(STL/MIN, center = T)) %>%  
  filter(Team == i) %>% slice_max(MIN, n = 5) %>% 
  select(Name, Assists, Points, Turnovers, Rebounds, P2, P3, Blocks, Steals) %>% as.data.frame()
rownames(radar) <- radar[,1]
radar <- radar[,-1]

radar <- rbind(apply(radar, 2, max), #fmsb SKAL have max og min i første og andet row. Super dumt
               apply(radar, 2, min),
               radar)

#colors
radarchart(radar, axistype=0,
           plwd=2, plty = 1,#polygon border size,linjetype 
           pcol = brewer.pal(5,"Set1"), #polygon border color
           pfcol = alpha(brewer.pal(5, "Set1"),0.3), #polygon farve 
           cglty = 1, cglwd = 1, #spiderweb line type, line thickness 
           vlcex=1.3, title = i) #var labs size 
legend(x=1.2, y=0.9, bty="n",  #legend position, title, bty er box around legend "n" er no 
       col = alpha(brewer.pal(5, "Set1"),0.6), pch=20, #legend colors (skal være samme som pcol/pfcol), pch er label type 
       legend = rownames(radar[-c(1,2),]), #rownames fra data 
       cex=1, pt.cex=2) #legend size,
}


radarplot("Boston Celtics")
radarplot("Denver Nuggets")
radarplot("Los Angeles Lakers")
radarplot("Milwaukee Bucks")
radarplot("Memphis Grizzlies")
radarplot("Philadelphia 76ers")
radarplot("New Orleans Pelicans")

tbox %>% 
      left_join(tadd, by = c("Team" = "Team")) %>% 
        left_join(obox, by = c("Team"="Team"), suffix = c("_t","_opp")) %>% 
  ggplot(aes(x=PTS_t, y=PTS_opp, label = team, color = Conference)) + 
  geom_text(size=4) +
  labs(x="Points",y="Points opponent") +
  scale_color_discrete(labels=c("East","West")) +
  theme_classic(base_size=20)



#Fourfactors
tbox_ff <- tbox %>% select(Team, P2A, P2M,P3A,P3M,FTA,FTM=FT,OREB=ORB,DREB=DRB,TOV,MIN=MP)
obox_ff <- obox %>% select(Team, P2A, P2M,P3A,P3M,FTA,FTM=FT,OREB=ORB,DREB=DRB,TOV,MIN=MP)
ff <- fourfactors(tbox_ff, obox_ff) %>% left_join(tadd, by = c("Team"="Team"))

ff %>% 
  ggplot(aes(x=ORtg, y=DRtg, label = team, color = Conference)) + 
  geom_text(size=4) +
  labs(x="Offensive rating",y="Defensive rating") +
  scale_color_discrete(labels=c("East","West")) +
  theme_classic(base_size=20)

ff %>% 
  ggplot(aes(x=PACE.Off, y=PACE.Def, label = team, color = Conference)) + 
  geom_text(size=4) +
  labs(x="Offensive pace",y="Defensive pace") +
  scale_color_discrete(labels=c("East","West"))+
  theme_classic(base_size=20)
