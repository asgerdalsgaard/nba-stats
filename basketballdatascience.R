#nba
lapply(c("BasketballAnalyzeR", "tidyverse", "ggplot2", "gridExtra", "gglorenz"), library, character.only=T)

tbs <- read.csv("../nba-stats/Data/2012-18_teamBoxScore.csv")
pbs <- read.csv("../nba-stats/Data/2012-18_playerBoxScore.csv")
obs <- read.csv("../nba-stats/Data/2012-18_officialBoxScore.csv")

tbs$gmDate <- as.Date(tbs$gmDate)
year <- as.numeric(format(tbs$gmDate, "%Y"))
month <- as.numeric(format(tbs$gmDate, "%m"))
tbs$season <- ifelse(month<5, paste0(year-1,"-",year),
                              paste0(year, "-",year+1))

#Teamboxscore
Tbox <- tbs %>% 
          group_by(season, teamAbbr) %>% 
          summarise(GP=n(), MIN=sum(round(teamMin/5)), PTS=sum(teamPTS),
                    W=sum(teamRslt=="Win"), L=sum(teamRslt=="Loss"),
                    P2M=sum(team2PM), P2A=sum(team2PA), P2p=P2M/P2A,
                    P3M=sum(team3PM), P3A=sum(team3PA), P3p=P3M/P3A,
                    FTM=sum(teamFTM), FTA=sum(teamFTA), FTp=FTM/FTA,
                    OREB=sum(teamORB), DREB=sum(teamDRB), AST=sum(teamAST),
                    TOV=sum(teamTO), STL=sum(teamSTL), BLK=sum(teamBLK),
                    PF=sum(teamPF), PM=sum(teamPTS-opptPTS)) %>% 
            rename(Season=season, Team=teamAbbr) %>% 
          as.data.frame()

#Opponent boxsxore
Obox <- tbs %>% 
          group_by(season, teamAbbr) %>% 
          summarise(GP=n(), MIN=sum(round(teamMin/5)), PTS=sum(opptPTS),
                    W=sum(opptRslt=="Win"), L=sum(opptRslt=="Loss"),
                    P2M=sum(oppt2PM), P2A=sum(oppt2PA), P2p=P2M/P2A,
                    P3M=sum(oppt3PM), P3A=sum(oppt3PA), P3p=P3M/P3A,
                    FTM=sum(opptFTM), FTA=sum(opptFTA), FTp=FTM/FTA,
                    OREB=sum(opptORB), DREB=sum(opptDRB), AST=sum(opptAST),
                    TOV=sum(opptTO), STL=sum(opptSTL), BLK=sum(opptBLK),
                    PF=sum(teamPF), PM=sum(teamPTS-opptPTS)) %>% 
          rename(Season=season, Team=teamAbbr) %>% 
          as.data.frame()


#PLayerboxscore
pbs$gmDate <- as.Date(pbs$gmDate)
year <- as.numeric(format(pbs$gmDate, "%Y"))
month <- as.numeric(format(pbs$gmDate, "%m"))
pbs$season <- ifelse(month<5, paste0(year-1,"-",year),
                              paste0(year, "-", year+1))

Pbox <- pbs %>% 
          group_by(season, teamAbbr, playDispNm) %>% 
          summarise(GP=n(), MIN=sum(playMin), PTS=sum(playPTS),
                    P2M=sum(play2PM), P2A=sum(play2PA), P2p=100*P2M/P2A,
                    P3M=sum(play3PM), P3A=sum(play3PA), P3p=100*P3M/P3A,
                    FTM=sum(playFTM), FTA=sum(playFTA), FTp=100*FTM/FTA,
                    OREB=sum(playORB), DREB=sum(playDRB), AST=sum(playAST),
                    TOV=sum(playTO), STL=sum(playSTL), BLK=sum(playBLK),
                    PF=sum(playPF)) %>% 
          rename(Season=season, Team=teamAbbr) %>% 
          as.data.frame()


###########################
#Data kan tjekkes og indlæses med: 
data(package="BasketballAnalyzeR") #Data fra pakken er KUN for 2017-2018 

Pbp <- PbPmanipulation(PbP.BDB) #indeholder næsten kun for GSW-spillere 
Tadd <- Tadd %>% 
          mutate(team = case_when(team == "NOP" ~ "NO", 
                                  team == "GSW" ~ "GS",
                                  team == "NYK" ~ "NY",
                                  team == "PHX" ~ "PHO",
                                  team == "SAS" ~ "SA",
                                  TRUE ~ as.character(team)))
Tbox <- Tbox %>% left_join(Tadd, by = c("Team" = "Team"))
Obox <- Obox %>% left_join(Tadd, by = c("Team" = "Team"))
Pbox <- Pbox


#4factors: F1 eFG, F2 TOratio (antal turnovers pr 100boldbesiddelse), F3 REB%, F4 FTrate). Offensive/defensive rating er point scoret/indkasseret pr 100 boldbesiddelse 
#https://www.basketball-reference.com/about/factors.html
#selected teams
Tbox_4 <- subset(Tbox, team %in% c("BOS","CLE","GS","HOU"))
Obox_4 <- subset(Obox, team %in% c("BOS","CLE","GS","HOU"))
p <- fourfactors(Tbox_4, Obox_4)
plot(p)
fourfactors(Tbox, Obox)

#all teams
pp <- fourfactors(Tbox, Obox)
pp %>% left_join(Tadd, by = c("Team" = "Team")) %>% 
              ggplot(aes(x = ORtg, y = DRtg, label = Team, color = Playoff)) + 
                geom_text(size = 5) + scale_y_reverse()
pp %>% left_join(Tadd, by = c("Team" = "Team")) %>% 
           ggplot(aes(x = F1.Off, y = F1.Def, label = Team, color = Playoff))+
              geom_text(size=5) + scale_y_reverse()


#Radial plots - data er KUN sammenligneligt for dem som er med!!! 
#Comparison of players
Pbox.PG <- subset(Pbox, Player %in% c('Russell Westbrook', 'Stephen Curry', 'Chris Paul',
                                      'Kyrie Irving', 'Damian Lillard', 'LeBron James',
                                      'Klay Thompson', 'Kevin Durant', 'Dirk Nowitzki'))
attach(Pbox.PG)
radial <- data.frame(P2M, P3M, FTM, REB=OREB+DREB, AST, STL, BLK)/MIN
detach(Pbox.PG)
radialprofile(data = radial, title = Pbox.PG$Player, std=TRUE) 

#Comparison of player over seasons 
Pbox.Player <- subset(Pbox, Player == 'Russell Westbrook')
attach(Pbox.Player)
radial_player <- data.frame(P2M, P3M, FTM, REB=OREB+DREB, AST, STL, BLK)/MIN
detach(Pbox.Player)
radialprofile(data = radial_player, title = Pbox.Player$Season, std = TRUE)


#Scatter plots
Pbox.sel <- subset(Pbox, MIN>= 700)
attach(Pbox.sel)
x <- data.frame(AST, TOV, PTS, P3M, REB=DREB+OREB)/MIN
detach(Pbox.sel)
scatterplot(x, data.var = c("AST","TOV"))
scatterplot(x, data.var = c("P3M","REB"))

team <- Pbox.sel %>% 
          filter(Team == "GS") %>% 
            group_by(Player) %>% 
              summarise(P2M = sum(P2M), P3M = sum(P3M), MIN = sum(MIN), GP_total = sum(GP), 
                        P2M_minute = P2M/MIN, P3M_minute = P3M/MIN, MIN_game = MIN/GP_total) %>% 
              filter(GP_total > 100)

team %>% ggplot(aes(x=P2M_minute, y = P3M_minute, size = MIN_game, label = Player)) + 
          geom_point() + geom_text(size=3, vjust=2) 

#Variability
Pbox.var <- subset(Pbox, Team == "CLE" & MIN > 500)
var <- variability(data=Pbox.var, data.var = c("P2p", "P3p", "FTp"),
                                  size.var = c("P2A", "P3A", "FTA"), 
                                  weight=TRUE, VC = TRUE)
plot(var, title = "Variability diagram - Cleveland Cavaliers")

#INequality - https://cran.r-project.org/web/packages/gglorenz/readme/README.html 
Pbox.ineq <- subset(Pbox, Team == "Houston Rockets" & GP > 50) #minimum 50 spillede kampe
Pbox.ineq %>% ggplot(aes(x=PTS, fill = "R")) + 
              stat_lorenz(geom = "polygon", alpha = 0.6) + 
              annotate_ineq(Pbox.ineq$PTS) + geom_abline(linetype = "dashed") +
              labs(y = "Percentage of players", x = "Points in percentage")

no.teams <- nrow(Tbox)
ineq <- array(0, no.teams)
for (k in 1:no.teams) {
  Teamk <- Tbox$Team[k]
  Pbox_team <- subset(Pbox, Team == Teamk)
  inequality <- inequality(Pbox_team$PTS, npl = 9)
  ineq[k] <- inequality$Gini 
}
df <- data.frame(ineq, PTS=Tbox$PTS, CONF=Tadd$Conference, Team=Tadd$Team)
df %>% ggplot(aes(x = ineq, y = PTS, color = CONF, label = Team)) + 
        geom_text(position = position_jitter(width=2, height=2)) 


######### 
#SHOT CHART
Pbp_player = subset(Pbp, player == "Kevin Durant")
Pbp_player$xx <- Pbp_player$original_x/10 #x og y er koordinater som skaleres
Pbp_player$yy <- Pbp_player$original_y/10-41.75
shotchart(data=Pbp_player, x = "xx", y = "yy", z = "playlength",
                          num.sect=5, type="sectors", scatter = TRUE)
shotchart(data=Pbp_player, x = "xx", y = "yy", z = "playlength", result = "result",
                          num.sect=5, type="sectors", scatter = FALSE)


#Assistnet
Pbp.GSW <- subset(Pbp, team == "GSW")
netdata <- assistnet(Pbp.GSW)
set.seed(7)
plot(netdata, edge.thr = 20)


#K-means
#shotclusters
shots <- subset(Pbp, !is.na(Pbp$shot_distance) & Pbp$team=="GSW")
shots <- mutate_if(shots, is.factor, droplevels)
attach(shots)
data <- data.frame(PTS=points, DIST=shot_distance, TIMEQ=periodTime, PL=playlength)
detach(shots)
kclu1 <- kclustering(data, algorithm = "MacQueen", 
                     nclumax = 15, iter.max = 5000) #undersøger antal clusters
plot(kclu1)
kclu2 <- kclustering(data, algorithm = "MacQueen", iter.max = 500, k=6)
plot(kclu2)

#hierarchical clustering of players
attach(Pbox)
data <- data.frame(PTS, P3M, REB=OREB+DREB, AST, TOV, STL, BLK, PF)
detach(Pbox)
data <- subset(data, Pbox$MIN>=1500)
ID <- Pbox$Player[Pbox$MIN>=1500]
hclu1 <- hclustering(data) #kun ved at specificere antal klynger kan man få et dendrogram frem
plot(hclu1)

hclu2 <- hclustering(data, labels=ID, k = 9) #hvis ikke k specificeres bliver det til scree-plot. Underligt 
plot(hclu2, profiles = T) #radial plots af clusters
plot(hclu2, profiles = F, rect = T, colored.branches = T) #profiles = T giver hierarchical plot

#Relationships
data <- subset(Pbox, MIN>=500)
attach(data)
x <- data.frame(PTS,P3M, P2M, REB=OREB+DREB, AST)/MIN
detach(data)
scatterplot(x, data.var=1:5,
              lower = list(continuous = "smooth_loess"),
              diag = list(continuous="barDiag"))
