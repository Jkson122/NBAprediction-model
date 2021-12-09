> data<-read.table(file="clipboard",sep="\t",header=TRUE)
> winner<-function(team1,team2){
+ attach(data)
+ model<-lm(MOV~TS.+TOV.+ORB.+TS..1+TOV..1+ORB..1)
+ wins=fitted.values(model)
+ comparisontable=cbind(Tm,wins)
+ teamnames=c("Atlanta Hawks","Boston Celtics","Brooklyn Nets","Charlotte Hornets","Chicago Bulls","Cleveland Cavaliers","Dallas Mavericks",
+ "Denver Nuggets","Detroit Pistons","Golden State Warriors","Houston Rockets","Indiana Pacers","Los Angeles Clippers","Los Angeles Lakers",
+ "Memphis Grizzlies","Miami Heat","Milwaukee Bucks","Minnesota Timberwolves","New Orleans Pelicans","New York Knicks","Oklahoma City Thunder",
+ "Orlando Magic","Philadelphia 76ers","Phoenix Suns","Portland Trail Blazers","Sacramento Kings","San Antonio Spurs","Toronto Raptors","Utah Jazz",
+ "Washington Wizards")
+ looktable1=cbind(teamnames, Tm)
+ for (i in 1:length(Tm)) if (team1==looktable1[i,1]) teamone=looktable1[i,2]
+ for (i in 1:length(Tm)) if (team2==looktable1[i,1]) teamtwo=looktable1[i,2]
+ if (teamone>teamtwo) print(paste(team1, "win!")) else if (teamtwo>teamone) print(paste(team2, "win!") ) else print("It is a tie!")
+ detach(data)}
