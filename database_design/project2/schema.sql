
drop table teams;
drop table coaches_season;
drop table draft;
drop table players;
drop table player_rs_career;
drop table player_rs;


create table teams (tid varchar(30), location varchar(30), name varchar(30), league varchar(30), primary key (tid, league));

create table coaches_season (cid varchar(30), year int, yr_order int,
firstname varchar(30), lastname varchar(30), season_win int, season_loss int, playoff_win int, playoff_loss int, tid varchar(30), primary key (cid, year, yr_order));

create table draft (draft_year int, draft_round int, selection int, tid varchar(30), firstname varchar(30), lastname varchar(30), ilkid  varchar(30), draft_from varchar(100), league varchar(30));

create table players (ilkid varchar(30), firstname varchar(30), lastname varchar(30), position varchar(1), first_season int, last_season int, h_feet int, h_inches real, weight real, college varchar(100), birthday varchar(30), primary key (ilkid)); 

create table player_rs_career (ilkid varchar(30), firstname varchar(30), lastname varchar(30), league varchar(10), gp int, minutes int, pts int, dreb int, oreb int, reb int, asts int, stl int, blk int, turnover int, pf int, fga int, fgm int, fta int, ftm int, tpa int, tpm int);

create table player_rs (ilkid varchar(30), year int, firstname varchar(30), lastname varchar(30), tid varchar(30), league varchar(10), gp int, minutes int, pts int, dreb int, oreb int, reb int, asts int, stl int, blk int, turnover int, pf int, fga int, fgm int, fta int, ftm int, tpa int, tpm int, primary key(ilkid, year, tid));


-- Put your SQL statement under the following lines:

-- 0. Under psql, load all the above tables with data obtained from the corresponding txt file using the `copy' command like this:

\copy teams from 'teams.txt' with delimiter ','
\copy coaches_season from 'coaches_season.txt' with delimiter ','
\copy players from 'players.txt' with delimiter ','
\copy player_rs from 'player_rs.txt' with delimiter ','
\copy player_rs_career from 'player_rs_career.txt' with delimiter ','
\copy draft from 'draft.txt' with delimiter ','
