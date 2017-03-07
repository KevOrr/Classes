-- change xxx in this line to your NetID

\o proj2-xxx.out



-- Put your SQL statement under the following lines:


--1. Find all the coaches who have coached exactly ONE team. List their first names followed by their last names;
SELECT c1.firstname, c1.lastname
FROM coaches_season c1
WHERE NOT EXISTS (SELECT null -- try to find a different entry where the same coach coached a different team
                  FROM coaches_season c2
                  WHERE lower(c2.cid) = lower(c1.cid) AND NOT (lower(c2.tid) = lower(c1.tid)));

--2. Find all the players who played in a Boston team and a Denver team (this does not have to happen in the same season). List their first names only.
SELECT p.ilkid, d1.tid, d2.tid
FROM players p, draft d1, draft d2
WHERE lower(p.ilkid) = lower(d1.ilkid) AND lower(p.ilkid) = lower(d2.ilkid)
      AND EXISTS (SELECT 1
                  FROM teams t
                  WHERE lower(d1.tid) = lower(t.tid) AND lower(t.location) = 'boston')
      AND EXISTS (SELECT 1
                  FROM teams t
                  WHERE lower(d2.tid) = lower(t.tid) AND lower(t.location) = 'denver');


--3. Find those who happened to be a coach and a player in the same team in the same season. List their first names, last names, the team where this happened, and the year(s) when this happened.
SELECT cs.firstname, cs.lastname, t.name as tname, rs.year
FROM coaches_season cs, teams t, player_rs rs
WHERE lower(cs.cid) = lower(rs.ilkid) AND cs.year = rs.year
      AND lower(rs.tid) = lower(t.tid) AND lower(cs.tid) = lower(t.tid);


--4. Find the average height (in centimeters) of each team coached by Phil Jackson in each season. Print the team name, season and the average height value (in centimeters), and sort the results by the average height.
SELECT t.name as tname, AVG(2.54 * (p.h_feet + 12*p.h_inches)) as avg_h_cm
FROM teams t, players p, draft d, coaches_season cs
WHERE lower(t.tid) = lower(d.tid) AND lower(t.tid) = lower(cs.tid) AND lower(p.ilkid) = lower(d.ilkid)
      AND lower(cs.firstname) = 'phil' AND lower(cs.lastname) = 'jackson'
GROUP BY t.tid, t.name
ORDER BY avg_h_cm;


--5. Find the coach(es) (first name and last name) who have coached the largest number of players in year 2004.
SELECT cs.firstname, cs.lastname
FROM coaches_season cs
WHERE cs.cid in (SELECT ranked_coaches.cid
                 FROM (SELECT cs2.cid, rank() OVER (ORDER BY COUNT(d.ilkid))
                       FROM coaches_season cs2, draft d
                       WHERE lower(cs2.tid) = lower(d.tid)
                       GROUP BY cs2.cid) as ranked_coaches
                 WHERE ranked_coaches.rank = 1);


--6. Find the coaches who coached in ALL leagues. List their first names followed by their last names.
SELECT cs.firstname, cs.lastname
FROM coaches_season cs
WHERE NOT EXISTS (SELECT t.league
                  FROM teams t
                  EXCEPT
                  (SELECT t.league
                  FROM teams t
                  WHERE lower(t.tid) = lower(cs.tid)));


--7. Find those who happened to be a coach and a player in the same season, but in different teams. List their first names, last names, the season and the teams this happened.
SELECT cs.firstname, cs.lastname, coach_t.name as coach_tname, player_t.name as player_tname, rs.year
FROM coaches_season cs, teams coach_t, teams player_t, player_rs rs
WHERE lower(cs.cid) = lower(rs.ilkid) AND cs.year = rs.year
AND lower(cs.tid) = lower(coach_t.tid) AND lower(rs.tid) = lower(player_t.tid);



--8. Find the players who have scored more points than Michael Jordan did. Print out the first name, last name, and total number of points they scored.
WITH mj_pts AS (SELECT MAX(player_rs_career.pts) as pts
                FROM player_rs_career, players p
                WHERE lower(player_rs_career.ilkid) = lower(p.ilkid)
                      AND lower(p.firstname) = 'michael' AND lower(p.lastname) = 'jordan')
SELECT rs_career.firstname, rs_career.lastname, rs_career.pts
FROM player_rs_career rs_career, mj_pts
WHERE rs_career.pts > mj_pts.pts;


--9. Find the second most successful coach in regular seasons in history. The level of success of a coach is measured as season_win /(season_win + season_loss). Note that you have to count in all seasons a coach attended to calculate this value.


--10. List the top 10 schools that sent the largest number of drafts to NBA. List the name of each school and the number of drafts sent. Order the results by number of drafts (hint: use "order by" to sort the results and 'limit xxx' to limit the number of rows returned);


