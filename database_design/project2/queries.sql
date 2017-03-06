-- change xxx in this line to your NetID

\o proj2-xxx.out



-- Put your SQL statement under the following lines:


--1. Find all the coaches who have coached exactly ONE team. List their first names followed by their last names;
SELECT c1.cid
FROM coaches_season c1
WHERE NOT EXISTS (SELECT null -- try to find a different entry where the same coach coached a different team
                  FROM coaches_season c2
                  WHERE c2.cid = c1.cid AND NOT (c2.tid = c1.tid));

--2. Find all the players who played in a Boston team and a Denver team (this does not have to happen in the same season). List their first names only.
SELECT p.ilkid, d1.tid, d2.tid
FROM players p, draft d1, draft d2
WHERE p.ilkid = d1.ilkid AND p.ilkid = d2.ilkid
      AND EXISTS (SELECT 1
                  FROM teams t
                  WHERE d1.tid = t.tid AND lower(t.location) = 'boston')
      AND EXISTS (SELECT 1
                  FROM teams t
                  WHERE d2.tid = t.tid AND lower(t.location) = 'denver');


--3. Find those who happened to be a coach and a player in the same team in the same season. List their first names, last names, the team where this happened, and the year(s) when this happened.
SELECT p.firstname, p.lastname, t.name as tname, d.draft_year
FROM players p, coaches_season cs, teams t, draft d
WHERE p.ilkid = cs.cid AND p.ilkid = d.ilkid AND cs.tid = t.tid AND t.tid = d.tid;


--4. Find the average height (in centimeters) of each team coached by Phil Jackson in each season. Print the team name, season and the average height value (in centimeters), and sort the results by the average height.
SELECT t.name as tname, AVG(2.54 * (p.h_feet + 12*p.h_inches)) as avg_h_cm
FROM teams t, players p, draft d, coaches_season cs
WHERE t.tid = d.tid AND t.tid = cs.tid AND p.ilkid = d.ilkid
      AND lower(cs.firstname) = 'phil' AND lower(cs.lastname) = 'jackson'
GROUP BY t.tid, t.name
ORDER BY avg_h_cm;


--5. Find the coach(es) (first name and last name) who have coached the largest number of players in year 2004.
SELECT cs.firstname, cs.lastname
FROM coaches_season cs
WHERE cs.cid in (SELECT ranked_coaches.cid
                 FROM (SELECT cs2.cid, rank() OVER (ORDER BY COUNT(d.ilkid))
                       FROM coaches_season cs2, draft d
                       WHERE cs2.tid = d.tid
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
                  WHERE t.tid = cs.tid));


--7. Find those who happened to be a coach and a player in the same season, but in different teams. List their first names, last names, the season and the teams this happened.


--8. Find the players who have scored more points than Michael Jordan did. Print out the first name, last name, and total number of points they scored.


--9. Find the second most successful coach in regular seasons in history. The level of success of a coach is measured as season_win /(season_win + season_loss). Note that you have to count in all seasons a coach attended to calculate this value.


--10. List the top 10 schools that sent the largest number of drafts to NBA. List the name of each school and the number of drafts sent. Order the results by number of drafts (hint: use "order by" to sort the results and 'limit xxx' to limit the number of rows returned);






-- redirecting output to console
\o
