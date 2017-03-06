-- #1
-- Find the pnames of parts for which there is some supplier.
SELECT P.pname
FROM Parts P, Catalog S
WHERE P.pid = C.pid

-- #2
-- Find the snames of suppliers who supply every part.
SELECT S.sname
FROM Suppliers S
WHERE NOT EXISTS ((SELECT P.pid
                   FROM Parts P
                   EXCEPT
                   (SELECT C.sid
                    FROM Catalog C
                    WHERE S.pid = C.pid)))


-- #3
-- Find the snames of suppliers who supply every red part.
SELECT S.sname
FROM Suppliers S
WHERE NOT EXISTS ((SELECT P.pid
                   FROM Parts P
                   WHERE P.color = 'Red'
                   EXCEPT
                   (SELECT C.sid
                   FROM Catalog C
                   WHERE S.pid = C.pid)))

-- #4
-- Find the pnames of parts supplied by Acme Widget Suppliers and no one else
SELECT P.pid
FROM (SELECT P.pid, P.pname
      FROM Parts P, Catalog C, Suppliers S
      WHERE P.pid = C.pid and C.sid = S.sid and S.sname = 'Acme Widget Suppliers'
      EXCEPT
      (SELECT P.pid, P.pname
      FROM Parts P, Catalog C, Suppliers S
      WHERE P.pid = C.pid and S.sid = S.sid and not (S.sname = 'Acme Widget Suppliers'))

-- #5
-- Find the sids of suppliers who charge more for some part than the average cost of that
-- part (averaged over all the suppliers who supply that part).
SELECT C.sid
FROM Catalog C
WHERE C.cost >= (SELECT AVG(C.cost)
                 FROM Catalog C)

-- #6
-- For each part, find the sname of the supplier who charges the most for that part.
SELECT P.pname, S.sname
FROM Parts P, Catalog C, Suppliers S
WHERE P.pid = C.pid and C.sid = S.sid
      and NOT EXISTS (SELECT C.pid
                      FROM Catalog C2
                      WHERE C2.cost > C.Cost)

-- #7
-- Find the sids of suppliers who supply only red parts.
SELECT C.sid
FROM Parts P, catalog C
WHERE P.pid = C.pid
      and NOT EXISTS (SELECT C2.sid
                      FROM Parts P, Catalog C2
                      WHERE P.pid = C2.pid and C2.pid = C.pid and P.color = 'Red')

-- #8
-- Find the sids of suppliers who supply a red part and a green part.
SELECT C.sid
FROM Parts P, Catalog C
WHERE P.pid = C.pid and P.color = 'Red'
INTERSECT
(SELECT C.sid
 FROM Parts P, Catalog C
 WHERE P.pid = C.pid and P.color = 'Green')

-- #9
-- Find the sids of suppliers who supply a red part or a green part.
SELECT C.sid
FROM Parts P, Catalog C
WHERE P.pid = C.pid and P.color = 'Red'
UNION
(SELECT C.sid
 FROM Parts P, Catalog C
 WHERE P.pid = C.pid and P.color = 'Green')

-- #10
-- For every supplier that only supplies green parts, print the name of the supplier and the
-- total number of parts that she supplies.
SELECT S.sname, COUNT(C.pid)
FROM Catalog C, Suppliers S
WHERE C.sid = S.sid
      and C.sid IN (SELECT C.sid
                    FROM Parts P, Catalog C
                    WHERE P.pid = C.pid and P.color = 'Green'
                    EXCEPT
                    (SELECT C.sid
                    FROM Parts P, Catalog C
                    WHERE P.pid = C.pid and not (P.color = 'Green'))
GROUP BY S.sid

-- #11
-- For every supplier that supplies a green part and a reel part, print the name and price
-- of the most expensive part that she supplies.
SELECT P.pname, max(C.cost)
FROM Parts P, Catalog C
WHERE P.pid = C.pid
      and C.sid in (SELECT C.sid
                    FROM Parts P, Catalog C
                    WHERE P.pid = C.pid and P.color = 'Red'
                    INTERSECT
                    (SELECT C.sid
                     FROM Parts P, Catalog C
                     WHERE P.pid = C.pid and P.color = 'Green')
GROUP BY C.pid
