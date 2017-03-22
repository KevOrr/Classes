--  Before you try the code in this ile from the psql client, you need to create your database NBA-xxx and copy data from NBA to it. For example,
--  createdb NBA-tuy
--  pg_dump -t player_rs_career NBA | psql NBA-tuy
--  Note that those should be done under the Linux console. Then you can log into NBA-xxx and try the following scripts.

--  The following line only needs to be executed once before you do anything at all with pgplsql functions
-- CREATE LANGUAGE 'plpgsql';

-- function 1 declaration
CREATE OR REPLACE FUNCTION player_height_rank (firstname VARCHAR, lastname VARCHAR) RETURNS int AS $$
DECLARE
  height_ranks record;
  r record;
  player_height integer;
  rank integer := 0;

BEGIN

  select into player_height (12*p.h_feet + p.h_inches)
         from players p
         where lower(p.firstname) = lower($1) and lower(p.lastname) = lower($2);

  for height_ranks in select distinct (12*p.h_feet + p.h_inches) as height
                      from players p
                      order by height desc
    loop
      rank := rank + 1;
      if player_height = height_ranks.height then
        return rank;
      end if;
    end loop;

  return 0;

END;
$$ LANGUAGE plpgsql;

-- select * from player_height_rank('Yao', 'Ming');


-- function 2 declaration
CREATE OR REPLACE FUNCTION player_weight_var (tid VARCHAR, yr INTEGER) RETURNS FLOAT AS $$
DECLARE

BEGIN

-- https://en.wikipedia.org/wiki/Variance#Definition
-- Var(X) = E[X^2] - E[X]^2
return avg(p.weight ^ 2) - avg(p.weight) ^ 2
       from player_rs prs, players p
       where prs.ilkid = p.ilkid and lower(prs.tid) = lower($1) and prs.year = $2;

END;
$$ LANGUAGE plpgsql;

-- select * from player_weight_var('MIA', 2004);
