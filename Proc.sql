/* ----- TRIGGERS     ----- */
-- Q1 

CREATE OR REPLACE FUNCTION check_add_user()
RETURNS TRIGGER AS $$
DECLARE user_email TEXT;
BEGIN 
	SELECT email INTO user_email
	FROM Users u
	WHERE NEW.email=u.email
	AND (u.email IN (SELECT email FROM Backers)
	OR (u.email IN (SELECT email FROM Creators)));
 
	IF user_email IS NOT NULL THEN
		RETURN NEW;
	ELSE
		RAISE EXCEPTION 'User must be backers or creators or both'; 
	END IF; 
END;
$$ LANGUAGE plpgsql;

CREATE CONSTRAINT TRIGGER check_add_user
AFTER INSERT ON Users
DEFERRABLE INITIALLY DEFERRED
FOR EACH ROW 
	EXECUTE FUNCTION check_add_user(); 


-- Q2
CREATE OR REPLACE FUNCTION check_backs_min_amt()
RETURNS TRIGGER AS $$
DECLARE minimum NUMERIC;
BEGIN
	SELECT min_amt INTO minimum
	FROM Rewards r
	WHERE NEW.name=r.name AND NEW.id=r.id;

	IF NEW.amount < minimum THEN 
		RAISE EXCEPTION 'Amount provided should be greater than or equal to minimum amount for the reward level';
	ELSE
		RETURN NEW;
	END IF;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER check_backs_min_amt
BEFORE INSERT ON Backs
FOR EACH ROW
	EXECUTE FUNCTION check_backs_min_amt(); 

-- Q3
CREATE OR REPLACE FUNCTION check_projects_reward_level()
RETURNS TRIGGER AS $$
DECLARE no_of_reward_levels INT;
BEGIN
	SELECT COUNT(*) INTO no_of_reward_levels
	FROM Rewards r
	WHERE r.id=NEW.id;

	IF no_of_reward_levels=0 THEN
		RAISE EXCEPTION 'Number of reward levels should be at least 1';
	ELSE
		RETURN NEW;
	END IF;

END;
$$ LANGUAGE plpgsql;

CREATE CONSTRAINT TRIGGER check_projects_reward_level
AFTER INSERT ON Projects
DEFERRABLE INITIALLY DEFERRED
FOR EACH ROW
	EXECUTE FUNCTION check_projects_reward_level(); 

-- Q4
CREATE OR REPLACE FUNCTION check_refund_request()
RETURNS TRIGGER AS $$

DECLARE refund_date_diff INT;
DECLARE request_date DATE;
BEGIN
	SELECT DATE_PART('day', b.request::timestamp-p.deadline::timestamp), b.request INTO refund_date_diff, request_date
	FROM Projects p JOIN Backs b ON p.id=b.id
	WHERE p.id=NEW.pid;
  
	IF refund_date_diff <= 90 AND request_date IS NOT NULL THEN  -- approve 
		RETURN (NEW.email, NEW.pid, NEW.eid, NEW.date, TRUE);
	ELSE -- reject
    IF request_date IS NOT NULL THEN 
		RETURN (NEW.email, NEW.pid, NEW.eid, NEW.date, FALSE);
    ELSE 
    RAISE EXCEPTION 'Backers do not have refund request';
 END IF;
END IF;

END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER check_refund_request
BEFORE INSERT ON Refunds
FOR EACH ROW
	EXECUTE FUNCTION check_refund_request();	

-- Q5
CREATE OR REPLACE FUNCTION check_backs_reward_date()
RETURNS TRIGGER AS $$
DECLARE p_deadline DATE; p_created DATE;
BEGIN
	SELECT deadline, created INTO p_deadline, p_created
  	FROM Rewards r JOIN Projects p ON p.id=r.id
  	WHERE NEW.name=r.name AND NEW.id=r.id;

  	IF NEW.backing <= p_deadline AND NEW.backing >= p_created THEN 
    	RETURN NEW;
  	ELSE 
      	RAISE EXCEPTION 'Backers should only back a project via reward level and within the deadline and created date';
  	END IF;

END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER check_backs_reward_date
BEFORE INSERT ON Backs
FOR EACH ROW
	EXECUTE FUNCTION check_backs_reward_date();	 


-- Q6
CREATE OR REPLACE FUNCTION check_backs_refund_request()
RETURNS TRIGGER AS $$
DECLARE pid INT; total_amount_backs INT; funding_goal INT; pdeadline DATE;
BEGIN 
  SELECT SUM(b.amount), p.goal, p.deadline INTO total_amount_backs, funding_goal, pdeadline
  FROM Projects p, Backs b
  WHERE  p.id=b.id AND NEW.id=p.id
  GROUP BY p.id
  HAVING SUM(b.amount) >= p.goal;

  IF total_amount_backs IS NULL OR funding_goal IS NULL THEN
   RAISE EXCEPTION 'No successful projects found';
  ELSIF NEW.request >= pdeadline AND (OLD.request IS NULL) AND (NEW.request IS NOT NULL) THEN
    RETURN NEW;
  ELSIF NEW.request < pdeadline THEN
   RAISE EXCEPTION 'Deadline not passed yet';
  ELSE
    RAISE EXCEPTION 'We can only update NULL to non-NULL values';
  END IF;

END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER check_backs_refund_request
BEFORE UPDATE ON Backs
FOR EACH ROW 
 EXECUTE FUNCTION check_backs_refund_request();


/* ------------------------ */





/* ----- PROECEDURES  ----- */
/* Procedure #1 */
CREATE OR REPLACE PROCEDURE add_user(
  email TEXT, name    TEXT, cc1  TEXT,
  cc2   TEXT, street  TEXT, num  TEXT,
  zip   TEXT, country TEXT, kind TEXT
) AS $$
-- add declaration here
BEGIN
  -- your code here
  INSERT INTO Users VALUES(email, name, cc1, cc2);
  IF kind='BACKER' THEN 
  	INSERT INTO Backers VALUES(email, street, num, zip, country);
  ELSIF kind='CREATOR' THEN
  	INSERT INTO Creators VALUES(email, country);
  ELSIF kind='BOTH' THEN
  	INSERT INTO Backers VALUES(email, street, num, zip, country);
  	INSERT INTO Creators VALUES(email, country);
  ELSE
  	RAISE EXCEPTION 'kind is not a backer, creator or both';
  END IF; 
END;
$$ LANGUAGE plpgsql; 


/* Procedure #2 */
CREATE OR REPLACE PROCEDURE add_project(
  id      INT,     email TEXT,   ptype    TEXT,
  created DATE,    name  TEXT,   deadline DATE,
  goal    NUMERIC, names TEXT[],
  amounts NUMERIC[]
) AS $$
-- add declaration here
DECLARE  
	idx INT;
	n INT;
	r_name TEXT;
	r_amount NUMERIC;
BEGIN
  -- your code here
  n := array_length(names, 1);
  idx := 1; 

  INSERT INTO Projects VALUES(id, email, ptype, created, name, deadline, goal);

  LOOP
  	EXIT WHEN idx=n+1;
  	r_name := names[idx];
  	r_amount := amounts[idx];  
  	IF r_amount IS NULL OR r_amount <= 0 THEN
  		RAISE EXCEPTION 'Invalid r_amount';
  	END IF;
  	INSERT INTO Rewards VALUES(r_name, id, r_amount);
  	idx := idx + 1; 
  END LOOP;

END;
$$ LANGUAGE plpgsql; 


/* Procedure #3 */
CREATE OR REPLACE PROCEDURE auto_reject(
  eid INT, today DATE
) AS $$
-- add declaration here
DECLARE email TEXT; date_diff INT; projectId INT;
BEGIN
  -- your code here
  SELECT b.email, DATE_PART('day', b.request::timestamp-p.deadline::timestamp), p.id INTO email, date_diff, projectId
  FROM Projects p JOIN Backs b ON p.id=b.id;

  IF date_diff > 90 THEN
  	INSERT INTO Refunds VALUES (email, projectId, eid, today, FALSE);
  END IF;
END;
$$ LANGUAGE plpgsql; 

/* ------------------------ */





/* ----- FUNCTIONS    ----- */
/* Function #1  */
CREATE OR REPLACE FUNCTION find_superbackers (today DATE) RETURNS TABLE (
  email TEXT,
  name TEXT
)
AS $BODY$
DECLARE 
curs CURSOR FOR(
  SELECT u.email as email, 
    u.name as name
  FROM Users u, Backers b
  WHERE u.email = b.email
  ORDER BY u.email ASC
  );
r RECORD;
total_funding INT; 
pid_cnt INT; 
ptype_cnt INT;

not_reuested_req BOOLEAN;
reuested_req BOOLEAN;
rejected_req BOOLEAN;
accepted_req BOOLEAN;

BEGIN


  OPEN curs;
    LOOP

      FETCH curs INTO r;
      EXIT WHEN NOT FOUND;

      WITH successful_proj AS (
        SELECT b.id pid, 
          p.ptype ptype, 
          SUM(b.amount) total_amount 
          --per pid
        FROM Backs b, Projects p
        WHERE p.deadline <= today
          --BETWEEN (today - interval '30 day') AND today
          AND p.id = b.id
        GROUP BY b.id, p.ptype
        HAVING SUM(b.amount) >= SUM(p.goal)
      )
      SELECT 
        COUNT(s.pid),
        COUNT(s.ptype),
        SUM(b2.amount)
      INTO
        pid_cnt,
        ptype_cnt,
        total_funding
      FROM Backers b1, Users u, Backs b2, successful_proj s
      WHERE b1.email = u.email 
        AND u.email = b2.email
        AND b2.id = s.pid
        AND u.email = r.email
        AND u.name = r.name
      GROUP BY u.email;

    
      SELECT CASE WHEN COUNT(1) >= 1 THEN false ELSE true END
          INTO reuested_req
      FROM backs ba
      WHERE ba.request is not null
        AND ( ba.request BETWEEN ( today - INTERVAL '30 day' ) AND today )
        AND ba.email = r.email;
      
      SELECT CASE WHEN COUNT(1) >= 1 THEN false ELSE true END
          INTO rejected_req
      FROM backs ba,refunds rf
      WHERE rf.pid = ba.id 
        AND rf.email = ba.email 
        AND ( rf.accepted = FALSE ) 
        AND ( rf.date BETWEEN ( today - INTERVAL '30 day' ) AND today )
        AND rf.email = r.email;

      SELECT CASE WHEN COUNT(1) >= 1 THEN false ELSE true END
        INTO accepted_req
      FROM backs ba,refunds rf
      WHERE rf.pid = ba.id 
        AND rf.email = ba.email 
        AND ( rf.accepted = TRUE ) 
        AND ( rf.date BETWEEN ( today - INTERVAL '30 day') AND today )
        AND rf.email = r.email;

      IF ( ( pid_cnt >= 5 AND ptype_cnt >=3 ) OR ( total_funding >= 1500 AND reuested_req AND rejected_req AND accepted_req ) ) THEN
        name := r.name;
        email := r.email;
        RETURN NEXT;
      END IF;
      
    END LOOP;
  CLOSE curs;

  EXCEPTION WHEN OTHERS THEN 
 RAISE;

END;
$BODY$
LANGUAGE PLPGSQL;




/* Function #2  */
CREATE OR REPLACE FUNCTION find_top_success (n INT, today DATE, ptype TEXT) RETURNS TABLE (
  id INT,
  name TEXT,
  email TEXT,
  amount NUMERIC
)
AS $$ 
      WITH pledged_projects
      AS
      (SELECT
          id,
          COALESCE(SUM(amount), 0) AS pledged_amount
        FROM backs
        GROUP BY id),
      successful_projects
      AS
      (SELECT
          p.id,
          p.ptype,
          p.created,
          p.name,
          p.deadline,
          p.email,
          p.goal,
          COALESCE(pl.pledged_amount, 0) AS pledged_amount
        FROM projects p
          LEFT JOIN pledged_projects pl
            ON p.id = pl.id)
      SELECT
            id,
            name,
            email,
            pledged_amount AS amount
      FROM successful_projects
      WHERE pledged_amount >= goal
          AND deadline <= $2
          AND ptype = $3
      ORDER BY (pledged_amount / goal) DESC, deadline DESC, id ASC LIMIT $1;
$$
LANGUAGE SQL;

/* Function #3  */
CREATE OR REPLACE FUNCTION find_top_popular(
  n INT, today DATE, ptype TEXT
) RETURNS TABLE(id INT, name TEXT, email TEXT,
                days INT) AS $$
                #variable_conflict use_variable

  DECLARE
  curs CURSOR FOR (SELECT p.id,
                    p.name,
                    p.email,
                    b.backing - p.created days,
                    SUM(b.amount) total_fund,
                    p.goal goal
                  FROM Backs b, Projects p
                  WHERE b.id = p.id
                    AND p.ptype = ptype
                    AND p.created <= today
                  GROUP BY p.id, b.backing 
                  ORDER BY days DESC, p.id ASC
                  );      
  r RECORD; 
  total_fund INT;
  counter INT;

BEGIN
  total_fund := 0; 
  counter := 0;
  OPEN curs;

    LOOP
      EXIT WHEN counter >= n;
      FETCH curs INTO r;
      EXIT WHEN NOT FOUND;

      total_fund := total_fund + r.total_fund;

      IF total_fund >= r.goal 
        THEN days := r.days;
          id := r.id;
          name := r.name;
          email := r.email;
          counter := counter + 1;
          RETURN NEXT;
      END IF;

    END LOOP;
  CLOSE curs;
END;
$$ LANGUAGE plpgsql;

/* ------------------------ */

