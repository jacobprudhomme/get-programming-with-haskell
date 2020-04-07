DROP TABLE IF EXISTS checkedout;
DROP TABLE IF EXISTS tools;
DROP TABLE IF EXISTS users;

CREATE TABLE users (
  id INTEGER PRIMARY KEY,
  username TEXT
);

CREATE TABLE tools (
  id INTEGER PRIMARY KEY,
  name TEXT,
  description TEXT,
  lastReturned TEXT,
  timesBorrowed INTEGER
);

CREATE TABLE checkedout (
  user_id INTEGER,
  tool_id INTEGER
);

INSERT INTO users (username) VALUES ('jacobprudhomme');

INSERT INTO tools (name, description, lastReturned, timesBorrowed)
VALUES ('hammer', 'bonk', '2020-03-14', 0);

INSERT INTO tools (name, description, lastReturned, timesBorrowed)
VALUES ('saw', 'bzzz', '2020-03-15', 0);
