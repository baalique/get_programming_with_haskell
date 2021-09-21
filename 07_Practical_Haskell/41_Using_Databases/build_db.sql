DROP TABLE IF EXISTS checkedout;
DROP TABLE IF EXISTS tools;
DROP TABLE IF EXISTS users;
CREATE TABLE users (id INTEGER PRIMARY KEY, username TEXT);
CREATE TABLE tools (
    id INTEGER PRIMARY KEY,
    name TEXT,
    description TEXT,
    lastReturned TEXT,
    timesBorrowed INTEGER
);
CREATE TABLE checkedout (user_id INTEGER, tool_id INTEGER);
INSERT INTO users (username)
VALUES ('John Doe');
INSERT INTO tools (name, description, lastReturned, timesBorrowed)
VALUES ('Saw', 'Tool that saws', '2021-01-01', 0);
INSERT INTO tools (name, description, lastReturned, timesBorrowed)
VALUES ('Hammer', 'Tool that nails', '2021-01-03', 0);