-- !Ups

CREATE EXTENSION IF NOT EXISTS "uuid-ossp";

CREATE TABLE users (
  id uuid primary key,
  name text not null
);

-- !Downs

DROP TABLE users CASCADE;
