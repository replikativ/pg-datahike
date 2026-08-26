-- API-focused replacement for the fixture-loading portions of PostgreSQL's
-- test_setup.sql that require server-side filesystem access or CTAS.
--
-- Run only against a fresh regression database after `bb pg-regress
-- test_setup`. bootstrap-api.sh streams the data after creating these missing
-- CTAS targets. Client-side \copy uses COPY FROM STDIN, which is both
-- supported by pg-datahike and safe for the server process. It also loads the
-- supported columns of test_setup's inheritance fixtures; path-backed road
-- fixtures remain unavailable until that type is implemented.

\set ON_ERROR_STOP on

CREATE TABLE IF NOT EXISTS onek2 (
  unique1 int4,
  unique2 int4,
  two int4,
  four int4,
  ten int4,
  twenty int4,
  hundred int4,
  thousand int4,
  twothousand int4,
  fivethous int4,
  tenthous int4,
  odd int4,
  even int4,
  stringu1 name,
  stringu2 name,
  string4 name
);

CREATE TABLE IF NOT EXISTS tenk2 (
  unique1 int4,
  unique2 int4,
  two int4,
  four int4,
  ten int4,
  twenty int4,
  hundred int4,
  thousand int4,
  twothousand int4,
  fivethous int4,
  tenthous int4,
  odd int4,
  even int4,
  stringu1 name,
  stringu2 name,
  string4 name
);
