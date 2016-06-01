CREATE TYPE job_type AS ENUM ('build', 'post-build');
CREATE TYPE job_status
       AS ENUM ('in-queue', 'in-progress', 'success', 'failure', 'rollbacked');

CREATE TABLE jobs (
       id                 serial     PRIMARY KEY,
       type               job_type   NOT NULL,
       status             job_status NOT NULL,
       project_identifier varchar    NOT NULL CHECK (project_identifier <> ''),
       dependencies       integer[]  NOT NULL,
       path               varchar    CHECK (path <> ''),
       creation_time      timestamp with time zone NOT NULL DEFAULT now()
);
