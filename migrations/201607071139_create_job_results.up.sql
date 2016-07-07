CREATE TYPE job_phase AS ENUM ('run', 'rollback');

CREATE TABLE job_results (
       id       serial    PRIMARY KEY,
       job_id   integer   NOT NULL REFERENCES jobs (id) ON DELETE CASCADE,
       error    varchar   CHECK (error <> ''),
       phase    job_phase NOT NULL,
       end_time timestamp with time zone NOT NULL DEFAULT now(),
       UNIQUE (job_id, phase)
);
