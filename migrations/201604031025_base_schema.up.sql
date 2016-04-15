CREATE TABLE builds (
       id         serial    PRIMARY KEY,
       project    varchar   NOT NULL CHECK (project  <> ''),
       location   varchar   NOT NULL UNIQUE CHECK (location <> ''),
       start_time timestamp NOT NULL,
       end_time   timestamp CHECK (end_time > start_time),
       failure    varchar   CHECK (failure <> '')
);

CREATE TABLE post_builds (
       id         serial    PRIMARY KEY,
       build_id   integer   NOT NULL REFERENCES builds (id),
       start_time timestamp NOT NULL,
       end_time   timestamp CHECK (end_time > start_time),
       failure    varchar   CHECK (failure <> '')
);

CREATE TABLE rollbacks (
       id            serial    PRIMARY KEY,
       post_build_id integer   NOT NULL REFERENCES post_builds (id)
       start_time    timestamp NOT NULL,
       end_time      timestamp CHECK (end_time > start_time),
       failure       varchar   CHECK (failure <> '')
);

CREATE TYPE job_type   AS ENUM ('build', 'post-build', 'rollback', 'clean');
CREATE TYPE job_status AS ENUM ('unpicked', 'picked', 'aborted', 'done');

CREATE TABLE jobs (
       id              serial     PRIMARY KEY,
       type            job_type   NOT NULL,
       project         varchar    NOT NULL CHECK (project <> ''),
       build_id        integer    REFERENCES builds (id),
       parent_build_id integer    REFERENCES builds (id),
       status          job_status NOT NULL,
       creation_time   timestamp  NOT NULL DEFAULT 'now()',
       CHECK (type = 'post-build' AND parent_build_id IS NOT NULL)
             OR (type <> 'post-build' AND parent_build_id IS NULL),
       CHECK ((type = 'build' OR type = 'post-build')
);

CREATE TABLE job_dependencies (
       id serial     PRIMARY KEY,
       origin_id     integer NOT NULL REFERENCES jobs (id),
       dependency_id integer NOT NULL REFERENCES jobs (id),
       UNIQUE (origin_id, dependency_id)
);

CREATE TABLE project_settings (
       id            varchar PRIMARY KEY,
       blocked       boolean NOT NULL DEFAULT 'f',
       no_post_build boolean NOT NULL DEFAULT 'f'
);
