ALTER TABLE job_results
      ADD COLUMN start_time timestamp with time zone,
      ALTER COLUMN end_time DROP NOT NULL;

UPDATE job_results SET start_time = end_time;

ALTER TABLE job_results ALTER COLUMN start_time SET NOT NULL;
