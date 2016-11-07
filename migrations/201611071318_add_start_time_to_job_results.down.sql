UPDATE job_results SET end_time = now() WHERE end_time IS NULL;

ALTER TABLE job_results
      ALTER COLUMN end_time SET NOT NULL,
      DROP COLUMN start_time;
