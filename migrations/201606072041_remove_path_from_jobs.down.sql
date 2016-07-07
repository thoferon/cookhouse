ALTER TABLE jobs ADD COLUMN path varchar CHECK (path <> '');
