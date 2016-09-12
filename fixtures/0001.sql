BEGIN;
INSERT INTO accounts(name, domain) VALUES ('example', 'http://example.com');
INSERT INTO sessions(account_id, client_id, user_id) VALUES (1, 'jackfruit', null);
INSERT INTO users(account_id, client_id) VALUES (1, 'example_client_id');
UPDATE sessions SET user_id = 1 WHERE account_id = 1 AND client_id = 'jackfruit';
COMMIT;