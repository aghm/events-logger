BEGIN;

	CREATE TABLE accounts (
		id BIGSERIAL PRIMARY KEY,
		name VARCHAR(128) NOT NULL,
		domain VARCHAR(128) NOT NULL,
		added TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT NOW()
	);

	CREATE TABLE users (
		id BIGSERIAL PRIMARY KEY,
		account_id BIGSERIAL REFERENCES accounts NOT NULL,
		client_id VARCHAR(64) NOT NULL,
		added TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT NOW()
	);

	CREATE UNIQUE INDEX users_client_id_account_ididx_1 ON users(client_id, account_id);

	CREATE TABLE sessions (
		id BIGSERIAL PRIMARY KEY,
		account_id BIGSERIAL REFERENCES accounts NOT NULL,
		client_id VARCHAR(128) NOT NULL,
		user_id BIGINT REFERENCES users,
		added TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT NOW()
	);

	CREATE TABLE events (
		id BIGSERIAL PRIMARY KEY,
		tag VARCHAR(32) NOT NULL,
		namespace VARCHAR(32) NOT NULL,
		data TEXT,
		session_id BIGSERIAL REFERENCES sessions NOT NULL,
		added TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT NOW()
	);

	CREATE INDEX accounts_name_idx_1 ON accounts(name);
	CREATE UNIQUE INDEX accounts_domain_unq_1 ON accounts(domain);

	CREATE INDEX users_client_id_idx_1 ON users(client_id);
	CREATE INDEX users_client_id_account_id_idx_1 ON users(account_id, client_id);

	CREATE INDEX sessions_client_id_idx_1 ON sessions(client_id);
	CREATE INDEX sessions_user_id_account_id_idx_1 ON sessions(account_id, user_id);
	CREATE INDEX sessions_client_id_account_id_idx_1 ON sessions(client_id, account_id);
	CREATE INDEX sessions_client_id_user_id_account_id_idx_1 ON sessions(client_id, user_id, account_id);

	CREATE INDEX events_tag_idx_1 ON events(tag);
	CREATE INDEX events_namespace_idx_1 ON events(namespace);
	CREATE INDEX event_tag_namespace_id_1 ON events(tag, namespace);
	CREATE INDEX events_tag_id_session_id_idx_1 ON events(tag, session_id);

COMMIT;