CREATE OR REPLACE DATABASE amelinium
 CHARACTER SET = 'utf8mb4'
 COLLATE = 'utf8mb4_general_ci'
 COMMENT = 'Amelinium Database';
--;;
USE amelinium;
--;;
CREATE TABLE IF NOT EXISTS password_suites (
  id    INTEGER UNSIGNED NOT NULL AUTO_INCREMENT PRIMARY KEY,
  suite JSON NOT NULL UNIQUE KEY
);
--;;
CREATE TABLE IF NOT EXISTS users (
  id                 INTEGER UNSIGNED NOT NULL AUTO_INCREMENT PRIMARY KEY,
  uid                UUID NOT NULL UNIQUE KEY,
  email              CHAR(128) NOT NULL UNIQUE KEY,
  account_type       ENUM('system', 'manager', 'user') NOT NULL DEFAULT 'user',
  first_name         VARCHAR(64) NOT NULL,
  last_name          VARCHAR(64) NOT NULL,
  middle_name        VARCHAR(64),
  phone              VARCHAR(64),
  password_suite_id  INTEGER UNSIGNED,
  password           JSON,
  login_attempts     SMALLINT,
  last_ok_ip         INET6,
  last_failed_ip     INET6,
  last_attempt       TIMESTAMP NULL,
  last_login         TIMESTAMP NULL,
  created            DATETIME DEFAULT NOW(),
  created_by         INTEGER UNSIGNED,
  soft_locked        TIMESTAMP NULL,
  locked             TIMESTAMP NULL,
  CONSTRAINT FK_password_suites_id_password_suite
    FOREIGN KEY(password_suite_id)
    REFERENCES password_suites(id)
    ON UPDATE CASCADE
    ON DELETE RESTRICT,
  CONSTRAINT FK_created_by_id_users
    FOREIGN KEY(created_by)
    REFERENCES users(id)
    ON UPDATE CASCADE
    ON DELETE SET NULL
);
--;;
CREATE INDEX IF NOT EXISTS account_type_index ON users(account_type);
--;;
CREATE TABLE IF NOT EXISTS roles (
  user_id    INTEGER  UNSIGNED NOT NULL,
  client_id  INTEGER  UNSIGNED NOT NULL,
  role       CHAR(32) NOT NULL,
  PRIMARY KEY(user_id, client_id, role),
  CONSTRAINT FK_roles_user_id_users
    FOREIGN KEY(user_id)
    REFERENCES users(id)
    ON DELETE CASCADE
    ON UPDATE CASCADE
) ENGINE=Aria TRANSACTIONAL=1;
--;;
CREATE INDEX IF NOT EXISTS role_index ON roles(role);
--;;
CREATE TABLE IF NOT EXISTS authlog (
  user_id      INTEGER UNSIGNED,
  client_id    INTEGER UNSIGNED,
  operation    VARCHAR(128) NOT NULL,
  success      BOOLEAN NOT NULL DEFAULT TRUE,
  level        ENUM('debug', 'info', 'notice', 'warning', 'error', 'critical', 'alert', 'emergency') NOT NULL DEFAULT 'info',
  executed     TIMESTAMP(6) PRIMARY KEY DEFAULT CURRENT_TIMESTAMP,
  message      TEXT(4096)
) ENGINE=InnoDB PAGE_COMPRESSED=1 ROW_FORMAT=DYNAMIC;
--;;
CREATE INDEX IF NOT EXISTS authlog_operation_index ON authlog(operation);
--;;
CREATE INDEX IF NOT EXISTS authlog_level_index ON authlog(level);
--;;
CREATE INDEX IF NOT EXISTS authlog_success_index ON authlog(success);
--;;
CREATE INDEX IF NOT EXISTS authlog_client_index ON authlog(client_id);
--;;
CREATE INDEX IF NOT EXISTS authlog_user_id_index ON authlog(user_id);
--;;
CREATE TABLE IF NOT EXISTS sessions (
  id           CHAR(64) PRIMARY KEY,
  user_id      INTEGER UNSIGNED NOT NULL,
  user_email   CHAR(128),
  created      TIMESTAMP(6) NOT NULL DEFAULT CURRENT_TIMESTAMP,
  active       TIMESTAMP(6) NOT NULL DEFAULT CURRENT_TIMESTAMP,
  ip           INET6,
  secure_token CHAR(128),
  CONSTRAINT FK_sessions_user_id_users
    FOREIGN KEY(user_id)
    REFERENCES users(id)
    ON DELETE CASCADE,
  CONSTRAINT FK_sessions_user_email_users
    FOREIGN KEY(user_email)
    REFERENCES users(email)
    ON DELETE CASCADE
    ON UPDATE CASCADE
) ENGINE=Aria TRANSACTIONAL=0 ROW_FORMAT=FIXED;
--;;
CREATE TABLE IF NOT EXISTS user_settings (
  id          CHAR(32) PRIMARY KEY,
  user_id     INTEGER UNSIGNED NOT NULL,
  value       VARBINARY(32768),
  CONSTRAINT FK_user_settings_user_id_users
    FOREIGN KEY(user_id)
    REFERENCES users(id)
    ON DELETE CASCADE
    ON UPDATE CASCADE
);
--;;
CREATE INDEX IF NOT EXISTS user_settings_index ON user_settings(user_id);
--;;
CREATE TABLE IF NOT EXISTS session_variables (
  id          CHAR(128) NOT NULL,
  session_id  CHAR(128) NOT NULL,
  value       BLOB,
  CONSTRAINT FK_session_id_sessions
    FOREIGN KEY(session_id)
    REFERENCES sessions(id)
    ON DELETE CASCADE
    ON UPDATE CASCADE,
  PRIMARY KEY(session_id, id)
) ENGINE=Aria TRANSACTIONAL=0 ROW_FORMAT=FIXED;
--;;
CREATE INDEX IF NOT EXISTS session_variables_index ON session_variables(session_id);
--;;
CREATE TABLE IF NOT EXISTS confirmations (
  id          CHAR(128) NOT NULL,
  user_id     INTEGER UNSIGNED NULL,
  code        CHAR(16) NULL,
  token       CHAR(128) NULL,
  reason      ENUM('creation', 'recovery', 'unlock') NOT NULL,
  attempts    SMALLINT UNSIGNED NOT NULL DEFAULT 0,
  created     TIMESTAMP(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(),
  expires     TIMESTAMP(6) NULL,
  confirmed   BOOLEAN NOT NULL DEFAULT FALSE,
  req_id      CHAR(128) NULL,
  first_name  VARCHAR(64),
  middle_name VARCHAR(64),
  last_name   VARCHAR(64),
  password    JSON,
  pwd_suite   INTEGER UNSIGNED,
  PRIMARY KEY(id, reason)
) ENGINE=Aria TRANSACTIONAL=0 ROW_FORMAT=FIXED;
--;;
CREATE UNIQUE INDEX IF NOT EXISTS confirmations_token_index   ON confirmations(token);
CREATE UNIQUE INDEX IF NOT EXISTS confirmations_code_id_index ON confirmations(code,id);
CREATE UNIQUE INDEX IF NOT EXISTS confirmations_dates_index   ON confirmations(created,expires);
--;;
CREATE OR REPLACE EVENT confirmations_cleanup
  ON SCHEDULE EVERY 1 HOUR
              STARTS CURRENT_TIMESTAMP + INTERVAL 1 MINUTE
  ON COMPLETION PRESERVE
  DO
    DELETE LOW_PRIORITY IGNORE FROM mana.confirmations WHERE expires < (CURRENT_TIMESTAMP - INTERVAL 2 HOUR);
--;;
