CREATE USER 'learninguser'@'localhost' IDENTIFIED BY 'learning';

GRANT ALL PRIVILEGES ON * . * TO 'learninguser'@'localhost';

ALTER USER 'learninguser'@'localhost' IDENTIFIED WITH mysql_native_password BY 'learning';

CREATE SCHEMA learningdb;