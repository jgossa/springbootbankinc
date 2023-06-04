CREATE DataBase test;
\c test;
CREATE SCHEMA bankinc;

-- bankinc.customer definition

-- Drop table

-- DROP TABLE bankinc.customer;

CREATE TABLE bankinc.customer (
	id int8 NOT NULL,
	"document" varchar(255) NULL,
	document_type int4 NULL,
	first_name varchar(255) NULL,
	last_name varchar(255) NULL,
	CONSTRAINT customer_pkey PRIMARY KEY (id)
);


-- bankinc.card definition

-- Drop table

-- DROP TABLE bankinc.card;

CREATE TABLE bankinc.card (
	id int8 NOT NULL,
	account_balance float4 NULL,
	block bool NULL,
	creation_date timestamp NULL,
	due_date timestamp NULL,
	enroll bool NULL,
	id_producto varchar(255) NULL,
	"number" varchar(255) NULL,
	transaction_type int4 NULL,
	customer_id int8 NULL,
	CONSTRAINT card_pkey PRIMARY KEY (id),
	CONSTRAINT fkep9gakg0tgnl37wylb6qg9dnt FOREIGN KEY (customer_id) REFERENCES bankinc.customer(id)
);


-- bankinc.transaction_history definition

-- Drop table

-- DROP TABLE bankinc.transaction_history;

CREATE TABLE bankinc.transaction_history (
	id int8 NOT NULL,
	account_balance float4 NULL,
	block bool NULL,
	"date" timestamp NULL,
	enroll bool NULL,
	transaction_ammount float4 NULL,
	transactionumber text NULL,
	transaction_type int4 NULL,
	card_id int8 NULL,
	CONSTRAINT transaction_history_pkey PRIMARY KEY (id),
	CONSTRAINT fk8ivhib6o7sa3tpa7bqspiotun FOREIGN KEY (card_id) REFERENCES bankinc.card(id)
);


-- bankinc.card_id_sequence definition

-- DROP SEQUENCE bankinc.card_id_sequence;

CREATE SEQUENCE bankinc.card_id_sequence
	INCREMENT BY 1
	MINVALUE 1
	MAXVALUE 9223372036854775807
	START 1
	CACHE 1
	NO CYCLE;


-- bankinc.customer_id_sequence definition

-- DROP SEQUENCE bankinc.customer_id_sequence;

CREATE SEQUENCE bankinc.customer_id_sequence
	INCREMENT BY 1
	MINVALUE 1
	MAXVALUE 9223372036854775807
	START 1
	CACHE 1
	NO CYCLE;


-- bankinc.transactionhistory_id_sequence definition

-- DROP SEQUENCE bankinc.transactionhistory_id_sequence;

CREATE SEQUENCE bankinc.transactionhistory_id_sequence
	INCREMENT BY 1
	MINVALUE 1
	MAXVALUE 9223372036854775807
	START 1
	CACHE 1
	NO CYCLE;
	
	
	
INSERT INTO bankinc.customer
(id, "document", document_type, first_name, last_name)
VALUES(nextval('bankinc."customer_id_sequence"'), '18513058', 0, 'juan', 'ossa');	
