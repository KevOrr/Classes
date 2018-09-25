#!/bin/bash

set -e

cat << EOF | mysql -uroot
CREATE DATABASE logs_db
EOF

cat << EOF | mysql -uroot logs_db
CREATE TABLE dhcp (
    ip_decimal INT(10) UNSIGNED NOT NULL,
    mac_string VARCHAR(20) NOT NULL,
    pc_name VARCHAR(20) NOT NULL,
    transaction VARCHAR(12) NOT NULL,
    timestamp TIMESTAMP NOT NULL,
)
INDEX USING BTREE (ip_decimal, timestamp),
INDEX USING HASH (mac_string);
EOF

cat << EOF | mysql -uroot logs_db
CREATE TABLE radacct (
    timestamp timestamp NOT NULL,
    username VARCHAR(64) NOT NULL,
    FramedIPAddress VARCHAR(15) NOT NULL,
    AcctStatusType VARCHAR(15) NOT NULL,
    CallingStationId VARCHAR(50) NOT NULL
)
INDEX USING BTREE (FramedIPAddress, timestamp),
EOF

cat << EOF | mysql -uroot logs_db
CREATE TABLE contactinfo (
    mac_string VARCHAR(20) NOT NULL,
    contact VARCHAR(20) NOT NULL,
)
INDEX USING HASH (mac_string);
EOF
