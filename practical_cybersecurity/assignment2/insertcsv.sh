set -e

echo "Inserting Contact info"
mysql -uroot --local_infile=1 logs_db -e "LOAD DATA LOCAL INFILE 'data/contact_info.csv' INTO TABLE contactinfo FIELDS TERMINATED BY ',' LINES TERMINATED BY '\n'";

echo "Inserting RADIUS logs"
mysql -uroot --local_infile=1 logs_db -e "LOAD DATA LOCAL INFILE 'data/radius_logs.csv' INTO TABLE radacct FIELDS TERMINATED BY ',' LINES TERMINATED BY '\n'";

echo "Inserting DHCP logs"
mysql -uroot --local_infile=1 logs_db -e "LOAD DATA LOCAL INFILE 'data/dhcp_logs.csv' INTO TABLE dhcp FIELDS TERMINATED BY ',' LINES TERMINATED BY '\n'";
