#!/usr/bin/env python3

import tarfile
import csv
import gzip
import logging
import io
import re
import os
import datetime

import click
from defusedxml import ElementTree
import pytz

import MySQLdb

from config import db_config

log = logging.getLogger(__name__)


def get_db_conn():
    return MySQLdb.connect(**db_config)

def insure_nat_table():
    with get_db_conn() as cur:
        cur.execute(
            '''\
            CREATE TABLE IF NOT EXISTS nat
            (private_ip INT(10), public_ip INT(10), public_port SMALLINT(5))
            INDEX index_nat_public USING HASH (public_ip, public_port)
            ''')

def get_nat_log_csv(natlog, name):
    peek = natlog.peek(2)[:2]
    if peek == b'\x1f\x8b':
        log.debug('%s is gzipped', name)
        natlog = gzip.open(natlog, 'r')
    else:
        log.debug('%s is NOT gzipped', name)

    natlog = io.TextIOWrapper(natlog, encoding='utf-8')

    log.debug('Extracting NAT CSV')
    try:
        r = csv.reader(natlog)
        lines = list(r)
    except (csv.Error, UnicodeDecodeError) as e:
        log.error(e)
        return None

    return lines

def import_nat_log(natlog, name):
    log.info('Importing %s ...', name)
    rows = get_nat_log_csv(natlog, name)
    if rows is None:
        log.warn('Could not open %s as CSV file, ignoring', name)
        return

    new_rows = [(line[0], #timestamp
                 ip_string_to_decimal(line[2]), int(line[3]), #private
                 ip_string_to_decimal(line[6]), int(line[7])) #public
                for line in rows]
    log.debug('Inserting %d rows', len(new_rows))

    with get_db_conn() as cur:
        cur.executemany(
            '''\
            INSERT INTO nat (timestamp, private_ip, private_port, public_ip, public_port)
            VALUES (%s, %s, %s)
            ''',
            new_rows)

def etree_to_dict(t):
    d = {t.tag : list(map(etree_to_dict, t))}
    d.update(('@' + k, v) for k, v in t.attrib.items())
    d['text'] = t.text
    return d

def parse_notice(path):
    with open(path, 'r') as f:
        m = re.search(r'(?:<\?xml.*>\s+)?<infringement.*</infringement>', f.read(), re.IGNORECASE|re.DOTALL)
        if not m:
            raise RuntimeError('Couldn\'t find <infringement> tag in DMCA notice')

    try:
        xml = ElementTree.fromstring(m.group())
    except ElementTree.ParseError as e:
        log.error(e)
        raise RuntimeError('Could not parse DMCA notice XML') from e

    ns = {'acns': 'http://www.acns.net/ACNS'}

    try:
        ts = xml.findall('./acns:Source/acns:TimeStamp', ns)[0].text
        ip = xml.findall('./acns:Source/acns:IP_Address', ns)[0].text
        port = int(xml.findall('./acns:Source/acns:Port', ns)[0].text)
    except (IndexError, ValueError) as e:
        log.error(e)
        raise RuntimeError('Error parsing DMCA notice') from e

    try:
        ts = datetime.datetime.strptime(ts, '%Y-%m-%dT%H:%M:%SZ')
        ts.replace(tzinfo=pytz.utc)
    except ValueError as e:
        raise ValueError('Could not parse timestamp: %s' % ts) from e

    return (ts, ip, port)

def ip_string_to_decimal(ip_string):
    m = re.match(r'(\d{1,3})\.(\d{1,3})\.(\d{1,3})\.(\d{1,3})', ip_string)
    if not m or len(m.groups()) != 4:
        raise RuntimeError('IP address {} could not be parsed'.format(ip_string))

    return sum(int(octet)*256**i for i,octet in enumerate(reversed(m.groups())))

def ip_decimal_to_string(ip_decimal):
    return '.'.join(str(ip_decimal >> i & 0xff) for i in range(0, 32, 8)[::-1])


@click.group()
@click.option('-v', '--verbose', count=True)
def main(verbose):
    if verbose > 0:
        logging.basicConfig(format='[%(levelname)-7s] [%(name)s:%(funcName)s:%(lineno)d] %(message)s')
    else:
        logging.basicConfig(format='%(message)s')

    rootLogger = logging.getLogger()
    if verbose >= 2:
        rootLogger.setLevel(logging.DEBUG)
    elif verbose >= 1:
        rootLogger.setLevel(logging.INFO)
    else:
        rootLogger.setLevel(logging.WARN)

@main.command(name='import')
@click.argument('path',
                # help='Path to tarfile or directory containg NAT logs',
                default='nat_logs/',
                type=click.Path(exists=True, file_okay=True,
                                dir_okay=True, readable=True))
def import_nat_logs(path):
    '''Imports NAT logs from a tarfile or directory'''

    insure_nat_table()

    if os.isdir(path):
        for path, _, files in os.walk(dir):
            for name in files:
                fullpath = os.path.join(path, name)
                if os.path.isfile(fullpath):
                    with open(fullpath, 'rb') as f:
                        import_nat_log(io.BufferedReader(f), name)

    else:
        try:
            tf = tarfile.open(tar, mode='r')
        except tarfile.TarError as e:
            log.error(e)
            raise IOError('Error opening %s as tar file', tar) from e

        with tf:
            for member in tf:
                if member.isfile():
                    import_nat_log(tf.extractfile(member), member.name)


@main.command()
@click.argument('path',
                # help='Path to DMCA notice',
                default='dmca.txt',
                type=click.Path(exists=True, file_okay=True,
                                dir_okay=False, readable=True))
def blame(path):
    '''Finds user(s) who match DMCA notice'''

    ten_min = datetime.timedelta(minutes=10)

    with get_db_conn() as cur:
        ts, ip, port = parse_notice(path)
        ip_decimal = ip_string_to_decimal(ip)

        log.info('Searching for address %s (%d), port %d in NAT logs', ip, ip_decimal, port)

        cur.execute(
            '''\
            SELECT DISTINCT (private_ip) FROM nat
            WHERE public_ip=%s AND public_port=%s AND timestamp BETWEEN %s AND %s
            ''',
            (ip_decimal, port, ts - ten_min, ts + ten_min))
        private_ips = [row[0] for row in cur.fetchall()]
        log.debug('Found %d matches', len(private_ips))

        if not private_ips:
            raise RuntimeError('No private addresses found matching public {}:{}'.format(dest_ip, dest_port))

        cur.execute(
            '''\
            SELECT DISTINCT (username) FROM radacct
            WHERE FramedIPAddress IN ({})'''.format(','.join(['%s'] * len(private_ips))),
            (ip_decimal_to_string(ip) for ip in private_ips))
        rad_users = [row[0] for row in cur.fetchall()]

        cur.execute(
            '''\
            SELECT DISTINCT (contact) from contactinfo
            INNER JOIN dhcp ON dhcp.mac_string = contactinfo.mac_string
            WHERE dhcp.ip_decimal IN ({})'''.format(','.join(['%s'] * len(private_ips))),
            private_ips)
        contacts = [row[0] for row in cur.fetchall()]

    if not rad_users and not contacts:
        print('Didn\'t find any users who match the DMCA notice!')

    for user in rad_users:
        print('Found RADIUS user:', user)
    for user in contacts:
        print('Found user in contctinfo:', user)

if __name__ == '__main__':
    main()
