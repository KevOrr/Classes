#!/usr/bin/env python

import csv
import sqlite3
import os.path
import functools
import itertools

COACH_FMT = '{0:<9} {1:<4} {2:<10} {3:<10} {4:<3} {5:<3} {6:<3} {7:<3} {8:<3}'
COACH_SHORT_FMT = '{0:<10} {1:<10}'
COACH_HEADER = COACH_FMT.format('ID', 'Year', 'First', 'Last', 'sw', 'sl', 'pw', 'pl', 'team')
TEAM_FMT = '{0:<9} {1:<15} {2:<10} {3:<1}'
TEAM_HEADER = TEAM_FMT.format('ID', 'Location', 'Name', 'League')

def create_empty_tables(conn):
    c = conn.cursor()
    c.executescript('''
    CREATE TABLE IF NOT EXISTS coaches(
        id TEXT, season INTEGER, first_name TEXT, last_name TEXT, team TEXT,
        season_win INTEGER, season_loss INTEGER, playoff_win INTEGER, playoff_loss INTEGER);

    CREATE TABLE IF NOT EXISTS teams(
        team_id TEXT, location TEXT, name TEXT, league TEXT);
    ''')
    c.close()
    conn.commit()

_commands = {}
def register_command(func):
    _commands[func.__name__] = func
    return func

def main():
    conn = sqlite3.connect(':memory:')
    create_empty_tables(conn)

    print 'Type exit to exit'

    while True:
        print '>>>',

        line = raw_input().strip()
        if not line:
            continue

        command = line.split()
        if command[0].lower() in ('exit', 'quit'):
            break
        elif command[0] in _commands:
            # Pass conn and rest of args to subcommand
            _commands[command[0]](conn, command[1:])
        else:
            print '%s is not a recognized command' % command[0]

@register_command
def add_coach(conn, args):
    if len(args) != 9:
        print 'add_coach takes 9 arguments'
        return

    c = conn.cursor()
    c.execute('''
        INSERT INTO coaches
        (id, season, first_name, last_name, season_win, season_loss, playoff_win, playoff_loss, team)
        VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)''', args)
    conn.commit()

@register_command
def add_team(conn, args):
    if len(args) != 4:
        print 'add_team takes 4 arguments'
        return

    c = conn.cursor()
    c.execute('''
        INSERT INTO teams
        (team_id, location, name, league)
        VALUES (?, ?, ?, ?)''', args)
    c.close()
    conn.commit()

@register_command
def load_coaches(conn, args):
    if len(args) != 1:
        print 'load_coaches takes 1 argument'
        return
    if not os.path.isfile(args[0]):
        print '%s does not exist or is not a file' % args[0]
        return

    with open(args[0]) as f:
        reader = csv.reader(f)
        next(reader)
        for row in reader:
            stripped = [f.strip() for f in row]
            add_coach(conn, stripped[:2]+stripped[3:])
        conn.commit()

@register_command
def load_teams(conn, args):
    if len(args) != 1:
        print 'load_teams takes 1 argument'
        return
    if not os.path.isfile(args[0]):
        print '%s does not exist or is not a file' % args[0]
        return

    with open(args[0]) as f:
        reader = csv.reader(f)
        next(reader)
        for row in reader:
            add_team(conn, row)
        conn.commit()

@register_command
def print_coaches(conn, args):
    c = conn.cursor()
    c.execute('SELECT COUNT(id) FROM coaches')
    if c.fetchone()[0] == 0:
        print 'No coaches found'
        return

    c.execute('''SELECT id, season, first_name, last_name, season_win, season_loss,
                        playoff_win, playoff_loss, team FROM coaches''')

    print COACH_HEADER
    for row in c:
        print COACH_FMT.format(*row)

    c.close()

@register_command
def print_teams(conn, args):
    c = conn.cursor()
    c.execute('SELECT COUNT(team_id) FROM teams')
    if c.fetchone()[0] == 0:
        print 'No teams found'
        return

    c.execute('''SELECT team_id, location, name, league FROM teams''')

    print TEAM_HEADER
    for row in c:
        print TEAM_FMT.format(*row)

    c.close()

@register_command
def coaches_by_name(conn, args):
    if len(args) != 1:
        print 'coaches_by_name takes 1 argument'
        return

    name = args[0].replace('+', ' ')
    c = conn.cursor()
    c.execute('SELECT COUNT(id) FROM coaches WHERE last_name LIKE ?', (name,))
    if c.fetchone()[0] == 0:
        print 'No coaches found with name ' + repr(name)
        return

    c.execute('''SELECT id, season, first_name, last_name, season_win, season_loss,
                 playoff_win, playoff_loss, team FROM coaches WHERE last_name=?''', (name,))

    print COACH_HEADER
    for row in c:
        print COACH_FMT.format(*row)

@register_command
def teams_by_city(conn, args):
    if len(args) != 1:
        print 'teams_by_city takes 1 argument'
        return

    name = args[0].replace('+', ' ')
    c = conn.cursor()
    c.execute('SELECT COUNT(team_id) FROM teams WHERE location LIKE ?', (name,))
    if c.fetchone()[0] == 0:
        print 'No teams found in ' + repr(name)
        return

    c.execute('''SELECT team_id, location, name, league FROM teams WHERE location LIKE ?''', (name,))

    print TEAM_HEADER
    for row in c:
        print TEAM_FMT.format(*row)

@register_command
def best_coach(conn, args):
    if len(args) != 1:
        print 'best_coach takes 1 argument'
        return

    c = conn.cursor()
    c.execute('''SELECT first_name, last_name FROM coaches
                 WHERE season=?
                 ORDER BY (season_win + playoff_win - season_loss - playoff_loss) DESC''',
              (args[0],))


    coach = c.fetchone()
    if coach:
        print COACH_SHORT_FMT.format(coach[0], coach[1])
    else:
        print 'No coaches found'
        return

@register_command
def search_coaches(conn, selectors):
    condition = ' AND '.join(selectors)
    if condition:
        condition = 'WHERE ' + condition

    c = conn.cursor();

    try:
        c.execute('''SELECT count(id) FROM coaches ''' + condition)
        if c.fetchone()[0] == 0:
            print 'No results'
            return

        c.execute('''SELECT id, season, first_name, last_name, season_win, season_loss,
                     playoff_win, playoff_loss, team FROM coaches ''' + condition)

        print COACH_HEADER
        for coach in c:
            print COACH_FMT.format(*coach)

    except sqlite3.OperationalError:
        pass

if __name__ == '__main__':
    main()
