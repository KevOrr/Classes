#!/usr/bin/env python

import csv
import sqlite3
import os.path

import click

def create_empty_tables(conn):
    c = conn.cursor()
    c.executescript('''
    CREATE TABLE IF NOT EXISTS coaches(
        id TEXT PRIMARY KEY, season INTEGER, first_name TEXT, last_name TEXT, team TEXT,
        season_win INTEGER, season_loss INTEGER, playoff_win INTEGER, playoff_loss INTEGER);

    CREATE TABLE IF NOT EXISTS teams(
        team_id TEXT PRIMARY KEY, location TEXT, name TEXT, league TEXT);
    ''')
    c.close()
    conn.commit()

_commands = {}
def register_command(func):
    _commands[func.__name__] = func
    return func

@click.group(invoke_without_command=True)
@click.option('-f', '--db-file', type=click.Path(exists=False), default='nba.db')
def main(db_file):
    conn = sqlite3.connect(db_file)
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
            #_commands[command[0]](conn, command[1:])
            print command
        else:
            print '%s is not a recognized command' % command[0]

@register_command
def add_coach(conn, args):
    if len(args) != 9:
        print 'add_coach takes 9 arguments'
        return

    c = conn.cursor()
    try:
        c.execute('''
            INSERT INTO coaches
            (id, season, first_name, last_name, season_win, season_loss, playoff_win, playoff_loss, team)
            VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)''', args)
    except sqlite3.IntegrityError:
        pass
    finally:
        c.close()
        conn.commit()

@register_command
def add_team(ctx, args):
    if len(args) != 4:
        print 'add_team takes 4 arguments'
        return

    conn = ctx.obj['conn']
    c = conn.cursor()
    try:
        c.execute('''
            INSERT INTO teams
            (team_id, location, name, league)
            VALUES (?, ?, ?, ?)''', args)
    except sqlite3.IntegrityError:
        pass
    finally:
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
        map(add_coach, reader)

@register_command
def load_teams(f):
    if len(args) != 1:
        print 'load_teams takes 1 argument'
        return
    if not os.path.isfile(args[0]):
        print '%s does not exist or is not a file' % args[0]
        return

    with open(args[0]) as f:
        reader = csv.reader(f)
        next(reader)
        map(add_coach, reader)

def print_coaches():
    pass

def print_teams():
    pass

def coaches_by_name(last_name):
    pass

def teams_by_city(city):
    pass

def best_coach(season):
    pass

@main.command()
@click.argument('selectors', type=str, nargs=-1)
def search_coaches(selectors):
    pass

if __name__ == '__main__':
    main(obj={})
