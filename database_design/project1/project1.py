#!/usr/bin/env python

import click

@click.group()
def main():
    pass

@main.command()
@click.argument('id', type=str)
@click.argument('season', type=int)
@click.argument('first_name', type=str)
@click.argument('last_name', type=str)
@click.argument('season_win', type=int)
@click.argument('season_loss', type=int)
@click.argument('playoff_win', type=int)
@click.argument('playoff_loss', type=int)
@click.argument('team', type=str)
def add_coach(id, season, first, last, swin, sloss, pwin, ploss, team):
    pass

@main.command()
@click.argument('id', type=str)
@click.argument('location=', type=str)
@click.argument('name', type=str)
@click.argument('league', type=str)
def add_team(id, loc, name, league):
    pass

@main.command()
@click.argument('file', type=click.File('r'))
def load_coaches(f):
    pass

@main.command()
@click.argument('file', type=click.File('r'))
def load_terams(f):
    pass

@main.command()
def print_coaches():
    pass

@main.command()
def print_teams():
    pass

@main.command()
@click.argument('last_name', type=str)
def coaches_by_name(last_name):
    pass

@main.command()
@click.argument('city', type=str)
def teams_by_city(city):
    pass

@main.command()
@click.argument('season', type=int)
def best_coach(season):
    pass

@main.command()
@click.argument('selectors', type=str, nargs=-1)
def search_coaches(selectors):
    pass

if __name__ == '__main__':
    main()
