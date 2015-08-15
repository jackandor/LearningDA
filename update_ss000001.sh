#!/bin/sh

rm -f ss000001.csv
curl -o ss000001.csv http://real-chart.finance.yahoo.com/table.csv?s=000001.SS&d=7&e=15&f=2015&g=d&a=11&b=19&c=1990&ignore=.csv
