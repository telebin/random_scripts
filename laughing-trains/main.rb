#!/bin/ruby
require_relative 'entry.rb'

def log(msg)
  STDERR.puts msg
end

if ARGV.size.zero?
  log 'Needs argument: input file'
  exit 1
end

INPUT_FILE = ARGV[0]
if File::size(INPUT_FILE) > 2 << 25
  log 'Insane input size'
  exit 2
end

log 'Reading file ' + INPUT_FILE
input = File::read INPUT_FILE
m = 'mapa okolic'
loc = '(?:\p{Word}|\s)+?'
opt_date = '(?:(?:\s?\d\d\.?){3})?'
time = '\d\d?:\d\d'
locations = "#{m}\\s+(?<start>#{loc})\\n#{m}\\s+(?<stop>#{loc})#{opt_date}"
times = "(?<dep>#{time})\\s+(?<arr>#{time})\\s+(?<dur>#{time})"
regex = /#{locations}\n[^\d]+#{times}.*$/

log 'Prepared regex: ' + regex.to_s

timetable = input.split("bilet\n").map do |entry|
  if (match = regex.match(entry))
    Entry.new(match['start'], match['dep'], match['stop'], match['arr'], match['dur'])
  else
    log "Could not parse the following:\n#{entry}"
    nil
  end
end.map(&:to_s)

puts timetable
