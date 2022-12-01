#!/usr/bin/env ruby

input = File.readlines('day_01.input')
sums =
  input
    .map(&:chomp)
    .slice_when {|str| str.empty?}
    .map {|group| (group - ['']).map(&:to_i).reduce(:+)}

puts "Part 1: #{sums.max}"
puts "Part 2: #{sums.sort.reverse.take(3).reduce(:+)}"
