#!/usr/bin/env ruby

def rows(s)
  l, h = [0, 127]
  s.chars.each do |c|
    if c == "F" 
      l, h = [l, (h-l)/2+l]
    else
      l, h = [(h-l)/2+1+l, h]
    end
  end
  return l
end

def cols(s)
  l, h = [0, 7]
  s.chars.each do |c|
    if c == "L" 
      l, h = [l, (h-l)/2+l]
    else
      l, h = [(h-l)/2+1+l, h]
    end
  end
  return l
end

def str_to_n(s, r)
  v = 0
  s.chars.each_with_index do |c, i| 
    v += 2**-(i+1) if c == r
  end
  (v * 2**s.length).to_i
end

all_ns = File.readlines("day5_data.txt").map { |num| 
  row_n = str_to_n(num[0..6], "B")
  col_n = str_to_n(num[7..9], "R")
  8 * row_n + col_n
}

puts "answer: " + all_ns.max.to_s

min_x = all_ns.min

all_ns.sort.each_with_index do |n, i|
  if min_x + i != n 
    puts "My seat ID: " + (n-1).to_s
    break
  end
end
