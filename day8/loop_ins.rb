#!/usr/bin/env ruby

ins_array = File.readlines("day_8_data.txt")
run_steps = []
value = 0
pos = 0
c_pos = 0

loop do
  if run_steps.include? pos
    # reset 
    puts "Part I answer: " + value.to_s if c_pos == 0
    run_steps = []
    value = 0
    pos = 0 
    c_pos += 1
    next 
  end
  str = ins_array[pos]
  ins, numStr = str.split " "
  if ins == "acc"
    value += numStr.to_i
    run_steps << pos
    pos += 1
  elsif ins == "jmp" && c_pos != pos || ins == "nop" && c_pos == pos
    run_steps << pos
    pos += numStr.to_i
  elsif ins == "nop" && c_pos != pos || ins == "jmp" && c_pos == pos
    run_steps << pos
    pos += 1
  end
  break if pos >= ins_array.length
end

puts "Part II answer: " + value.to_s
