#!/usr/bin/env ruby

v1 = 0
v2 = 0

def check_body?(head, body)
  if head == "byr"
    is_v_byr?(body.to_i)
  elsif head == "iyr"
    is_v_iyr?(body.to_i)
  elsif head == "eyr"
    is_v_eyr?(body.to_i)
  elsif head == "hgt"
    is_v_hgt?(body)
  elsif head == "hcl"
    is_v_hcl?(body)
  elsif head == "ecl"
    is_v_ecl?(body)
  elsif head == "pid"
    is_v_pid?(body)
  elsif head == "cid"
    is_v_cid?(body)
  end
end

def is_v_cid?(y)
  true
end

def is_v_byr?(y)
  y >= 1920 && y <= 2002
end

def is_v_iyr?(y)
  y >= 2010 && y <= 2020
end

def is_v_eyr?(y)
  y >= 2020 && y <= 2030
end

def is_v_hgt?(hs)
  if hs.end_with?("cm") 
    h = hs.to_i
    h >= 150 && h <= 193
  elsif hs.end_with?("in")
    h = hs.to_i
    h >= 59 && h <= 76
  else
    false
  end
end

def is_v_hcl?(cl)
  cl.match(/^#[0-9a-f]{6}$/) != nil
end

def is_v_ecl?(cl)
  ["amb", "blu","brn","gry","grn","hzl","oth"].include? cl
end

def is_v_pid?(ps)
  ps.match(/^[0-9]{9}$/) != nil
end

File.readlines('day_4_data.txt').each do |line|
    is_v1 = line.include?("byr:") &&
           line.include?("iyr:") &&
           line.include?("eyr:") &&
           line.include?("hgt:") &&
           line.include?("hcl:") &&
           line.include?("ecl:") &&
           line.include?("pid:")
    if is_v1 
      v1 += 1
      is_v2 = true
      line.split(" ").each do |item|
        head,body = item.split(":")
        if not check_body?(head, body)
          is_v2 = false
          break
        end
      end

      if is_v2 
        v2 += 1
        puts line
      end
    end
end

puts v1
puts v2

