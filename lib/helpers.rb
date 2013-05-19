def format_number(number)
  number.to_s.reverse.gsub(/([0-9]{3}(?=([0-9])))/, "\\1#{','}").reverse
end
