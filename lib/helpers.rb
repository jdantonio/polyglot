module Helpers

  def format_number(number)
    whole, decimal = number.to_s.split('.')
    whole = whole.to_s.reverse.gsub(/([0-9]{3}(?=([0-9])))/, "\\1#{','}").reverse
    if decimal.nil?
      return whole
    else
      return whole + '.' + decimal
    end
  end

end
