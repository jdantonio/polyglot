# greed game kata

module Greed

  def self.score(*scores)
    scores = scores.flatten
    raise ArgumentError.new('must roll five dice') unless scores.size == 5

    result = 0
    valid = (1..6).to_a

    tally = scores.inject(Array.new(7, 0)) do |memo, score|
      raise ArgumentError.new("#{score} is not a valid roll") unless valid.include?(score)
      memo[score] += 1
      memo
    end

    tally.each_with_index do |count, score|
      if count >= 3
        if score == 1
          result += 1000
        else
          result += (score * 100)
        end
        count -= 3
      end
      if score == 1
        result += (100 * count)
      elsif score == 5
        result += (50 * count)
      end
    end

    return result
  end
end
