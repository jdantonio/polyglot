
def x_in_square(arr)
  return [] if arr.empty?
  
  length = arr[0].length
  split = length / 2
  result = []
  
  (0..length-1).each do |index|
    if index < split
      result << arr[index][index]
      result << arr[index][length-index-1]
    else
      result << arr[index][length-index-1]
      result << arr[index][index]
    end
  end

  result
end
