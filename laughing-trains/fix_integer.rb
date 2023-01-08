class Integer
  def minutes
    self * 60
  end

  def hours
    self * 60.minutes
  end
end

