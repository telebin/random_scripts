require 'time'

class Entry
  class Record
    attr_reader :time, :name

    def initialize(station, time)
      @name = station
      @time = time
    end

    def to_s
      "#{@name} @ #{@time.strftime '%H:%M'}"
    end
  end

  attr_reader :destination, :source, :travel_seconds

  def initialize(start, depart, stop, arriv, time)
    @source = Record.new start, (Time.parse depart)
    @destination = Record.new stop, (Time.parse arriv)
    @travel_seconds = time.split(/\D/).map(&:to_i).reduce {|a, v| a * 60 + v} * 60
  end

  def to_s
    "#{@source} -(#{travel_time})> #{@destination}"
  end

  def travel_time
    "#{@travel_seconds / 60 ** 2}:#{'%02d' % (@travel_seconds % 60 ** 2 / 60)}"
  end
end
