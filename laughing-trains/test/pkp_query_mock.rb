require_relative '../pkp_query'
require 'time'

class PkpQueryMock < PkpQuery
  attr_reader :times

  def initialize
    @times = []
    @parameters = {}
  end

  def at(time)
    @times << time
    self
  end

  def get
    if times.last < Time.parse('16:45')
      File.open(File.dirname(__FILE__) + '/data1.html') {|f| f.readlines.join}
    else
      File.open(File.dirname(__FILE__) + '/data2.html') {|f| f.readlines.join}
    end
  end
end
