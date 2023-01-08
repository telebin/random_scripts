require 'nokogiri'
require_relative 'entry'

class EntryMapper
  def initialize(html)
    @html = Nokogiri::HTML html
  end

  def map_rows
    @html.xpath('//*[@id="wyniki"]/tbody/tr').map do |row|
      from = row.xpath('td[2]/span[1]').text
      to = row.xpath('td[2]/span[2]').text
      # date = row.xpath('td[3]').text
      departure = row.xpath('td[4]/p[1]/span[1]/span[3]').text
      arrival = row.xpath('td[4]/p[2]/span[1]/span[3]').text
      time = row.xpath('td[5]').text
      Entry.new(from, departure, to, arrival, time)
    end
  end
end
