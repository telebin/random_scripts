require 'net/http'
require_relative 'log'

class PkpQuery
  DAYS = %w(Nd Pn Wt Śr Cz Pt So).freeze

  def initialize
    @proxy = URI(ENV['http_proxy'] || ENV['HTTP_PROXY'] || '')
    @uri = URI('http://rozklad-pkp.pl/pl/tp')
    @parameters = {
        queryPageDisplayed: 'yes', # required
        start: 'Wyszukaj połączenie', # required
        REQ0JourneyStopsS0A: 1, # required
        REQ0JourneyStopsZ0A: 1, # required
        REQ0JourneyProduct_prod_section_0_3: 1 # only regios
    }
    log "Initialized #{self.class} (proxy <#{@proxy}>, uri <#{@uri}>)"
  end

  def from(loc_id)
    @parameters[:REQ0JourneyStopsS0G] = loc_id
    self
  end

  def to(loc_id)
    @parameters[:REQ0JourneyStopsZ0G] = loc_id
    self
  end

  def at(time)
    @parameters[:REQ0JourneyDate] = "#{DAYS[time.wday]}. #{time.strftime '%d.%m.%y'}"
    @parameters[:REQ0JourneyTime] = time.strftime '%H:%M'
    self
  end

  def get
    res = Net::HTTP.new(@uri.host, @uri.port, @proxy.host, @proxy.port).start do |http|
      log "Getting <#{@uri.path}?#{URI.encode_www_form @parameters}>"
      http.get "#{@uri.path}?#{URI.encode_www_form @parameters}"
    end
    log "Got result: #{res.inspect}, body length #{res.body.length}"
    res.body
  end
end
