require_relative 'entry_mapper'
require_relative 'log'

class EntryCache
  def initialize(query)
    @query = query
    @entries = []
  end

  def get_conn_after(time)
    closest_entry = find_entry_closest_to(time)
    return closest_entry if closest_entry

    fill_cache(time + 1.minutes)
    find_entry_closest_to(time)
  end

  private
  def fill_cache(time)
    log 'Filling cache with query for time ' + time.to_s
    @entries += EntryMapper.new(@query.at(time).get).map_rows
  end

  def find_entry_closest_to(time)
    log 'Searching for closest entry in cache'
    entry = @entries.select {|e| e.source.time > time}.min_by {|e| e.source.time}
    log "Found #{entry}"
    entry
  end
end
