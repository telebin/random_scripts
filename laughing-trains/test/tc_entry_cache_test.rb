require_relative 'pkp_query_mock'
require_relative '../entry_cache'
require 'time'
require 'test/unit'

class TC_EntryCacheTest < Test::Unit::TestCase

  def setup
    @query = PkpQueryMock.new
    @cache = EntryCache.new @query
  end

  def test_get_soonest_connection
    entry = @cache.get_conn_after Time.parse('16:20')

    assert(entry.source.time.strftime('%H:%M') == '16:22',
           "Time is not correct (is #{entry.source.time.strftime('%H:%M')}, should be 16:22")
  end

  def test_fire_query_for_first_get
    @cache.get_conn_after Time.parse('16:30')

    assert(@query.times.count == 1)
  end

  def test_fire_query_once_for_close_gets
    entry1 = @cache.get_conn_after Time.parse('16:20')
    entry2 = @cache.get_conn_after Time.parse('16:30')

    assert_equal('16:22', entry1.source.time.strftime('%H:%M'))
    assert_equal('16:35', entry2.source.time.strftime('%H:%M'))
    assert_equal(1, @query.times.count)
  end

  def test_fire_query_when_out_of_cached_data
    @cache.get_conn_after Time.parse('16:20')
    @cache.get_conn_after Time.parse('17:00')

    assert_equal(2, @query.times.count)
    assert_equal('16:20', @query.times[0].strftime('%H:%M'))
    assert_equal('17:00', @query.times[1].strftime('%H:%M'))
  end

  def test_not_first_connection_from_second_get_is_ok
    entry1 = @cache.get_conn_after Time.parse('16:30')
    entry2 = @cache.get_conn_after Time.parse('17:30')

    assert_equal('16:35', entry1.source.time.strftime('%H:%M'))
    assert_equal('17:40', entry2.source.time.strftime('%H:%M'))
  end
end
