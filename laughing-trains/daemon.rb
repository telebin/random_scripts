require_relative 'fix_integer'
require_relative 'pkp_query'
require_relative 'log'
require_relative 'entry_cache'
require_relative 'notification'
require_relative 'gir_notification_executor'

# TODO make configurable
MINIMAL_DELAY = 30.minutes

class Daemon
  def initialize
    @notifier = GirNotificationExecutor.new
# TODO make route configurable
    @mik_cze_cache = EntryCache.new PkpQuery.new.from(LOCATIONS[:mik]).to(LOCATIONS[:cze])
    #@wro_zach_cache = EntryCache.new PkpQuery.new.from(LOCATIONS[:wro]).to(LOCATIONS[:zach])
  end

  def run
    while true
      # TODO add support for sooner trains (i.e. 10 min from now - instant notification?)
      now = Time.now
      delayed_time = now + MINIMAL_DELAY
      soonest_conn = @mik_cze_cache.get_conn_after(delayed_time)
      #change_conn = @wro_zach_cache.get_conn_after(soonest_conn.destination.time + 6.minutes)

      log "Now waiting #{soonest_conn.source.time - delayed_time}s with notification..."
      sleep(soonest_conn.source.time - delayed_time)
      notification = Notification.new 'Interesting trainz', soonest_conn
      log 'Executing notification'
      @notifier.execute notification
    end
  end
end
