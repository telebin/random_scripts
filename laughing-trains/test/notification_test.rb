require_relative 'pkp_query_mock'

notifier = GirNotificationExecutor.new
mik_wro_cache = EntryCache.new PkpQueryMock.new.from(LOCATIONS[:mik]).to(LOCATIONS[:wro])

