require 'gir_ffi-gtk3'
require_relative 'log'

class GirNotificationExecutor
  def initialize(timeout = 0)
    @timeout = timeout.to_i * 1000
    @random = Random.new
    Thread.new {
      Gtk::init
      Gtk::main
    }
    GirFFI.setup :Notify
    Notify.init('Train parser')
  end

  def execute(n)
    notif = Notify::Notification.new(n.header, n.generate_body, 'dialog-information')
    notif.timeout = @timeout
    notif.add_action('link' + @random.rand(8 << 32).to_s(32), 'Do nothing.') {
      log 'Clicked button, TODO'
    }
    show_notification(notif)
  end

  private
  def show_notification(notif)
    begin
      notif.show
    rescue RuntimeError => e
      log "Error occurred while showing notification with following body: #{notif.body}"
      log e
    end
  end
end
