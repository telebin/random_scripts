require 'fileutils'

OUTPUT = ARGV[0].end_with?('/') ? ARGV[0] : (ARGV[0] + '/')

def create_dirs(mp3)
  puts "Creating dirs for #{mp3}"
  path = mp3[0, mp3.rindex('/')]
  FileUtils.mkdir_p(path)
end

def fire_ffmpeg(ogg, mp3)
  cmd = ["ffmpeg", '-y', '-i', ogg, '-map_metadata', '0:s:0', '-q:a', '3', mp3].join(' ')
  puts "Converting #{ogg} to #{mp3} with cmd <#{cmd}>"
  system("ffmpeg", '-y', '-i', ogg, '-map_metadata', '0:s:0', '-q:a', '3', mp3)
end

ILLEGAL_CHARS = '"*:<>?\|'.chars.map(&:ord)
def convert(ogg)
  mp3 = (OUTPUT + ogg.sub('ogg', 'mp3')).chars.map {|c| ILLEGAL_CHARS.include?(c.ord) ? '_' : c }.join
  if File.exists? mp3
    puts "File #{mp3} exists, skipping"
    return false
  end
  create_dirs mp3
  unless fire_ffmpeg ogg, mp3
    puts "Failed converting #{ogg}"
    return false
  end
end

unless OUTPUT
  puts 'Script reads from all oggs from working dir recursively and converts them to mp3 storing in same structure in <output_dir>'
  puts "Usage: <ruby_cmd> #$0 <output_dir>"
  exit 1
end

# TODO add progress bar and put ffmpeg logs into files (one for each process)
all_oggs = Dir.glob('**/*.ogg')
all_oggs.each_slice(all_oggs.count / 8) do |oggs|
  fork do
    puts "Fork here! Converting from <#{oggs.first}> to <#{oggs.last}>"
    oggs.each { |ogg| convert ogg }
  end
end
Process.waitall

