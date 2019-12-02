require 'taglib'
require 'fileutils'

OUTPUT = ARGV[0]

def create_dirs(mp3)
  puts "Creating dirs for #{mp3}"
  path = mp3[0, mp3.rindex('/')]
  FileUtils.mkdir_p(path)
end

def fire_ffmpeg(ogg, mp3)
  cmd = "ffmpeg -y -i $'#{ogg.gsub /'/, "\\\\'"}' -q:a 3 $'#{mp3.gsub /'/, "\\\\'"}'"
  puts "Converting #{ogg} to #{mp3} with cmd <#{cmd}>"
  system(cmd)
end

def fill_tags(mp3, ogg)
  puts "Copying tags to #{mp3}"
  mp3.tag.artist = ogg.tag.artist
  mp3.tag.album =  ogg.tag.album
  mp3.tag.track =  ogg.tag.track
  mp3.tag.title =  ogg.tag.title
  mp3.tag.year  =  ogg.tag.year
end

def copy_tags(ogg, mp3)
  TagLib::MPEG::File.open(mp3) do |m|
    TagLib::Ogg::Vorbis::File.open(ogg) do |o|
      fill_tags m, o
      m.save(TagLib::MPEG::File::ID3v1 | TagLib::MPEG::File::ID3v2)
    end
  end
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
  copy_tags ogg, mp3
end

unless OUTPUT
  puts 'Script reads from all oggs from working dir recursively and converts them to mp3 storing in same structure in <output_dir>'
  puts "Usage: <ruby_cmd> #$0 <output_dir>"
  exit 1
end

all_oggs = Dir.glob('**/*.ogg')
all_oggs.each_slice(all_oggs.count / 4) do |oggs|
  fork do
    puts "Fork here! Converting from <#{oggs.first}> to <#{oggs.last}>"
    oggs.each { |ogg| convert ogg }
  end
end
Process.waitall

