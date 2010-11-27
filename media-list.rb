# ------------------------------------------------------------------------------
# Usage:
# - Put this file in `media-dir-prefix'
# - Change dirs to match `media-dir'
# - Schedule it to run regularly and send its output to 'list'.
#   Make sure you cd into `media-dir-prefix' before running:
#   cd /mnt/share && ruby media-list.rb > list
# ------------------------------------------------------------------------------

require 'find'

dirs = ["torrents/", "shows/"]

for dir in dirs
  Find.find(dir) do |path|
    if FileTest.directory?(path)
      next
    elsif path =~ /(\.avi$|\.mp4$|\.mpg$|\.mpeg$|\.mov$|\.mkv$)/
      modtime = File.mtime(path)
      print path + "\n" + modtime.ctime + "\n"
    end
  end
end


