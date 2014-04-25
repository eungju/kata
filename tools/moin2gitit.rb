#!/usr/bin/env ruby

require 'fileutils'

if ARGV.length != 2
  puts "Usages: #{$0} <moin pages directory> <gitit wikidata directory>"
  exit 1
end

from_dir = File.expand_path ARGV[0]
to_dir = File.expand_path ARGV[1]
Dir.new(from_dir).each do |page|
  #skip . and ..
  next if page.match('^[.].*')
  #decode page name
  page_name = page.gsub(/\([^)]+\)/) { |chunk|
    chunk[1..-2].gsub(/../) { |ch| ch.hex.chr }
  }
  #handle subpages
  page_name = page_name.gsub('/', '-')
  #pick the current reversion
  version = File.open(File.join(from_dir, page, 'current')) { |f| f.readline.chop }

  #copy the page
  puts "Copy #{page}/revisions/#{version} to #{page_name}"
  FileUtils.cp(File.join(from_dir, page, 'revisions', version), File.join(to_dir, page_name + '.page'), :verbose => true)

  #copy the attachments if it is exist
  attachments = File.join(from_dir, page, 'attachments')
  if File.exist?(attachments)
    Dir.mkdir(File.join(to_dir, page_name))
    Dir.new(attachments).each do |attachment|
      next if attachment.match('^[.].*')
      FileUtils.cp(File.join(attachments, attachment), File.join(to_dir, page_name, attachment), :verbose => true)
    end
  end
end
