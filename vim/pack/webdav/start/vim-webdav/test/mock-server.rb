#!/usr/bin/env ruby
# Simple mock WebDAV server using only Ruby built-ins

require 'socket'
require 'uri'
require 'time'

server = TCPServer.new(9999)
puts "Mock WebDAV server running on http://localhost:9999"
puts "Press Ctrl+C to stop"

# Simple in-memory storage for file versions
$file_versions = {}

def get_file_version(path)
  $file_versions[path] ||= { etag: "\"v1\"", mtime: Time.now.httpdate, content: nil }
end

def update_file_version(path, content)
  current = $file_versions[path] || {}
  version_num = current[:etag] ? current[:etag].scan(/\d+/).first.to_i + 1 : 1
  $file_versions[path] = {
    etag: "\"v#{version_num}\"",
    mtime: Time.now.httpdate,
    content: content
  }
end

loop do
  client = server.accept
  request = client.gets

  # Parse request
  method, path, version = request.split(' ')

  # Read headers
  headers = {}
  while line = client.gets
    break if line == "\r\n"
    key, value = line.split(': ', 2)
    headers[key] = value.chomp if key
  end

  # Handle different methods
  case method
  when 'PROPFIND'
    if path.include?('/test/deep/subfolder')
      response_body = <<~XML
        <?xml version="1.0"?>
        <D:multistatus xmlns:D="DAV:">
          <D:response>
            <D:href>/test/deep/subfolder/</D:href>
          </D:response>
          <D:response>
            <D:href>/test/deep/subfolder/nested.txt</D:href>
          </D:response>
        </D:multistatus>
      XML
    elsif path.include?('/test/deep')
      response_body = <<~XML
        <?xml version="1.0"?>
        <D:multistatus xmlns:D="DAV:">
          <D:response>
            <D:href>/test/deep/</D:href>
          </D:response>
          <D:response>
            <D:href>/test/deep/file.txt</D:href>
          </D:response>
          <D:response>
            <D:href>/test/deep/subfolder/</D:href>
          </D:response>
          <D:response>
            <D:href>/test/deep/anotherfolder/</D:href>
          </D:response>
          <D:response>
            <D:href>/test/deep/aaa.md</D:href>
          </D:response>
          <D:response>
            <D:href>/test/deep/zzz.txt</D:href>
          </D:response>
        </D:multistatus>
      XML
    elsif path == '/test/' || path == '/test'
      # Main test directory
      response_body = <<~XML
        <?xml version="1.0"?>
        <D:multistatus xmlns:D="DAV:">
          <D:response>
            <D:href>/test/</D:href>
          </D:response>
          <D:response>
            <D:href>/test/file1.txt</D:href>
          </D:response>
          <D:response>
            <D:href>/test/folder1/</D:href>
          </D:response>
          <D:response>
            <D:href>/test/%ED%95%9C%EA%B8%80.md</D:href>
          </D:response>
          <D:response>
            <D:href>/test/zzz_folder/</D:href>
          </D:response>
          <D:response>
            <D:href>/test/aaa_folder/</D:href>
          </D:response>
          <D:response>
            <D:href>/test/001_file.txt</D:href>
          </D:response>
        </D:multistatus>
      XML
    elsif path.start_with?('/test/')
      # Handle any other subfolder under /test/ (empty folders)
      response_body = <<~XML
        <?xml version="1.0"?>
        <D:multistatus xmlns:D="DAV:">
          <D:response>
            <D:href>#{path}</D:href>
          </D:response>
        </D:multistatus>
      XML
    else
      response_body = <<~XML
        <?xml version="1.0"?>
        <D:multistatus xmlns:D="DAV:">
          <D:response>
            <D:href>/test/</D:href>
          </D:response>
          <D:response>
            <D:href>/test/file1.txt</D:href>
          </D:response>
          <D:response>
            <D:href>/test/folder1/</D:href>
          </D:response>
          <D:response>
            <D:href>/test/%ED%95%9C%EA%B8%80.md</D:href>
          </D:response>
          <D:response>
            <D:href>/test/zzz_folder/</D:href>
          </D:response>
          <D:response>
            <D:href>/test/aaa_folder/</D:href>
          </D:response>
          <D:response>
            <D:href>/test/001_file.txt</D:href>
          </D:response>
        </D:multistatus>
      XML
    end

    client.puts "HTTP/1.1 207 Multi-Status\r\n"
    client.puts "Content-Type: application/xml\r\n"
    client.puts "Content-Length: #{response_body.bytesize}\r\n"
    client.puts "\r\n"
    client.puts response_body

  when 'GET'
    # Decode URL-encoded path
    decoded_path = URI.decode_www_form_component(path)

    # Debug
    puts "GET: path=#{path}, decoded=#{decoded_path}"

    # Get file version info
    file_version = get_file_version(decoded_path)

    response_body = case decoded_path
    when '/test/file1.txt'
      file_version[:content] || "This is test file content.\nLine 2\nLine 3"
    when '/test/한글.md'
      file_version[:content] || "# 한글 문서\n\n테스트 내용입니다."
    when '/test/deep/file.txt'
      file_version[:content] || "Deep path file content\nLine 2"
    when '/test/deep/subfolder/nested.txt'
      file_version[:content] || "Nested file content"
    else
      file_version[:content] || "Mock content for #{path}"
    end

    client.puts "HTTP/1.1 200 OK\r\n"
    client.puts "Content-Type: text/plain; charset=utf-8\r\n"
    client.puts "Content-Length: #{response_body.bytesize}\r\n"
    client.puts "ETag: #{file_version[:etag]}\r\n"
    client.puts "Last-Modified: #{file_version[:mtime]}\r\n"
    client.puts "\r\n"
    client.puts response_body

  when 'PUT'
    # Decode URL-encoded path
    decoded_path = URI.decode_www_form_component(path)

    # Read request body
    content_length = headers['Content-Length'].to_i
    body = content_length > 0 ? client.read(content_length) : ""

    puts "PUT: path=#{decoded_path}, size=#{body.bytesize}"

    # Check conditional headers for conflict detection
    if_match = headers['If-Match']
    if_unmodified = headers['If-Unmodified-Since']

    current_version = get_file_version(decoded_path)

    # Validate preconditions
    conflict = false
    if if_match && if_match != current_version[:etag]
      conflict = true
      puts "  Conflict: ETag mismatch (expected #{if_match}, got #{current_version[:etag]})"
    elsif if_unmodified
      request_time = Time.httpdate(if_unmodified) rescue Time.now
      current_time = Time.httpdate(current_version[:mtime]) rescue Time.now
      if current_time > request_time
        conflict = true
        puts "  Conflict: File modified since #{if_unmodified}"
      end
    end

    if conflict
      # Precondition failed
      client.puts "HTTP/1.1 412 Precondition Failed\r\n"
      client.puts "Content-Length: 0\r\n"
      client.puts "\r\n"
    else
      # Success: Update file version
      update_file_version(decoded_path, body)
      new_version = get_file_version(decoded_path)

      client.puts "HTTP/1.1 204 No Content\r\n"
      client.puts "ETag: #{new_version[:etag]}\r\n"
      client.puts "Last-Modified: #{new_version[:mtime]}\r\n"
      client.puts "\r\n"
    end
  end

  client.close
end