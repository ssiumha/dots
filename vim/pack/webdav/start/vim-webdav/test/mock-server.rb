#!/usr/bin/env ruby
# Simple mock WebDAV server using only Ruby built-ins

require 'socket'
require 'uri'

server = TCPServer.new(9999)
puts "Mock WebDAV server running on http://localhost:9999"
puts "Press Ctrl+C to stop"

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

    response_body = case decoded_path
    when '/test/file1.txt'
      "This is test file content.\nLine 2\nLine 3"
    when '/test/한글.md'
      "# 한글 문서\n\n테스트 내용입니다."
    when '/test/deep/file.txt'
      "Deep path file content\nLine 2"
    when '/test/deep/subfolder/nested.txt'
      "Nested file content"
    else
      "Mock content for #{path}"
    end

    client.puts "HTTP/1.1 200 OK\r\n"
    client.puts "Content-Type: text/plain; charset=utf-8\r\n"
    client.puts "Content-Length: #{response_body.bytesize}\r\n"
    client.puts "\r\n"
    client.puts response_body
  end

  client.close
end