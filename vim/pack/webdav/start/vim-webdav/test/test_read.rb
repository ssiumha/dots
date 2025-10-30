#!/usr/bin/env ruby
# WebDAV file reading tests

require_relative 'test_base'

class TestWebDAVRead < TestWebDAVBase
  # Test WebDAVGet retrieves content
  def test_webdav_get_content
    start_vim("WEBDAV_DEFAULT_URL" => "http://localhost:9999")
    vim_cmd("WebDAVGet /test/file1.txt")

    # Check content appears as a block
    assert_screen_block <<~CONTENT
      This is test file content.
      Line 2
      Line 3
    CONTENT
  end

  # Test Korean filename
  def test_korean_filename
    start_vim("WEBDAV_DEFAULT_URL" => "http://localhost:9999")
    vim_cmd("WebDAVGet /test/한글.md")
    sleep 0.5  # Wait for content to load

    assert_screen_block <<~CONTENT
      # 한글 문서

      테스트 내용입니다.
    CONTENT
  end

  # Test ETag version tracking
  def test_webdav_etag_tracking
    start_vim("WEBDAV_DEFAULT_URL" => "http://localhost:9999")
    vim_cmd("WebDAVGet /test/file1.txt")
    wait_for { capture.include?("This is test file content") }

    # Check that ETag was stored
    vim_cmd("echo exists('b:webdav_etag')")
    assert_includes capture, "1", "ETag should be stored in buffer variable"
  end
end
