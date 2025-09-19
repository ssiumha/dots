#!/usr/bin/env ruby
# WebDAV plugin test suite using Docker + tmux

require 'minitest/autorun'
require 'securerandom'
require_relative 'tmux_helper'
require_relative 'screen_helper'

class TestWebDAV < Minitest::Test
  include TmuxHelper
  include ScreenHelper

  def setup
    @container = "vim-webdav-test-#{SecureRandom.hex(4)}"

    # Start container
    system("docker run --rm -d --name #{@container} vim-webdav-test", out: '/dev/null')
    wait_for_container

    # Start mock server
    docker_exec("cd /root/.vim/pack/webdav/start/vim-webdav/test && ruby mock-server.rb > /dev/null 2>&1 &")
    wait_for_server

    # Create tmux session
    docker_exec("tmux new-session -d -s test")
  end

  def wait_for_container(timeout = 2)
    start = Time.now
    while Time.now - start < timeout
      output = docker_exec("echo 'ready'")
      return if output.strip == "ready"
      sleep 0.05
    end
    raise "Container not ready"
  end

  def wait_for_server(timeout = 2)
    start = Time.now
    while Time.now - start < timeout
      output = docker_exec("curl -s -o /dev/null -w '%{http_code}' http://localhost:9999 || echo '000'")
      return if output.strip != "000"
      sleep 0.05
    end
    raise "Mock server not ready"
  end

  def teardown
    system("docker rm -f #{@container}", out: '/dev/null')
  end

  # Test plugin loads
  def test_plugin_loads
    start_vim("WEBDAV_DEFAULT_URL" => "http://localhost:9999")
    vim_cmd("if exists('g:loaded_webdav') | echo 'LOADED' | endif")
    assert_includes capture, "LOADED"
  end

  # Test commands exist
  def test_commands_exist
    start_vim("WEBDAV_DEFAULT_URL" => "http://localhost:9999")
    vim_cmd("if exists(':WebDAVList') == 2 | echo 'LIST_EXISTS' | endif")
    assert_includes capture, "LIST_EXISTS"

    vim_cmd("if exists(':WebDAVGet') == 2 | echo 'GET_EXISTS' | endif")
    assert_includes capture, "GET_EXISTS"
  end

  # Test WebDAVList shows files
  def test_webdav_list_shows_files
    start_vim("WEBDAV_DEFAULT_URL" => "http://localhost:9999")
    vim_cmd("WebDAVList /test/")

    # Check sorted order: ../ first, then directories, then files
    assert_screen_includes <<~SCREEN
      " WebDAV: /test/
      ../
      aaa_folder/
      folder1/
      zzz_folder/
      001_file.txt
      file1.txt
      한글.md
    SCREEN
  end

  # Test Enter key mapping works
  def test_enter_key_opens_file
    start_vim("WEBDAV_DEFAULT_URL" => "http://localhost:9999")
    vim_cmd("WebDAVList /test/")

    # Move to file1.txt line and press Enter (now on line 7 with ../ at top)
    send_keys("7G")  # Go to line 7 (file1.txt after ../ and folders)
    send_enter
    wait_for { capture.include?("This is test file content") }

    # Check if file opened
    output = capture
    assert_includes output, "This is test file content", "Enter should open the file"
  end

  # Test buffer is not modifiable
  def test_buffer_not_modifiable
    start_vim("WEBDAV_DEFAULT_URL" => "http://localhost:9999")
    vim_cmd("WebDAVList /test/")

    send_keys("i")  # Try insert mode
    send_keys("test")

    output = capture
    assert_match(/modifiable.is.off|Cannot make changes/i, output)
  end

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

  # Test error without URL
  def test_error_without_url
    start_vim()  # Start without env var
    vim_cmd("WebDAVList /")

    output = capture
    assert_includes output, "Error: Set WEBDAV_DEFAULT_URL"
  end

  # Test with pattern matching (using ... for any content)
  def test_webdav_list_pattern
    start_vim("WEBDAV_DEFAULT_URL" => "http://localhost:9999")
    vim_cmd("WebDAVList /test/")

    # Use pattern with ... to skip lines (../ is now at top)
    assert_screen_pattern <<~PATTERN
      " WebDAV: /test/
      ../
      ...
      한글.md
    PATTERN
  end

  # Test using specific helper methods
  def test_webdav_list_with_helpers
    start_vim("WEBDAV_DEFAULT_URL" => "http://localhost:9999")
    vim_cmd("WebDAVList /test/")

    assert_header '" WebDAV: /test/'
    assert_files_listed '../', 'aaa_folder/', 'folder1/', 'zzz_folder/', '001_file.txt', 'file1.txt', '한글.md'
  end

  # Test with base URL having a deep path
  def test_webdav_list_with_deep_base_url
    # Base URL이 deep path를 포함하는 경우
    start_vim("WEBDAV_DEFAULT_URL" => "http://localhost:9999/test/deep")
    vim_cmd("WebDAVList /")  # Root of the deep path

    assert_header '" WebDAV: /'
    assert_files_listed '../', 'anotherfolder/', 'subfolder/', 'aaa.md', 'file.txt', 'zzz.txt'
  end

  # Test navigating from deep base URL
  def test_webdav_navigation_with_deep_base_url
    start_vim("WEBDAV_DEFAULT_URL" => "http://localhost:9999/test/deep")
    vim_cmd("WebDAVList /subfolder/")

    assert_screen_includes <<~SCREEN
      " WebDAV: /subfolder/
      ../
      nested.txt
    SCREEN
  end

  # Test that folders appear before files in sorted order
  def test_sorted_listing_folders_first
    start_vim("WEBDAV_DEFAULT_URL" => "http://localhost:9999")
    vim_cmd("WebDAVList /test/")

    output = capture
    # Get content lines excluding vim chrome
    lines = output.split("\n").take_while { |l| !l.start_with?("~") }.select { |l| !l.empty? }

    # Extract the file list (skip header and ../, and any status lines)
    files_and_dirs = lines[2..-1].reject { |l| l == "../" || l.include?("All") || l.include?("more lines") }  # Skip header and ../ on line 2

    # Check that all directories come before all files
    dir_indices = files_and_dirs.each_index.select { |i| files_and_dirs[i].end_with?("/") }
    file_indices = files_and_dirs.each_index.reject { |i| files_and_dirs[i].end_with?("/") }

    unless dir_indices.empty? || file_indices.empty?
      assert dir_indices.max < file_indices.min,
             "Directories should appear before files. Got: #{files_and_dirs.inspect}"
    end

    # Also check alphabetical order within each group
    dirs = files_and_dirs.select { |f| f.end_with?("/") }
    files = files_and_dirs.reject { |f| f.end_with?("/") }

    assert_equal dirs.sort, dirs, "Directories should be alphabetically sorted"
    assert_equal files.sort, files, "Files should be alphabetically sorted"
  end
end
