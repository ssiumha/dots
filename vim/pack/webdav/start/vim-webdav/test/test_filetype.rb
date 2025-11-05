#!/usr/bin/env ruby
# WebDAV filetype and syntax tests

require_relative 'test_base'

class TestWebDAVFiletype < TestWebDAVBase
  # Test WebDAVList sets filetype correctly
  def test_webdav_list_filetype
    start_vim("WEBDAV_DEFAULT_URL" => "http://localhost:9999")
    vim_cmd("WebDAVList /test/")
    wait_for_text("test/")

    # Check filetype
    vim_cmd("echo &filetype")

    output = capture
    assert_includes output, "webdavlist", "WebDAVList should set filetype=webdavlist"
  end

  # Test WebDAVGet with markdown file sets correct filetype and syntax
  def test_webdav_get_markdown_syntax
    start_vim("WEBDAV_DEFAULT_URL" => "http://localhost:9999")
    vim_cmd("WebDAVGet /test/한글.md")
    wait_for_text("한글")

    # Check filetype is webdav
    vim_cmd("echo &filetype")
    output = capture
    assert_includes output, "webdav", "WebDAVGet should set filetype=webdav"

    # Check syntax is markdown
    vim_cmd("echo &syntax")
    output = capture
    assert_includes output, "markdown", "Markdown file should have syntax=markdown"
  end

  # Test WebDAVGet with text file
  def test_webdav_get_text_syntax
    start_vim("WEBDAV_DEFAULT_URL" => "http://localhost:9999")
    vim_cmd("WebDAVGet /test/file1.txt")
    wait_for_text("file1.txt")

    # Check filetype
    vim_cmd("echo &filetype")
    output = capture
    assert_includes output, "webdav", "Should set filetype=webdav"

    # Check syntax is text (or empty)
    vim_cmd("echo &syntax")
    output = capture
    # text syntax might be empty or "text"
    assert output.include?("text") || output.include?("webdav"), "Text file should have appropriate syntax"
  end

  # Test buffer settings for WebDAVList
  def test_webdav_list_buffer_settings
    start_vim("WEBDAV_DEFAULT_URL" => "http://localhost:9999")
    vim_cmd("WebDAVList /test/")
    wait_for_text("test/")

    # Check buftype
    vim_cmd("echo &buftype")
    output = capture
    assert_includes output, "nofile", "WebDAVList should have buftype=nofile"

    # Check modifiable
    vim_cmd("echo &modifiable")
    output = capture
    assert_includes output, "0", "WebDAVList should be non-modifiable"

    # Check swapfile
    vim_cmd("echo &swapfile")
    output = capture
    assert_includes output, "0", "WebDAVList should have noswapfile"
  end

  # Test buffer settings for WebDAVGet
  def test_webdav_get_buffer_settings
    start_vim("WEBDAV_DEFAULT_URL" => "http://localhost:9999")
    vim_cmd("WebDAVGet /test/file1.txt")
    wait_for_text("file1.txt")

    # Check buftype (should be empty for normal editing)
    vim_cmd("echo &buftype")
    output = capture
    refute_includes output, "nofile", "WebDAVGet should have empty buftype for editing"

    # Check swapfile
    vim_cmd("echo &swapfile")
    output = capture
    assert_includes output, "0", "WebDAV files should have noswapfile"
  end

  # Test filetype plugin loaded
  def test_ftplugin_loaded
    start_vim("WEBDAV_DEFAULT_URL" => "http://localhost:9999")
    vim_cmd("WebDAVList /test/")
    wait_for_text("test/")

    # Check if ftplugin set did_ftplugin
    vim_cmd("echo exists('b:did_ftplugin')")
    output = capture
    assert_includes output, "1", "ftplugin should be loaded"
  end

  # Test syntax highlighting exists for webdavlist
  def test_syntax_defined_for_webdavlist
    start_vim("WEBDAV_DEFAULT_URL" => "http://localhost:9999")
    vim_cmd("WebDAVList /test/")
    wait_for_text("test/")

    # Check if syntax is loaded
    vim_cmd("echo exists('b:current_syntax')")
    output = capture
    assert_includes output, "1", "Syntax should be defined"

    vim_cmd("echo b:current_syntax")
    output = capture
    assert_includes output, "webdavlist", "Syntax name should be webdavlist"
  end

  # Test extension mapping works for various file types
  def test_syntax_extension_mapping
    test_cases = {
      'test.rb' => 'ruby',
      'test.py' => 'python',
      'test.js' => 'javascript',
      'test.vim' => 'vim',
      'test.yaml' => 'yaml',
      'test.json' => 'json',
    }

    start_vim("WEBDAV_DEFAULT_URL" => "http://localhost:9999")

    test_cases.each do |filename, expected_syntax|
      # Create a test file with that extension
      docker_exec("curl -s -X PUT http://localhost:9999/test/#{filename} -d 'test content' > /dev/null")
      wait_for(0.5) { true }  # Brief wait for file creation

      # Open it
      vim_cmd("bdelete! | WebDAVGet /test/#{filename}")
      wait_for_text(filename)

      # Check syntax
      vim_cmd("echo &syntax")
      output = capture

      assert_includes output, expected_syntax, "File #{filename} should have syntax=#{expected_syntax}"
    end
  end
  def test_filetype_when_opened_from_list
    start_vim("WEBDAV_DEFAULT_URL" => "http://localhost:9999")

    # Open list
    vim_cmd("WebDAVList /test/")
    wait_for_text("test/")

    # Check list filetype
    vim_cmd("echo &filetype")
    output = capture
    assert_includes output, "webdavlist", "List should have filetype=webdavlist"

    # Open markdown file (한글.md)
    send_keys("/한글.md")  # Search for 한글.md
    send_enter  # Execute search
    wait_for_text("한글")
    send_enter  # Open file
    wait_for_text("한글")

    # Check file filetype and syntax
    vim_cmd("echo &filetype")
    output = capture
    assert_includes output, "webdav", "File should have filetype=webdav"

    vim_cmd("echo &syntax")
    output = capture
    assert_includes output, "markdown", "Markdown file should have syntax=markdown"
  end
end
