#!/usr/bin/env ruby
# WebDAV internationalization workflow tests

require_relative 'test_base'

class TestWebDAVI18nWorkflow < TestWebDAVBase
  def test_korean_filename_workflow
    start_vim("WEBDAV_DEFAULT_URL" => "http://localhost:9999")

    # Open list
    vim_cmd("WebDAVList /test/")
    wait_for_text("한글.md", 2)

    # Find 한글.md in list
    output = capture
    assert_includes output, "한글.md", "Should show Korean filename in list"

    # Navigate to it
    send_keys("/한글.md")  # Search for 한글.md
    send_enter  # Execute search
    send_enter  # Open file
    wait_for_text("한글 문서", 2)

    # Should open Korean file
    output = capture
    assert_includes output, "한글 문서", "Should open Korean file content"

    # Modify and save (use send_keys for proper editing)
    send_keys("gg")  # First line
    send_keys("dG")  # Delete all
    send_keys("i")  # Insert mode
    send_keys("# 워크플로우 테스트")
    send_keys("\e")  # Exit insert mode

    vim_cmd("write")

    # Verify saved - check server content
    server_content = docker_exec("curl -s http://localhost:9999/test/한글.md")
    assert_includes server_content, "워크플로우 테스트", "Korean file content should be saved on server"
  end

  # Test Korean folder names (fixes E499 error)
  def test_korean_folder_names
    start_vim("WEBDAV_DEFAULT_URL" => "http://localhost:9999")

    # Open a file in Korean-named folder
    vim_cmd("tabnew")
    vim_cmd("WebDAVGet /test/한글폴더/test.txt")
    sleep 1

    # Check buffer is created without E499 error
    vim_cmd("echo exists('b:webdav_managed')")
    wait_for_text("1", 1)

    output = capture
    assert_includes output, "1", "Should handle Korean folder names without E499 error"

    # Verify buffer name is set correctly
    vim_cmd("echo bufname(bufnr())")
    wait_for_text("webdav://", 1)

    output = capture
    assert_match /webdav:\/\/.*한글폴더/, output, "Buffer name should contain Korean folder name"
  end

  # Test special characters in folder names
  def test_special_chars_in_paths
    start_vim("WEBDAV_DEFAULT_URL" => "http://localhost:9999")

    # Test path with spaces
    vim_cmd("tabnew")
    vim_cmd("WebDAVGet /test/공백 폴더/file.txt")
    sleep 1

    # Should not crash with E499
    vim_cmd("echo exists('b:webdav_managed')")
    wait_for_text("1", 1)

    output = capture
    assert_includes output, "1", "Should handle spaces in folder name"
  end

  # Test URL encoding preservation
  def test_url_encoding_preservation
    start_vim("WEBDAV_DEFAULT_URL" => "http://localhost:9999")

    vim_cmd("tabnew")
    vim_cmd("WebDAVGet /test/한글폴더/test.txt")
    sleep 1

    # Check that original path (not URL-encoded) is stored
    vim_cmd("echo b:webdav_original_path")
    wait_for_text("/test/한글폴더/test.txt", 1)

    output = capture
    assert_includes output, "/test/한글폴더/test.txt", "Original path should be in Korean, not URL-encoded"
  end
end
