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
end
