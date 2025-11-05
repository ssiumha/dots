#!/usr/bin/env ruby
# WebDAV List-based workflow tests

require_relative 'test_base'

class TestWebDAVListWorkflow < TestWebDAVBase
  def test_list_open_edit_save_workflow
    start_vim("WEBDAV_DEFAULT_URL" => "http://localhost:9999")

    # Step 1: Open directory listing
    vim_cmd("WebDAVList /test/")
    wait_for_text("file1.txt", 2)

    assert_screen_includes "file1.txt", "Should show file in list"

    # Step 2: Navigate to file1.txt and press Enter
    send_keys("/file1.txt")  # Search for file1.txt
    send_enter  # Execute search
    send_enter  # Press Enter to open file
    wait_for_text("This is test file content", 2)

    # Step 3: Verify file content loaded
    output = capture
    assert_includes output, "This is test file content", "Should open file content"

    # Step 4: Edit the file (use send_keys for proper buffer editing)
    send_keys("gg")  # Go to first line
    send_keys("dG")  # Delete to end
    send_keys("i")  # Enter insert mode
    send_keys("Workflow test: modified from list")
    send_keys("\e")  # Exit insert mode
    send_keys("o")  # Open new line
    send_keys("Line 2 of workflow test")
    send_keys("\e")  # Exit insert mode

    # Step 5: Check buffer is marked as modified
    vim_cmd("echo &modified")
    wait_for_text("1", 2)
    output = capture
    assert_includes output, "1", "Buffer should be marked as modified"

    # Step 6: Save the file with :w
    vim_cmd("write")

    # Step 7: Verify save succeeded
    vim_cmd("echo &modified")
    wait_for_text("0", 2)
    output = capture
    assert_includes output, "0", "Buffer should not be modified after save"

    # Check for success message
    output = capture
    refute_match(/Error|Conflict/i, output, "Should not have errors")

    # Step 8: CRITICAL - Verify content was actually saved to server
    # Use curl to check server content directly
    server_content = docker_exec("curl -s http://localhost:9999/test/file1.txt")
    assert_includes server_content, "Workflow test: modified from list", "Content should be saved on WebDAV server (line 1)"
    assert_includes server_content, "Line 2 of workflow test", "Content should be saved on WebDAV server (line 2)"
    refute_match(/This is test file content/, server_content, "Old content should be replaced on server")

    # Step 9: Also verify by reopening in Vim
    vim_cmd("bdelete!")

    vim_cmd("tabnew")

    vim_cmd("WebDAVGet /test/file1.txt")
    wait_for_text("Workflow test: modified from list", 2)

    output = capture
    assert_includes output, "Workflow test: modified from list", "Should be able to reopen saved file"
  end

  def test_nested_folder_navigation_workflow
    start_vim("WEBDAV_DEFAULT_URL" => "http://localhost:9999")

    # Open root list
    vim_cmd("WebDAVList /test/")
    wait_for_text("folder1", 2)

    # Navigate to folder1/ and open it
    send_keys("/folder1")  # Search for folder1
    send_enter  # Execute search
    send_enter  # Open folder
    wait_for_text("/test/folder1/", 2)

    # Should now show folder1 contents
    output = capture
    assert_includes output, "/test/folder1/", "Should show folder1 path in header"
    assert_includes output, "../", "Should show parent directory"

    # If there are files in folder1, open one
    # For now just verify we can navigate back
    send_keys("2G")  # Go to ../
    send_enter
    wait_for_text("/test/", 2)

    # Should be back at /test/ (header should show /test/, not /test/folder1/)
    output = capture
    assert_includes output, '" WebDAV: http://localhost:9999/test/', "Should navigate back to parent"
    refute_match(/\/test\/folder1\//, output, "Should not show /test/folder1/ in header")
  end

  def test_enter_on_header_or_comment
    start_vim("WEBDAV_DEFAULT_URL" => "http://localhost:9999")

    vim_cmd("WebDAVList /test/")
    wait_for_text("file1.txt", 2)

    # Press Enter on header line (line 1)
    send_keys("1G")
    send_enter

    # Should still be in list (not open anything)
    output = capture
    assert_includes output, "WebDAV:", "Should stay in list when Enter on header"
    assert_includes output, "file1.txt", "Should still show list"
  end
end
