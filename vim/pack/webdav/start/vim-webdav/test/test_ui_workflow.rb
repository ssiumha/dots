#!/usr/bin/env ruby
# WebDAV UI workflow tests

require_relative 'test_base'

class TestWebDAVUIWorkflow < TestWebDAVBase
  def test_webdavui_list_open_save_workflow
    start_vim(
      "WEBDAV_UI_LOCAL" => "http://localhost:9999/test"
    )

    # Step 1: Open server via WebDAVUI
    send_keys(":WebDAVUI local")
    send_enter
    wait_for_text("server selection", 2)
    send_enter  # Dismiss connection message
    wait_for_text("Connected", 2)

    # Step 2: Should show list (with /test/ as base)
    output = capture
    assert_includes output, "file1.txt", "Should show file list"

    # Step 3: Open file
    send_keys("/file1.txt")  # Search for file1.txt
    send_enter  # Execute search
    wait_for_screen_change
    send_enter  # Open file
    wait_for_text("test file content", 2)

    # Step 4: Verify content
    output = capture
    assert_includes output, "This is test file content", "Should load file"

    # Step 5: Modify (use send_keys for proper tab editing)
    send_keys("gg")  # Go to first line
    send_keys("dG")  # Delete all
    wait_for_screen_change
    send_keys("i")  # Insert mode
    send_keys("WebDAVUI workflow test")
    send_keys("\e")  # Exit insert mode
    wait_for_screen_change

    # Step 6: Save
    vim_cmd("write")
    wait_for_text("written", 2)

    # Step 7: Verify saved
    vim_cmd("echo &modified")
    wait_for_screen_change
    output = capture
    assert_includes output, "0", "Should save successfully"
  end
end
