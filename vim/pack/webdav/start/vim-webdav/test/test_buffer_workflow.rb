#!/usr/bin/env ruby
# WebDAV buffer and multi-file workflow tests

require_relative 'test_base'

class TestWebDAVBufferWorkflow < TestWebDAVBase
  def test_buffer_switching_list_file
    start_vim("WEBDAV_DEFAULT_URL" => "http://localhost:9999")

    # Open list
    vim_cmd("WebDAVList /test/")
    wait_for_text("WebDAV:")

    # Open a file
    send_keys("/file1.txt")  # Search for file1.txt
    send_enter  # Execute search
    send_enter  # Open file
    wait_for_text("This is test file content")

    # We should now have 2 buffers: list and file
    vim_cmd("echo bufnr('$')")
    output = capture
    # Should have at least 2 buffers

    # Switch back to previous buffer (list)
    vim_cmd("bprevious")
    wait_for_text("file1.txt")

    # Should see list again
    output = capture
    assert_includes output, "WebDAV:", "Should switch back to list"
    assert_includes output, "file1.txt", "Should show file list"

    # Switch forward to file
    vim_cmd("bnext")
    wait_for_text("This is test file content")

    # Should see file content
    output = capture
    assert_includes output, "This is test file content", "Should switch to file"
  end

  def test_sequential_file_operations
    start_vim("WEBDAV_DEFAULT_URL" => "http://localhost:9999")

    # Open list
    vim_cmd("WebDAVList /test/")
    wait_for_text("WebDAV:")

    # Open first file
    send_keys("/file1.txt")  # Search for file1.txt
    send_enter  # Execute search
    send_enter  # Open file
    wait_for_text("This is test file content")

    # Edit and save (use send_keys for proper editing)
    send_keys("gg")
    send_keys("dG")
    send_keys("i")
    send_keys("First file edit")
    send_keys("\e")

    vim_cmd("write")
    send_enter  # Dismiss "Press ENTER" prompt if present

    # Go back to list
    vim_cmd("bprevious")
    wait_for_text("file1.txt")

    # Open different file (한글.md)
    send_keys("/한글.md")  # Search for 한글.md
    send_enter  # Execute search
    send_enter  # Open file

    # Edit and save second file (use send_keys for proper editing)
    send_keys("gg")
    send_keys("dG")
    send_keys("i")
    send_keys("Second file edit")
    send_keys("\e")

    vim_cmd("write")
    send_enter  # Dismiss "Press ENTER" prompt if present

    # Both saves should succeed - verify both files on server
    file1_content = docker_exec("curl -s http://localhost:9999/test/file1.txt")
    assert_includes file1_content, "First file edit", "First file should be saved on server"

    file2_content = docker_exec("curl -s http://localhost:9999/test/한글.md")
    assert_includes file2_content, "Second file edit", "Second file should be saved on server"
  end
end
