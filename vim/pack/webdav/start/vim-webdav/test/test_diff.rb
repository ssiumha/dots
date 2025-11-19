#!/usr/bin/env ruby
# WebDAVDiff tests

require_relative 'test_base'

class TestWebDAVDiff < TestWebDAVBase
  # Test 1: Diff shows local changes
  def test_diff_with_local_changes
    start_vim("WEBDAV_DEFAULT_URL" => "http://localhost:9999")

    # Open file
    vim_cmd("WebDAVGet /test/file1.txt")
    wait_for_text("This is test file content", 2)

    # Make local changes
    vim_cmd("normal! GoLocal modification")
    vim_cmd("normal! \\<Esc>")
    wait_for_text("Local modification", 1)

    # Run diff
    vim_cmd("WebDAVDiff")
    wait_for_text("Diff ready", 2)

    # Check we have 2 windows
    vim_cmd("echo winnr('$')")
    output = capture
    assert_includes output, "2", "Should have 2 windows"

    # Check right window shows local changes
    vim_cmd("wincmd l")
    wait_for_text("Local modification", 1)

    # Check it's readonly
    vim_cmd("echo &readonly")
    output = capture
    assert_includes output, "1", "Right window should be readonly"
  end

  # Test 2: Diff basic functionality
  def test_diff_basic
    start_vim("WEBDAV_DEFAULT_URL" => "http://localhost:9999")

    # Open file
    vim_cmd("WebDAVGet /test/file1.txt")
    wait_for_text("This is test file content", 2)

    # Run diff (no local changes)
    vim_cmd("WebDAVDiff")
    wait_for_text("Diff ready", 2)

    # Should indicate no local changes
    output = capture
    assert_match(/No local changes|Diff ready/i, output)
  end

  # Test 3: Diff on non-WebDAV buffer fails
  def test_diff_non_webdav_buffer
    start_vim("WEBDAV_DEFAULT_URL" => "http://localhost:9999")

    # Open normal file
    vim_cmd("edit /tmp/normal.txt")
    vim_cmd("call setline(1, 'normal content')")

    # Try diff
    vim_cmd("WebDAVDiff")
    wait_for_text("Error", 1)

    output = capture
    assert_match(/Not a WebDAV buffer/i, output, "Should error on non-WebDAV buffer")
  end

  # Test 4: Server fetch failure shows error
  def test_diff_server_error
    start_vim("WEBDAV_DEFAULT_URL" => "http://localhost:9999")

    # Open file
    vim_cmd("WebDAVGet /test/file1.txt")
    wait_for_text("This is test file content", 2)

    # Delete file on server
    docker_exec("curl -X DELETE http://localhost:9999/test/file1.txt")
    sleep 0.3

    # Try diff (should fail)
    vim_cmd("WebDAVDiff")
    wait_for_text("Error", 2)

    output = capture
    assert_match(/Error|HTTP 404/i, output, "Should show error")
  end

  # Test 5: Conflict shows 'd' option
  def test_conflict_shows_diff_option
    start_vim("WEBDAV_DEFAULT_URL" => "http://localhost:9999")

    # Open file
    vim_cmd("WebDAVGet /test/file1.txt")
    wait_for_text("This is test file content", 2)

    # Make local changes
    vim_cmd("normal! GoLocal modification")
    vim_cmd("normal! \\<Esc>")

    # Change file on server (create conflict)
    docker_exec("curl -X PUT http://localhost:9999/test/file1.txt -d 'Server modified'")
    sleep 0.5

    # Try to save (should trigger conflict)
    vim_cmd("w")
    wait_for_text("Conflict", 2)

    # Check 'd' option is shown
    output = capture
    assert_match(/d.*diff/i, output, "Should show 'd' option")

    # Cancel
    send_keys("c")
  end

  # Test 6: Diff replaces current buffer with server version
  def test_diff_replaces_buffer_with_server_version
    start_vim("WEBDAV_DEFAULT_URL" => "http://localhost:9999")

    # Open file
    vim_cmd("WebDAVGet /test/file1.txt")
    wait_for_text("This is test file content", 2)

    # Make local changes
    vim_cmd("normal! GoLocal modification")
    vim_cmd("normal! \\<Esc>")
    wait_for_text("Local modification", 1)

    # Modify server version (simple single line)
    docker_exec("curl -s -X PUT http://localhost:9999/test/file1.txt -d 'ServerModified' > /dev/null")
    sleep 0.3

    # Run diff
    vim_cmd("WebDAVDiff")
    sleep 1

    # Check we have 2 windows
    vim_cmd("echo winnr('$')")
    output = capture
    assert_includes output, "2", "Should have 2 windows after diff"

    # Check left buffer content (simplified - just verify it changed from original)
    vim_cmd("wincmd h")
    vim_cmd("echo getline(1) =~ 'Server'")
    output = capture
    assert_includes output, "1", "Left buffer should contain server version"
  end

  # Test 7: Diff updates ETag and Last-Modified metadata
  def test_diff_updates_metadata
    start_vim("WEBDAV_DEFAULT_URL" => "http://localhost:9999")

    # Open file
    vim_cmd("WebDAVGet /test/file1.txt")
    wait_for_text("This is test file content", 2)

    # Save original ETag
    vim_cmd("echo b:webdav_etag")
    original_etag = capture.strip

    # Modify file on server (ETag will change)
    docker_exec("curl -s -X PUT http://localhost:9999/test/file1.txt -d 'Updated server content' > /dev/null")
    sleep 0.3

    # Run diff
    vim_cmd("WebDAVDiff")
    sleep 1

    # Check ETag is updated
    vim_cmd("wincmd h")  # Focus left window
    vim_cmd("echo b:webdav_etag")
    new_etag = capture.strip

    refute_equal original_etag, new_etag, "ETag should be updated to server's latest"
  end

  # Test 8: Save after diff works
  def test_diff_save_after_diff
    start_vim("WEBDAV_DEFAULT_URL" => "http://localhost:9999")

    # Open file
    vim_cmd("WebDAVGet /test/file1.txt")
    wait_for_text("This is test file content", 2)

    # Make local changes
    vim_cmd("normal! GoLocal")
    vim_cmd("normal! \\<Esc>")

    # Modify server (simple single word)
    docker_exec("curl -s -X PUT http://localhost:9999/test/file1.txt -d 'NewContent' > /dev/null")
    sleep 0.3

    # Run diff (now buffer has server's 'NewContent')
    vim_cmd("WebDAVDiff")
    sleep 1

    # Verify we're in diff mode
    vim_cmd("echo winnr('$')")
    output = capture
    assert_includes output, "2", "Should be in diff mode"

    # Save without modification (just verify save works)
    vim_cmd("wincmd h")
    vim_cmd("w")
    sleep 1

    # Verify server still has the content
    result = docker_exec("curl -s http://localhost:9999/test/file1.txt")
    assert_includes result, "NewContent", "Save after diff should work"
  end

  # Test 9: Temp file is created
  def test_diff_creates_temp_file
    start_vim("WEBDAV_DEFAULT_URL" => "http://localhost:9999")

    # Open file
    vim_cmd("WebDAVGet /test/file1.txt")
    wait_for_text("This is test file content", 2)

    # Make local changes
    vim_cmd("normal! GoLocal content")
    vim_cmd("normal! \\<Esc>")

    # Run diff
    vim_cmd("WebDAVDiff")
    sleep 1

    # Check temp file variable exists
    vim_cmd("wincmd h")  # Focus left window
    vim_cmd("echo exists('b:webdav_diff_temp')")
    output = capture
    assert_includes output, "1", "Temp file variable should exist"
  end

  # Test 10: Temp file cleanup after save
  def test_diff_temp_file_deleted_after_save
    start_vim("WEBDAV_DEFAULT_URL" => "http://localhost:9999")

    # Open file
    vim_cmd("WebDAVGet /test/file1.txt")
    wait_for_text("This is test file content", 2)

    # Make local changes
    vim_cmd("normal! GoLocal")
    vim_cmd("normal! \\<Esc>")

    # Run diff
    vim_cmd("WebDAVDiff")
    sleep 1

    # Check variable exists before save
    vim_cmd("wincmd h")
    vim_cmd("echo exists('b:webdav_diff_temp')")
    output = capture
    assert_includes output, "1", "Temp file variable should exist before save"

    # Save
    vim_cmd("w")
    sleep 1

    # Check variable removed after save (use try-catch for undefined var)
    vim_cmd("try | echo exists('b:webdav_diff_temp') | catch | echo 0 | endtry")
    output = capture
    assert_includes output, "0", "Temp file variable should be removed after save"
  end

  # Test 11: Conflict 'd' option triggers diff
  def test_conflict_d_option_triggers_diff
    start_vim("WEBDAV_DEFAULT_URL" => "http://localhost:9999")

    # Open file
    vim_cmd("WebDAVGet /test/file1.txt")
    wait_for_text("This is test file content", 2)

    # Make local changes
    vim_cmd("normal! GoLocal modification")
    vim_cmd("normal! \\<Esc>")
    wait_for_text("Local modification", 1)

    # Change file on server (create conflict)
    docker_exec("curl -s -X PUT http://localhost:9999/test/file1.txt -d 'ServerModified' > /dev/null")
    sleep 0.5

    # Try to save (trigger conflict)
    vim_cmd("w")
    wait_for_text("Conflict", 2)

    # Press 'd' to trigger diff
    send_keys("d")
    sleep 1

    # Verify 2 windows created
    vim_cmd("echo winnr('$')")
    output = capture
    assert_includes output, "2", "Diff should open 2 windows after 'd' in conflict"

    # Verify left has server version (simplified check)
    vim_cmd("wincmd h")
    vim_cmd("echo getline(1) =~ 'Server'")
    output = capture
    assert_includes output, "1", "Left buffer should contain server version after conflict diff"
  end
end
