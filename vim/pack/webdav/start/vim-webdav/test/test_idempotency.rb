#!/usr/bin/env ruby
# WebDAV idempotency tests

require_relative 'test_base'

class TestWebDAVIdempotency < TestWebDAVBase
  # Test multiple saves of same content should be idempotent
  def test_multiple_saves_same_content
    start_vim("WEBDAV_DEFAULT_URL" => "http://localhost:9999")
    vim_cmd("WebDAVGet /test/file1.txt")
    wait_for_text("This is test file content")

    # Save without changes (first save)
    vim_cmd("write")
    wait_for_screen_change

    # Save again without changes (second save)
    vim_cmd("write")
    wait_for_screen_change

    # Save again without changes (third save)
    vim_cmd("write")
    wait_for_screen_change

    # All saves should succeed (no errors in output)
    output = capture
    refute_match(/Error|Conflict/i, output, "Multiple saves should not produce errors")
  end

  # Test save-reload-save cycle (idempotent round trip)
  def test_save_reload_save_cycle
    start_vim("WEBDAV_DEFAULT_URL" => "http://localhost:9999")
    vim_cmd("WebDAVGet /test/file1.txt")
    wait_for_text("This is test file content")

    # Modify and save
    vim_cmd("normal! ggdG")
    vim_cmd("call setline(1, 'Idempotency test content')")
    vim_cmd("write")
    wait_for_screen_change

    # Close and reopen
    vim_cmd("bdelete!")
    wait_for_screen_change
    vim_cmd("WebDAVGet /test/file1.txt")
    wait_for_text("Idempotency test content")

    # Save again without conflict
    vim_cmd("call setline(2, 'Line 2')")
    vim_cmd("write")
    wait_for_screen_change

    output = capture
    # Should not have conflict after proper reload cycle
    refute_match(/Conflict|서버 파일이 변경/i, output, "Should not have conflict after reload")
    # Should see saved message
    assert_match(/Saved to WebDAV/i, output, "Second save should succeed")
  end

  # Test multiple edit-save cycles in single buffer
  def test_modify_save_modify_save
    start_vim("WEBDAV_DEFAULT_URL" => "http://localhost:9999")
    vim_cmd("WebDAVGet /test/file1.txt")
    wait_for_text("This is test file content")

    # First modification and save
    vim_cmd("call setline(1, 'Modification 1')")
    vim_cmd("write")
    wait_for_screen_change

    # Second modification and save
    vim_cmd("call setline(1, 'Modification 2')")
    vim_cmd("write")
    wait_for_screen_change

    # Third modification and save
    vim_cmd("call setline(1, 'Modification 3')")
    vim_cmd("write")
    wait_for_screen_change

    output = capture
    # Should have saved successfully multiple times
    refute_match(/Conflict|Error/i, output, "Multiple saves should not produce conflicts or errors")
  end

  # Test reopening after external change gets fresh version
  def test_reopen_after_external_change
    start_vim("WEBDAV_DEFAULT_URL" => "http://localhost:9999")
    vim_cmd("WebDAVGet /test/file1.txt")
    wait_for_text("This is test file content")

    # Save local change
    vim_cmd("call setline(1, 'My local change')")
    vim_cmd("write")
    wait_for_screen_change

    # Close buffer
    vim_cmd("bdelete!")
    wait_for_screen_change

    # External modification (simulate another client)
    docker_exec("curl -s -X PUT http://localhost:9999/test/file1.txt -d 'ExternalMod' > /dev/null")
    wait_for_screen_change

    # Reopen - should get external version
    vim_cmd("WebDAVGet /test/file1.txt")
    wait_for_text("ExternalMod")
  end

  # Test save without modifications
  def test_no_change_save
    start_vim("WEBDAV_DEFAULT_URL" => "http://localhost:9999")
    vim_cmd("WebDAVGet /test/file1.txt")
    wait_for_text("This is test file content")

    # Save without any modifications
    vim_cmd("write")
    wait_for_screen_change

    output = capture
    # Should succeed or be no-op (no errors)
    refute_match(/Error.*Not a WebDAV buffer/i, output, "Save should work even without changes")
  end

  # Test that external modification while editing is detected on save
  def test_external_modification_conflict
    start_vim("WEBDAV_DEFAULT_URL" => "http://localhost:9999")

    # Open file
    vim_cmd("WebDAVGet /test/file1.txt")
    wait_for_text("This is test file content")

    # Make local modification (but don't save yet)
    vim_cmd("call setline(1, 'Local modification')")
    wait_for_screen_change

    # External modification happens (simulate another client saving)
    docker_exec("curl -s -X PUT http://localhost:9999/test/file1.txt -d 'External save' > /dev/null")
    wait_for_screen_change

    # Now try to save our local changes (should conflict)
    vim_cmd("write")
    wait_for_text("Conflict", 2)

    output = capture
    # Should see conflict message (Korean or English)
    assert_match(/Conflict|서버 파일이 변경|Opening latest version/i, output, "Should detect conflict from external modification")
  end
end
