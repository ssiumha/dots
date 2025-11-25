#!/usr/bin/env ruby
# Tests for Obsidian-style wikilink support

require_relative 'test_base'

class TestWikilink < TestWebDAVBase
  # Test: Open relative path wikilink
  def test_open_relative_path
    start_vim("WEBDAV_DEFAULT_URL" => "http://localhost:9999")

    # Set vault root using separate commands
    vim_cmd("let g:webdav_vault_roots = {}")
    vim_cmd("let g:webdav_vault_roots.default = '/vault'")

    # Open a file with relative link
    vim_cmd("WebDAVGet /vault/notes/daily/2024-01-01.md")
    wait_for_text("Daily Note 2024-01-01", 2)

    # Move cursor to line 5 (relative link), inside [[]]
    # File content line 5: "- Relative parent: [[../index.md]]"
    vim_cmd("call cursor(5, 24)")
    sleep 0.2

    # Use command to follow link
    vim_cmd("WebDAVFollowLink")
    wait_for_text("Notes Index", 3)

    # Verify correct file opened
    vim_cmd("echo b:webdav_original_path")
    sleep 0.2
    output = capture
    assert_includes output, "/vault/notes/index.md", "Should open relative wikilink"
  end

  # Test: Open filename-only wikilink (adds .md)
  def test_open_filename_only
    start_vim("WEBDAV_DEFAULT_URL" => "http://localhost:9999")

    # Set vault root using separate commands
    vim_cmd("let g:webdav_vault_roots = {}")
    vim_cmd("let g:webdav_vault_roots.default = '/vault'")

    # Open vault index
    vim_cmd("WebDAVGet /vault/index.md")
    wait_for_text("Vault Index", 2)

    # Move cursor to line 5 [[projects/todo.md]] link
    # File content line 5: "- [[projects/todo.md]]"
    vim_cmd("call cursor(5, 6)")
    sleep 0.2

    # Use command to follow link
    vim_cmd("WebDAVFollowLink")
    wait_for_text("TODO List", 3)

    # Verify correct file opened
    vim_cmd("echo b:webdav_original_path")
    sleep 0.2
    output = capture
    assert_includes output, "/vault/projects/todo.md", "Should open filename wikilink"
  end

  # Test: Open parent directory wikilink (../)
  def test_open_parent_dir_link
    start_vim("WEBDAV_DEFAULT_URL" => "http://localhost:9999")

    # Set vault root using separate commands
    vim_cmd("let g:webdav_vault_roots = {}")
    vim_cmd("let g:webdav_vault_roots.default = '/vault'")

    # Start from notes index
    vim_cmd("WebDAVGet /vault/notes/index.md")
    wait_for_text("Notes Index", 2)

    # Follow parent dir link (../index.md) - line 3
    vim_cmd("call cursor(3, 6)")
    sleep 0.2
    vim_cmd("WebDAVFollowLink")
    wait_for_text("Vault Index", 3)
    vim_cmd("echo b:webdav_original_path")
    sleep 0.2
    output = capture
    assert_includes output, "/vault/index.md", "Should open parent dir wikilink"
  end

  # Test: Open absolute path wikilink ([[/path/to/file]])
  # Combined with parent dir test to reuse vault_root setup
  def test_open_absolute_path_link
    start_vim("WEBDAV_DEFAULT_URL" => "http://localhost:9999", "WEBDAV_TEST_MODE" => "1")

    # Set vault root (same pattern as other passing tests)
    vim_cmd("let g:webdav_vault_roots = {}")
    vim_cmd("let g:webdav_vault_roots.default = '/vault'")

    # First: Open notes index
    vim_cmd("WebDAVGet /vault/notes/index.md")
    wait_for_text("Notes Index", 2)

    # Navigate to parent (to get to vault root level)
    vim_cmd("call cursor(3, 6)")  # [[../index.md]]
    sleep 0.2
    vim_cmd("WebDAVFollowLink")
    wait_for_text("Vault Index", 3)

    # Re-set vault root before absolute path test
    vim_cmd("call TestSetDefaultVaultRoot()")

    # Now test absolute path: from /vault/index.md, follow [[/notes/index.md]]
    # Line 4: "- [[/notes/index.md]]"
    vim_cmd("call cursor(4, 6)")
    sleep 0.2

    vim_cmd("WebDAVFollowLink")
    wait_for_text("Notes Index", 3)

    # Verify correct file opened via absolute path
    vim_cmd("echo b:webdav_original_path")
    sleep 0.2
    output = capture
    assert_includes output, "/vault/notes/index.md", "Should open absolute path wikilink"
  end


  # Test: No wikilink under cursor message
  def test_no_wikilink_message
    start_vim("WEBDAV_DEFAULT_URL" => "http://localhost:9999")

    # Open a file
    vim_cmd("WebDAVGet /vault/projects/todo.md")
    wait_for_text("TODO List", 2)

    # Move to header line (no wikilink)
    vim_cmd("call cursor(1, 1)")
    sleep 0.1

    # Try to follow link
    vim_cmd("WebDAVFollowLink")
    sleep 0.3
    output = capture
    assert_includes output, "No wikilink under cursor", "Should show message when no wikilink"
  end

  # Test: Deep URL path vault root (e.g., /vault/nested/path)
  # This tests the scenario where base URL has multiple path segments
  def test_deep_url_path_vault_root
    start_vim("WEBDAV_DEFAULT_URL" => "http://localhost:9999", "WEBDAV_TEST_MODE" => "1")

    # Set deep vault root
    vim_cmd("call TestSetDeepVaultRoot()")

    # Open file in deep vault
    vim_cmd("WebDAVGet /vault/nested/path/notes/daily.md")
    wait_for_text("Daily Notes", 2)

    # Follow absolute path link [[/subdir/note.md]]
    # Line 3: "- [[/subdir/note.md]]"
    vim_cmd("call cursor(3, 6)")
    sleep 0.2
    vim_cmd("WebDAVFollowLink")
    wait_for_text("Subdir Note", 3)

    # Verify correct path: should be /vault/nested/path/subdir/note.md
    # NOT /vault/nested/path/vault/subdir/note.md (the bug we fixed)
    vim_cmd("echo b:webdav_original_path")
    sleep 0.2
    output = capture
    assert_includes output, "/vault/nested/path/subdir/note.md", "Should resolve absolute path with deep vault root"
  end

  # Test: External path - vault_root outside server base path
  # Server URL: http://host/vault/nested/path
  # vault_root: /vault
  # wikilink: [[/external/shared.md]] -> http://host/vault/external/shared.md
  def test_external_path_vault_root
    start_vim("WEBDAV_DEFAULT_URL" => "http://localhost:9999/vault/nested/path", "WEBDAV_TEST_MODE" => "1")

    # Set vault_root to /vault (parent of server base path)
    vim_cmd("call TestSetVaultOnlyRoot()")

    # Open file in deep vault that links to external path
    vim_cmd("WebDAVGet /link-to-external.md")
    wait_for_text("Link to External", 2)

    # Follow link to external file [[/external/shared.md]]
    # Line 3: "- [[/external/shared.md]]"
    vim_cmd("call cursor(3, 6)")
    sleep 0.2
    vim_cmd("WebDAVFollowLink")
    wait_for_text("Shared External File", 3)

    # Verify correct path: /vault/external/shared.md
    # NOT /vault/nested/path/vault/external/shared.md
    vim_cmd("echo b:webdav_original_path")
    sleep 0.2
    output = capture
    assert_includes output, "/vault/external/shared.md", "Should resolve external path with vault_root outside server base"

    # Verify b:webdav_base_url is set for external paths
    vim_cmd("echo exists('b:webdav_base_url')")
    sleep 0.1
    output = capture
    assert_includes output, "1", "Should have b:webdav_base_url set for external path"

    # Verify b:webdav_url is the original server URL (for validation)
    vim_cmd("echo b:webdav_url")
    sleep 0.1
    output = capture
    assert_includes output, "/vault/nested/path", "b:webdav_url should be original server URL"
  end

  # Test: External path file can be saved without URL mismatch error
  def test_external_path_save
    start_vim("WEBDAV_DEFAULT_URL" => "http://localhost:9999/vault/nested/path", "WEBDAV_TEST_MODE" => "1")

    vim_cmd("call TestSetVaultOnlyRoot()")

    # Open external file
    vim_cmd("WebDAVGet /link-to-external.md")
    wait_for_text("Link to External", 2)

    # Follow link to external file
    vim_cmd("call cursor(3, 6)")
    sleep 0.2
    vim_cmd("WebDAVFollowLink")
    wait_for_text("Shared External File", 3)

    # Modify content
    vim_cmd("normal! Go")
    vim_cmd("normal! iTest modification")
    sleep 0.1

    # Save should work without URL mismatch error
    vim_cmd("WebDAVPut")
    sleep 1

    # Check for success message (not error)
    output = capture
    refute_includes output, "URL mismatch", "Should not have URL mismatch error"
  end

  # Test: Wikilink extraction with heading anchor
  def test_extract_with_heading
    start_vim("WEBDAV_DEFAULT_URL" => "http://localhost:9999", "WEBDAV_TEST_MODE" => "1")

    # Create buffer with heading anchor link
    vim_cmd("enew")
    vim_cmd("let b:webdav_managed = 1")
    vim_cmd("let b:webdav_original_path = '/test.md'")
    vim_cmd("let b:webdav_server = ''")
    vim_cmd("let g:webdav_vault_roots = {}")
    vim_cmd("let g:webdav_vault_roots.default = '/vault'")
    vim_cmd("call setline(1, '[[target#section]]')")
    vim_cmd("call cursor(1, 5)")
    sleep 0.1

    # Check extraction - should strip the #section part
    vim_cmd("let result = TestWikilinkExtract()")
    sleep 0.1
    vim_cmd("echo result.target")
    sleep 0.2
    output = capture
    assert_includes output, "target", "Should extract target without heading anchor"
  end
end
