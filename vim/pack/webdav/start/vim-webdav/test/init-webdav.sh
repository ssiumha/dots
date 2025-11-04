#!/bin/bash
# Initialize WebDAV test data

# Create directory structure
mkdir -p /var/webdav/test/folder1
mkdir -p /var/webdav/test/zzz_folder
mkdir -p /var/webdav/test/aaa_folder
mkdir -p /var/webdav/test/nested
mkdir -p /var/webdav/test/deep/subfolder
mkdir -p /var/webdav/test/deep/anotherfolder

# Create test files
cat > /var/webdav/test/file1.txt << 'EOF'
This is test file content.
Line 2
Line 3
EOF

cat > /var/webdav/test/한글.md << 'EOF'
# 한글 문서

테스트 내용입니다.
EOF

cat > /var/webdav/test/001_file.txt << 'EOF'
Mock content for /test/001_file.txt
EOF

cat > /var/webdav/test/deep/file.txt << 'EOF'
Deep path file content
Line 2
EOF

cat > /var/webdav/test/deep/aaa.md << 'EOF'
# AAA Document
EOF

cat > /var/webdav/test/deep/zzz.txt << 'EOF'
ZZZ content
EOF

cat > /var/webdav/test/deep/subfolder/nested.txt << 'EOF'
Nested file content
EOF

# Set permissions
chmod -R 777 /var/webdav
