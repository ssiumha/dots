module Lib
  class MysqlChiz < Base
    md :alter_lock, 'alter lock algorithm', <<~MD
      MySQL 5.6 이후부터 InnoDB 테이블에 Online DDL 기능이 추가됨
      ALGORITHM, LOCK 2개 옵션을 추가로 명시하면 테이블 락을 최소화하여 스키마 수정 가능
        ALGORITHM=COPY : 원본은 그대로 두고, 새 테이블을 만들어 이전 후 교체한다. 시스템 자원 소모가 큼
        ALGORITHM=INPLACE : 테이블 복제 과정 없이 메타데이터 변경만으로 반영.
          단, 5.6 이전에 만든 테이블에 대해 실행하면 ERROR 1846이 발생한다. COPY로 리빌드해야 사용 가능해진다
        ALGORITHM=INSTANT : MySQL 8.0.12 (Aurora 3 (2021-11-18))부터 지원하는 모드. 테이블 락 없이 처리된다
          - 컬럼 추가시 명시적으로 위치를 조정할 수 없다. 컬럼 위치가 필요하면 ALGORITHM=INPLACE, LOCK=NONE 필요
          - ROW_FORMAT=COMPRESSED 거나 FULLTEXT 인덱스를 가지면 사용할 수 없다

      ```sql
      ALTER TABLE model
        ADD COLUMN news_column TINYINT(1) NULL DEFAULT 0 COMMENT 'comments...',
        ALGORITHM=INSTANT;
      ```

      github에서 만든 스키마 변경 마이그레이션 툴을 사용해 볼 수도 있다
        - https://github.com/github/gh-ost
    MD

    md :json, 'json column, virtual index', <<~MD
      ```sql
      CREATE TABLE t1 (jdoc JSON);

      # JSON_SET, JSON_OBJECT 등등등..
      ```

      https://vladmihalcea.com/index-json-columns-mysql/

      ```sql
      # JSON 컬럼에 대해 필터를 걸면 테이블 풀스캔이 발생
      SELECT isbn FROM book WHERE properties->>"$.title" = 'Book Title'

      # virtual column을 추가하고 index를 걸면 풀스캔을 방지할 수 있다
      ALTER TABLE book ADD COLUMN title VARCHAR(50) GENERATED ALWAYS AS (properties->>\\"$.title\\")
      CREATE INDEX book_title_idx ON book (title)

      # virtual column을 업데이트할 때는 JSON_SET으로..
      UPDATE book SET properties = JSON_SET(properties, '$.title', 'Book Title 2') WHERE isbn = '123-123123'
      ```
    MD
  end
end
