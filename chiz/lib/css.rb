
module Lib
  class CssChiz < Base
    md :zen, 'how to thinking', <<~MD
      # 모바일 UI 먼저 고려

      데스크탑에 맞는 UI를 축소하는 것보다, 모바일 크기의 UI를 확장하는 편이 더 깔끔하다

      ```scss
        .menu {
          // mobile용 정의

          @media (min-width: 768px) {
            // desktop용 정의
          }
        }
      ```

      # defensive css
      - https://defensivecss.dev/
      - https://ishadeed.com/article/defensive-css/

      되도록 깨진 화면은 보이지 않게 만들기 위한 가이드라인

      - `flex-wrap: wrap` 기본값으로 사용
      - `text-overflow: ellipsis` 사용하기
      - `object-fix: cover`로 이미지 크기를 비율로 조정하기
      - `overscroll-behavior-y: contain` 으로 팝업의 스크롤이 body로 전파되지 않도록 하기
      - `var(--xxx, 10px)` css 변수가 없을 경우 default 값 사용
      - 내부 콘텐츠의 크기가 커질 떄를 대비하여 width, height 대신 min-width, min-height 사용하기
      - `background-repeat: no-repeat` 사용하기
      - vertical media queries를 써서 sticky 메뉴 내용이 겹쳐지지 않도록 만들기
      - space-between 대신 gap 사용하기
      - `scrollbar-gutter: stable`: 스크롤바가 레이아웃에 영향을 끼치지 않도록 하기
      - `min-width: 0`: flex box에서 최소 컨텐츠 조정. default가 auto라 break-word 등이 안먹힌다
        - gridbox도 최소크기가 auto라 같은 문제가 있다. `grid-template-columns: minmax(0, 1fr) 240px`나 `min-width: 0`으로 해결해야한다
    MD

    md :structure_guide, 'ref directory structure', <<~MD
      - https://sass-guidelin.es/

      ```
      application.scss|app.scss|main.scss
      stylesheets/
        abstracts/ | utilities/ | helpers/   # -> 빌드시 직접적인 css가 출력되지 않을 것을 권장
          - _variables.scss
          - _mixins.scss
          - _extends.scss
          - _placeholders.scss
          - _fonts.scss
        base/
          - _base.scss
          - _reset.scss
          - _typography.scss
          - _config.scss
        components/ | modules/
          - _buttons.scss
          - _carousel.scss
          - _cover.scss
          - _dropdown.scss
          - _thumbnails.scss
          - tabs/
              ...
        layout/ | partials/
          - _navigation.scss
          - _grid.scss
          - _header.scss
          - _footer.scss
          - _sidebar.scss
          - _forms.scss
        pages/
          - _home.scss
          - _contact.scss
        themes/
          - _theme.scss
          - _admin.scss
        vendor/
          - _bootstrap.scss
          - _jquery-ui.scss
      ```

      main.scss
      ```scss
      @charset "utf-8";

      @import '...'
      ```
    MD

    md :mixin, 'scss mixin feature', <<~MD, lang: :scss
      // 정의
      @mixin flexContainer($direction, $justify, $align) {
        display: flex;
        flex-direction: $direction;
        justify-content: $justify;
        align-items: $align;
      }

      // 사용
      .main-container {
        @include flexContainer(column, center, center);
      }
    MD

    md :import_scope, 'isolate import css to specific class', <<~MD, lang: :scss
      .bootstrap-styles {
        @import (less) url("bootstrap.css");
      }

      // 단, import는 deprecate고.. `@use as`로 폴어 볼 수 있을듯
    MD

    md :scroll, 'overflow, chaining, card, slide', <<~MD, lang: :css
      // https://www.brunildo.org/test/Overflowxy2.html
      // overflow-x, overflow-y는 서로에게 영향을 끼치는 옵션이다
      // - 한축을 scroll로 설정하면, 다른 축은 visible이 작동하지 않는다


      // 어떤 item의 스크롤이 끝까지 도달하면 상위 요소의 스크롤이 시작된다
      // 이를 스크롤 체이닝이라 부르고, 다음 옵션으로 방지할 수 있다
      //   auto : 기본
      //   contain : 연결되지 않음
      //   none: contain + 스크롤 영역 경계를 넘기는 스크롤 동작도 막음
      //         (터치 인터페이스 등으로 동작하는 새로고침, 뒤로가기 기능이 막힘)
      // 2022-09 기준 77% 지원이라 prod에서 쓰기 애매
      overscroll-behavior: contain;

      // anchor로 page jump가 발생했을 때 스크롤을 부드럽게 이동시킨다
      scroll-behavior: smooth;

      // card, slide, snap
      // type의 1번 인자는 스크롤 스냅이 작동할 방향
      // type의 2번은 동작 조건. mandatory는 항상 동작, proximity는 경계선에 가까우면 snap 동작
      // align은 snap의 기준 위치
      scroll-snap-type: [x|y|block|inline|both] [mandatory|proximity]
      scroll-snap-align: [none|start|end|center]
    MD
  end
end
